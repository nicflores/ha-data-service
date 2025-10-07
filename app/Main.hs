{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Amazonka (Env, discover, newEnv, runResourceT, send, toBody)
import Amazonka.Data (ToJSON)
import qualified Amazonka.Data as LazyByteString
import Amazonka.S3 (BucketName (BucketName), ObjectKey (ObjectKey), PutObjectResponse, newPutObject)
import Config (AppConfig (..), loadConfig, s3Bucket, s3Prefix, serverPort, yahooFinanceBaseUrl, yahooFinanceLookupUrl)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object), object, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Text as T (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException, JSONException, Request, getResponseBody, getResponseStatusCode, httpJSONEither, httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (Status, status200, status400, status404, status500, status502)
import Web.Scotty (ScottyM, get, json, pathParam, post, scotty, status)

main :: IO ()
main = do
  config <- loadConfig
  let port = serverPort config
  scotty port $ do
    healthEndpoint
    downloadTickerData config

-- Handlers
healthEndpoint :: ScottyM ()
healthEndpoint =
  get "/health" $ do
    json $ object ["status" .= ("healthy" :: Text)]

downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
      json $ object ["error" .= ("Invalid ticker format" :: Text)]
    else do
      result <- liftIO $ copyUrltoS3 productionDeps (decodeUtf8 ticker) config

      case result of
        Left err -> do
          let (httpStatus, message) = errorToResponse err
          status httpStatus
          json $ object ["error" .= T.pack message]
        Right _ -> do
          status status200
          json $
            object
              [ "status" .= ("success" :: Text),
                "message" .= ("Data uploaded to S3" :: Text),
                "ticker" .= decodeUtf8 ticker
              ]

-- Data types
data ApiResponse
  = Success {status_code :: Int}
  | Error {error_message :: String}
  deriving (Generic, Show)

instance ToJSON ApiResponse

type Ticker = Text

data CopyToS3Error
  = TickerError TickerLookupError
  | TickerNotFound Ticker
  | NetworkError HttpException
  | S3UploadError HttpException
  | ApiError Int String
  deriving (Show)

data FetchConfig = FetchConfig
  { baseUrl :: Text,
    range :: Text,
    interval :: Text
  }
  deriving (Show, Eq)

-- Convert errors to HTTP responses
errorToResponse :: CopyToS3Error -> (Status, String)
errorToResponse (TickerError (HttpError _)) = (status502, "Failed to verify ticker")
errorToResponse (TickerError (JsonParseError _)) = (status502, "Invalid response from ticker lookup")
errorToResponse (TickerError (InvalidResponse msg)) = (status502, msg)
errorToResponse (TickerNotFound ticker) = (status404, "Ticker not found: " ++ T.unpack ticker)
errorToResponse (NetworkError _) = (status502, "Network error fetching data")
errorToResponse (S3UploadError _) = (status500, "Failed to upload to S3")
errorToResponse (ApiError code msg) = (toEnum code, msg)

-- Pure function: Build the request (easily testable)
buildYahooRequest :: FetchConfig -> Ticker -> IO Request
buildYahooRequest config ticker = do
  let url = T.unpack $ baseUrl config <> "/" <> ticker
  let queryParams =
        [ ("range", Just $ BS.pack $ T.unpack $ range config),
          ("interval", Just $ BS.pack $ T.unpack $ interval config),
          ("includePrePost", Just "true"),
          ("events", Just "div,split")
        ]
  let headers = [("User-Agent", "Mozilla/5.0"), ("Accept-Encoding", "gzip, deflate")]
  setRequestHeaders headers . setRequestQueryString queryParams <$> parseRequest url

-- Separate concerns: HTTP fetching
fetchYahooData :: Request -> IO (Either CopyToS3Error Data.ByteString.Lazy.LazyByteString)
fetchYahooData request = do
  result <- try $ httpLbs request
  case result of
    Left (httpEx :: HttpException) -> return $ Left (NetworkError httpEx)
    Right response ->
      if getResponseStatusCode response == 200
        then return $ Right (getResponseBody response)
        else
          return $
            Left $
              ApiError
                (getResponseStatusCode response)
                ("Yahoo Finance API error: " ++ show (getResponseStatusCode response))

-- Pure function: Generate S3 object key (easily testable)
generateS3Key :: S3Config -> Ticker -> LazyByteString.UTCTime -> ObjectKey
generateS3Key config ticker currentTime =
  let datePath = formatTime defaultTimeLocale "%Y/%m/%d" currentTime
      timeStamp = formatTime defaultTimeLocale "%H%M%S" currentTime
      keyText =
        prefix config
          <> "/"
          <> T.pack datePath
          <> "/"
          <> ticker
          <> "_"
          <> T.pack timeStamp
          <> ".json"
   in ObjectKey keyText

-- Separate concerns: S3 uploading
uploadToS3 :: Env -> BucketName -> ObjectKey -> Data.ByteString.Lazy.LazyByteString -> IO (Either CopyToS3Error PutObjectResponse)
uploadToS3 env bucketName objectKey body = do
  let putReq = newPutObject bucketName objectKey (toBody body)
  uploadResult <- try $ runResourceT $ send env putReq
  case uploadResult of
    Left (s3Ex :: HttpException) -> return $ Left (S3UploadError s3Ex)
    Right putRes -> return $ Right putRes

data Dependencies = Dependencies
  { depFetchData :: Request -> IO (Either CopyToS3Error Data.ByteString.Lazy.LazyByteString),
    depUploadToS3 :: Env -> BucketName -> ObjectKey -> Data.ByteString.Lazy.LazyByteString -> IO (Either CopyToS3Error PutObjectResponse),
    depGetCurrentTime :: IO LazyByteString.UTCTime,
    depNewEnv :: IO Env
  }

-- Production dependencies
productionDeps :: Dependencies
productionDeps =
  Dependencies
    { depFetchData = fetchYahooData,
      depUploadToS3 = uploadToS3,
      depGetCurrentTime = getCurrentTime,
      depNewEnv = newEnv discover
    }

data S3Config = S3Config
  { bucket :: Text,
    prefix :: Text
  }
  deriving (Show, Eq)

-- Fully testable version with dependency injection
copyUrltoS3 :: Dependencies -> Ticker -> AppConfig -> IO (Either CopyToS3Error PutObjectResponse)
copyUrltoS3 deps ticker config = do
  tickerCheck <- tickerLookup ticker config
  case tickerCheck of
    Left lookupErr -> return $ Left (TickerError lookupErr)
    Right False -> return $ Left (TickerNotFound ticker)
    Right True -> do
      let fetchConfig =
            FetchConfig
              { baseUrl = yahooFinanceBaseUrl config,
                range = dataRange config,
                interval = dataInterval config
              }

      request <- buildYahooRequest fetchConfig ticker
      fetchResult <- depFetchData deps request
      case fetchResult of
        Left err -> return $ Left err
        Right body -> do
          env <- depNewEnv deps
          currentTime <- depGetCurrentTime deps
          let s3Config =
                S3Config
                  { bucket = s3Bucket config,
                    prefix = s3Prefix config
                  }
          let objectKey = generateS3Key s3Config ticker currentTime
          let bucketName = BucketName (bucket s3Config)
          depUploadToS3 deps env bucketName objectKey body

-- Ticker lookup
-- rewrite using eitherT
tickerLookup :: Ticker -> AppConfig -> IO (Either TickerLookupError Bool)
tickerLookup ticker config = do
  result <- try $ do
    let queryUrl = yahooFinanceLookupUrl config
    request <- parseRequest $ T.unpack queryUrl
    let requestWithHeaders =
          setRequestHeaders [("User-Agent", "Mozilla/5.0")] $
            setRequestQueryString
              [("query", Just $ BS.pack $ T.unpack ticker)]
              request
    httpJSONEither requestWithHeaders

  case result of
    Left httpEx -> return $ Left (HttpError httpEx)
    Right response ->
      case getResponseBody response of
        Left jsonEx -> return $ Left (JsonParseError jsonEx)
        Right (value :: Value) ->
          case extractTotal value of
            Nothing -> return $ Left (InvalidResponse "Could not parse total field")
            Just total -> return $ Right (total > 0)

data TickerLookupError
  = HttpError HttpException
  | JsonParseError JSONException
  | InvalidResponse String
  deriving (Show)

extractTotal :: Value -> Maybe Int
extractTotal (Object obj) = parseMaybe parser obj
  where
    parser v = do
      finance <- v .: "finance"
      results <- finance .: "result"
      case results of
        [] -> return 0
        (firstResult : _) -> firstResult .: "total"
extractTotal _ = Nothing

-- TODO
-- x Add health endpoint
-- x pass in the ticker as an agrument
-- x check if the ticker exists before making a request for the data

-- return proper https resonse errors
--   x a. if ticker doesn't exist
--   x b. if we get an error back from yahoo finance
--   x c. if we get an error from AWS

-- x Get hardcoded values from ENV Vars (write code for a Config)
-- x Error handling of config (ie. if env vars are wrong or don't exist)
-- Handle error if no AWS credentials exist

-- Terraform
-- x a. write terraform
--   b. deploy terraform (probably won't do this through github actions)

-- Github workflow to
-- x a. build docker image
-- b. push docker image
-- c. deploy docker image