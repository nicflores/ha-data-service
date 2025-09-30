{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Amazonka (discover, newEnv, runResourceT, send, toBody)
import Amazonka.Data (ToJSON)
import Amazonka.S3 (ObjectKey (ObjectKey), newPutObject)
import Amazonka.STS (newGetCallerIdentity)
import Amazonka.STS.GetCallerIdentity (getCallerIdentityResponse_account, getCallerIdentityResponse_arn, getCallerIdentityResponse_userId)
import Control.Exception (SomeException, try)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object), object, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException, JSONException, getResponseBody, getResponseStatusCode, httpJSONEither, httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (Status, status404, status500, status502)
import Web.Scotty (ScottyM, get, json, pathParam, scotty)

main :: IO ()
main = scotty 8080 $ do
  healthEndpoint
  getData

-- Handlers
healthEndpoint :: ScottyM ()
healthEndpoint =
  get "/health" $ do
    json $ object ["status" .= ("healthy" :: Text)]

getData :: ScottyM ()
getData = get "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  result <- liftIO $ copyUrltoS3 ticker
  case result of
    Left err -> json $ Error (show err)
    Right _ -> json $ Success 200

data ApiResponse
  = Success {status_code :: Int}
  | Error {error_message :: String}
  deriving (Generic, Show)

instance ToJSON ApiResponse

-- Downloader Helper
copyUrltoS3 :: Ticker -> IO (Either CopyToS3Error ())
copyUrltoS3 ticker = do
  tickerCheck <- tickerLookup ticker
  case tickerCheck of
    Left lookupErr -> return $ Left (TickerError lookupErr)
    Right False -> return $ Left (TickerNotFound ticker)
    Right True -> do
      env <- newEnv discover
      result <- try $ do
        let baseUrl = "https://query2.finance.yahoo.com/v8/finance/chart/AAPL"
        let queryParams = [("range", Just "1d"), ("interval", Just "1m"), ("includePrePost", Just "true"), ("events", Just "div,split")]
        let headersParams = [("User-Agent", "Mozilla/5.0"), ("Accept-Encoding", "gzip, deflate")]
        request <- parseRequest baseUrl
        let requestWithOptions = setRequestHeaders headersParams $ setRequestQueryString queryParams request
        response <- httpLbs requestWithOptions

        if getResponseStatusCode response == 200
          then do
            currentTime <- getCurrentTime
            let datePath = formatTime defaultTimeLocale "%Y/%m/%d" currentTime
            let timeStamp = formatTime defaultTimeLocale "%H%M%S" currentTime
            let objectKey = pack $ "financial-data/" ++ datePath ++ "/data_" ++ timeStamp ++ ".json"
            let bodySource = getResponseBody response
            let putReq = newPutObject "nf-json-data" (ObjectKey objectKey) $ toBody bodySource

            uploadResult <- try $ runResourceT $ do
              _resp <- send env putReq
              return ()

            case uploadResult of
              Left (s3Ex :: HttpException) -> return $ Left (S3UploadError s3Ex)
              Right _ -> return $ Right ()
          else
            return $
              Left
                ( ApiError
                    (getResponseStatusCode response)
                    ("Yahoo Finance API error: " ++ show (getResponseStatusCode response))
                )

      case result of
        Left (httpEx :: HttpException) -> return $ Left (NetworkError httpEx)
        Right res -> return res

type Ticker = Text

data CopyToS3Error
  = TickerError TickerLookupError
  | TickerNotFound Ticker
  | NetworkError HttpException
  | S3UploadError HttpException
  | ApiError Int String
  deriving (Show)

errorToResponse :: CopyToS3Error -> (Status, String)
errorToResponse (TickerError (HttpError _)) = (status502, "Failed to verify ticker")
errorToResponse (TickerError (JsonParseError _)) = (status502, "Invalid response from ticker lookup")
errorToResponse (TickerError (InvalidResponse msg)) = (status502, msg)
errorToResponse (TickerNotFound ticker) = (status404, "Ticker not found: " <> unpack ticker)
errorToResponse (NetworkError _) = (status502, "Network error fetching data")
errorToResponse (S3UploadError _) = (status500, "Failed to upload to S3")
errorToResponse (ApiError code msg) = (toEnum code, msg)

-- Ticker lookup
tickerLookup :: Ticker -> IO (Either TickerLookupError Bool)
tickerLookup ticker = do
  result <- try $ do
    request <- parseRequest $ unpack $ "https://query1.finance.yahoo.com/v1/finance/lookup?query=" <> ticker
    let requestWithHeaders = setRequestHeaders [("User-Agent", "Mozilla/5.0")] request
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

-- AWS Helpers
checkAwsAuth :: IO (Either String AuthInfo)
checkAwsAuth = do
  result <- try $ do
    env <- newEnv discover
    runResourceT $ do
      response <- send env newGetCallerIdentity
      return $
        AuthInfo
          { account = response ^. getCallerIdentityResponse_account,
            arn = response ^. getCallerIdentityResponse_arn,
            userId = response ^. getCallerIdentityResponse_userId
          }
  case result of
    Left (ex :: SomeException) -> return $ Left (show ex)
    Right authInfo -> return $ Right authInfo

data AuthInfo = AuthInfo
  { account :: Maybe Text,
    arn :: Maybe Text,
    userId :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON AuthInfo

-- TODO
-- x Add health endpoint
-- x pass in the ticker as an agrument
-- x check if the ticker exists before making a request for the data

-- return proper https resonse errors
--   x a. if ticker doesn't exist
--   x b. if we get an error back from yahoo finance
--   x c. if we get an error from AWS

-- Get hardcoded values from ENV Vars (write code for a Config)
-- Error handling of config (ie. if env vars are wrong or don't exist)
-- Handle error if no AWS credentials exist

-- Terraform
-- x a. write terraform
--   b. deploy terraform (probably won't do this through github actions)

-- Github workflow to
-- x a. build docker image
-- b. push docker image
-- c. deploy docker image