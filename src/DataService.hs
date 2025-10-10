{-# LANGUAGE OverloadedStrings #-}

module DataService where

import Amazonka (Env, discover, newEnv, runResourceT, send, toBody)
import qualified Amazonka.Data as LazyByteString
import Amazonka.S3 (BucketName (BucketName), ObjectKey (ObjectKey), PutObjectResponse, newPutObject)
import Control.Exception (try)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Text as T (pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatusCode, httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)
import TickerLookup (tickerLookup)
import Types (AppConfig (dataInterval, dataRange, s3Bucket, s3Prefix, yahooFinanceBaseUrl), CopyToS3Error (ApiError, NetworkError, S3UploadError, TickerError, TickerNotFound), Dependencies (Dependencies, depFetchData, depGetCurrentTime, depNewEnv, depUploadToS3), FetchConfig (FetchConfig, baseUrl, interval, range), S3Config (S3Config, bucket, prefix), Ticker)

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

fetchYahooData :: Request -> IO (Either CopyToS3Error LazyByteString)
fetchYahooData request = do
  result <- try $ httpLbs request
  case result of
    Left httpEx -> return $ Left (NetworkError httpEx)
    Right response ->
      if getResponseStatusCode response == 200
        then return $ Right (getResponseBody response)
        else
          return $
            Left $
              ApiError
                (getResponseStatusCode response)
                ("Yahoo Finance API error: " ++ show (getResponseStatusCode response))

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

uploadToS3 :: Env -> BucketName -> ObjectKey -> LazyByteString -> IO (Either CopyToS3Error PutObjectResponse)
uploadToS3 env bucketName objectKey body = do
  let putReq = newPutObject bucketName objectKey (toBody body)
  uploadResult <- try $ runResourceT $ send env putReq
  case uploadResult of
    Left s3Ex -> return $ Left (S3UploadError s3Ex)
    Right putRes -> return $ Right putRes

productionDeps :: Dependencies
productionDeps =
  Dependencies
    { depFetchData = fetchYahooData,
      depUploadToS3 = uploadToS3,
      depGetCurrentTime = getCurrentTime,
      depNewEnv = newEnv discover
    }

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