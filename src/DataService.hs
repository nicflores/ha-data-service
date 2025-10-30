{-# LANGUAGE OverloadedStrings #-}

module DataService where

import qualified Aws
import Aws.S3 (PutObject)
import qualified Aws.S3 as S3
import Control.Exception (try)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 as BS (pack)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text as T (pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.Time as LazyByteString
import Network.HTTP.Client.Conduit (Manager, RequestBody (RequestBodyLBS))
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatusCode, httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)
import TickerLookup (tickerLookup)
import Types (AppConfig (dataInterval, dataRange, s3Bucket, s3Prefix, yahooFinanceBaseUrl), BucketName, CopyToS3Error (ApiError, NetworkError, S3UploadError, TickerError, TickerNotFound), Dependencies (Dependencies, depFetchData, depGetAwsPutObj, depGetCurrentTime, depNewCfg, depNewMgr, depSendPutObj), FetchConfig (FetchConfig, baseUrl, interval, range), ObjectKey (ObjectKey), S3Config (S3Config, bucket, prefix), Ticker)

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

getAwsPutObj :: BucketName -> ObjectKey -> LazyByteString -> PutObject
getAwsPutObj bucketName (ObjectKey objectKey) lbs = S3.putObject bucketName objectKey $ RequestBodyLBS lbs

sendPutObj :: Aws.Configuration -> Manager -> PutObject -> IO (Either CopyToS3Error S3.PutObjectResponse)
sendPutObj cfg mgr putObj = do
  result <- try $ runResourceT $ Aws.pureAws cfg Aws.defServiceConfig mgr putObj
  case result of
    Left err -> return $ Left (S3UploadError err)
    Right resp -> return $ Right resp

productionDeps :: Dependencies
productionDeps =
  Dependencies
    { depFetchData = fetchYahooData,
      depGetAwsPutObj = getAwsPutObj,
      depSendPutObj = sendPutObj,
      depGetCurrentTime = getCurrentTime,
      depNewMgr = newManager tlsManagerSettings,
      depNewCfg = Aws.baseConfiguration
    }

copyUrltoS3 :: Dependencies -> Ticker -> AppConfig -> IO (Either CopyToS3Error S3.PutObjectResponse)
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
          cfg <- depNewCfg deps
          mgr <- depNewMgr deps
          currentTime <- depGetCurrentTime deps
          let s3Config =
                S3Config
                  { bucket = s3Bucket config,
                    prefix = s3Prefix config
                  }
          let objectKey = generateS3Key s3Config ticker currentTime
          let bucketName = bucket s3Config
          let putObj = depGetAwsPutObj deps bucketName objectKey body
          depSendPutObj deps cfg mgr putObj