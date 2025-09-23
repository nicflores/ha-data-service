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
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException, getResponseBody, getResponseStatusCode, httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)
import Web.Scotty (ScottyM, get, json, scotty, text)

main :: IO ()
main = scotty 8080 $ do
  simpleGet
  testAwsAuth
  simpleClient
  getData

-- Handlers
simpleGet :: ScottyM ()
simpleGet = get "/" $ text "Yay!"

testAwsAuth :: ScottyM ()
testAwsAuth = get "/aws" $ do
  result <- liftIO checkAwsAuth
  case result of
    Left err -> json ([("satus", "error"), ("message", err)] :: [(Text, String)])
    Right authInfo -> json authInfo

simpleClient :: ScottyM ()
simpleClient = get "/client" $ do
  result <- liftIO simpleHttp
  case result of
    Left err -> json $ Error (show err)
    Right code -> json $ Success code

getData :: ScottyM ()
getData = get "/download" $ do
  result <- liftIO copyUrltoS3
  case result of
    Left err -> json $ Error (show err)
    Right _ -> json $ Success 200

-- Downloader Helper
copyUrltoS3 :: IO (Either String ())
copyUrltoS3 = do
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
        let putReq =
              newPutObject "nf-json-data" (ObjectKey objectKey) $
                toBody bodySource
        runResourceT $ do
          _resp <- send env putReq
          return ()
      else error $ "HTTP error: " ++ show (getResponseStatusCode response)

  case result of
    Left (ex :: HttpException) -> return $ Left (show ex)
    Right _ -> return $ Right ()

-- HTTP Helpers
simpleHttp :: IO (Either HttpException Int)
simpleHttp = do
  try $ do
    request <- parseRequest "https://google.com"
    response <- httpLbs request
    return $ getResponseStatusCode response

data ApiResponse
  = Success {status_code :: Int}
  | Error {error_message :: String}
  deriving (Generic, Show)

instance ToJSON ApiResponse

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