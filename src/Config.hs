{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative ((<|>))
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Types (AppConfig (..))

-- Load configuration: try file first, then env vars, or error out
loadConfig :: IO AppConfig
loadConfig = do
  putStrLn "Loading configuration..."
  hFlush stdout

  fileConfig <- loadConfigFromFile "config.json"
  envConfig <- loadConfigFromEnv

  case fileConfig <|> envConfig of
    Nothing -> do
      hPutStrLn stderr "ERROR: No configuration found. Please provide config.json or set environment variables."
      hFlush stderr
      exitFailure
    Just config -> do
      return config

loadConfigFromFile :: FilePath -> IO (Maybe AppConfig)
loadConfigFromFile path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> do
          putStrLn $ "Config file parse error: " ++ err
          hFlush stderr
          return Nothing
        Right config -> do
          putStrLn $ "Loaded config from " ++ path
          hFlush stdout
          return (Just config)
    else return Nothing

-- Load configuration from environment variables
loadConfigFromEnv :: IO (Maybe AppConfig)
loadConfigFromEnv = do
  s3BucketEnv <- lookupEnv "S3_BUCKET"
  s3PrefixEnv <- lookupEnv "S3_PREFIX"
  yahooBaseUrlEnv <- lookupEnv "YAHOO_FINANCE_BASE_URL"
  yahooLookupUrlEnv <- lookupEnv "YAHOO_FINANCE_LOOKUP_URL"
  rangeEnv <- lookupEnv "DATA_RANGE"
  intervalEnv <- lookupEnv "DATA_INTERVAL"
  portEnv <- lookupEnv "SERVER_PORT"
  regionEnv <- lookupEnv "AWS_REGION"
  case (s3BucketEnv, s3PrefixEnv, yahooBaseUrlEnv, yahooLookupUrlEnv) of
    (Just bucketEnv, Just prefixEnv, Just baseUrlEnv, Just lookupUrlEnv) -> do
      putStrLn "Loaded config from environment variables"
      hFlush stdout
      return $
        Just
          AppConfig
            { s3Bucket = T.pack bucketEnv,
              s3Prefix = T.pack prefixEnv,
              yahooFinanceBaseUrl = T.pack baseUrlEnv,
              yahooFinanceLookupUrl = T.pack lookupUrlEnv,
              dataRange = maybe "1d" T.pack rangeEnv,
              dataInterval = maybe "1m" T.pack intervalEnv,
              serverPort = maybe 8080 read portEnv,
              awsRegion = maybe "us-east-1" T.pack regionEnv
            }
    _ -> do
      return Nothing