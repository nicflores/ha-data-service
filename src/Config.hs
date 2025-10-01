{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

-- import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

data AppConfig = AppConfig
  { s3Bucket :: Text,
    s3Prefix :: Text,
    yahooFinanceBaseUrl :: Text,
    yahooFinanceLookupUrl :: Text,
    dataRange :: Text,
    dataInterval :: Text,
    serverPort :: Int,
    awsRegion :: Text
  }
  deriving (Generic, Show)

instance FromJSON AppConfig

instance ToJSON AppConfig

-- Default configuration
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { s3Bucket = "my-bucket",
      s3Prefix = "my-bucket-key",
      yahooFinanceBaseUrl = "https://query2.finance.yahoo.com/v8/finance/chart",
      yahooFinanceLookupUrl = "https://query1.finance.yahoo.com/v1/finance/lookup",
      dataRange = "1d",
      dataInterval = "1m",
      serverPort = 8080,
      awsRegion = "us-east-1"
    }

-- Load configuration from environment variables, falling back to config file, then defaults
loadConfig :: IO AppConfig
loadConfig = do
  putStrLn "Loading configuration..."

  -- Try to load from file first (for local development)
  fileConfig <- loadConfigFromFile "config.json"

  -- Get the base config (from file or defaults)
  let baseConfig = fromMaybe defaultConfig fileConfig

  -- Override with environment variables if present
  configFromEnv <- loadConfigFromEnv baseConfig

  putStrLn $ "Configuration loaded: " ++ show configFromEnv
  return configFromEnv

-- Load configuration from JSON file (for local development)
loadConfigFromFile :: FilePath -> IO (Maybe AppConfig)
loadConfigFromFile path = do
  result <- eitherDecodeFileStrict path
  case result of
    Left err -> do
      putStrLn $ "No config file found or parse error: " ++ err
      return Nothing
    Right config -> do
      putStrLn $ "Loaded config from " ++ path
      return (Just config)

-- Load configuration from environment variables
loadConfigFromEnv :: AppConfig -> IO AppConfig
loadConfigFromEnv base = do
  s3BucketEnv <- lookupEnv "S3_BUCKET"
  s3PrefixEnv <- lookupEnv "S3_PREFIX"
  yahooBaseUrlEnv <- lookupEnv "YAHOO_FINANCE_BASE_URL"
  yahooLookupUrlEnv <- lookupEnv "YAHOO_FINANCE_LOOKUP_URL"
  rangeEnv <- lookupEnv "DATA_RANGE"
  intervalEnv <- lookupEnv "DATA_INTERVAL"
  portEnv <- lookupEnv "SERVER_PORT"
  regionEnv <- lookupEnv "AWS_REGION"

  return
    AppConfig
      { s3Bucket = maybe (s3Bucket base) T.pack s3BucketEnv,
        s3Prefix = maybe (s3Prefix base) T.pack s3PrefixEnv,
        yahooFinanceBaseUrl = maybe (yahooFinanceBaseUrl base) T.pack yahooBaseUrlEnv,
        yahooFinanceLookupUrl = maybe (yahooFinanceLookupUrl base) T.pack yahooLookupUrlEnv,
        dataRange = maybe (dataRange base) T.pack rangeEnv,
        dataInterval = maybe (dataInterval base) T.pack intervalEnv,
        serverPort = maybe (serverPort base) read portEnv,
        awsRegion = maybe (awsRegion base) T.pack regionEnv
      }