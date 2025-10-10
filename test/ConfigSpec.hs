{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec where

import Config (loadConfigFromFile)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Types (AppConfig (AppConfig, awsRegion, dataInterval, dataRange, s3Bucket, s3Prefix, serverPort, yahooFinanceBaseUrl, yahooFinanceLookupUrl))

spec :: Spec
spec = do
  describe "loadConfigFromFile" $ do
    it "returns Nothing when file doesn't exist" $ do
      result <- loadConfigFromFile "nonexistent.json"
      result `shouldBe` Nothing

    it "loads valid config from file" $ do
      withSystemTempDirectory "config-test" $ \tmpDir -> do
        let configPath = tmpDir </> "test-config.json"
        let testConfig =
              AppConfig
                { s3Bucket = "test-bucket",
                  s3Prefix = "test-prefix",
                  yahooFinanceBaseUrl = "http://test.com",
                  yahooFinanceLookupUrl = "http://lookup.com",
                  dataRange = "1d",
                  dataInterval = "1m",
                  serverPort = 3000,
                  awsRegion = "us-west-2"
                }
        LBS.writeFile configPath (encode testConfig)

        result <- loadConfigFromFile configPath
        case result of
          Nothing -> expectationFailure "Expected config to load"
          Just config -> do
            s3Bucket config `shouldBe` "test-bucket"
            serverPort config `shouldBe` 3000

  describe "AppConfig" $ do
    it "has all required fields" $ do
      let config =
            AppConfig
              { s3Bucket = "bucket",
                s3Prefix = "prefix",
                yahooFinanceBaseUrl = "url1",
                yahooFinanceLookupUrl = "url2",
                dataRange = "1d",
                dataInterval = "1m",
                serverPort = 8080,
                awsRegion = "us-east-1"
              }
      s3Bucket config `shouldBe` "bucket"
      serverPort config `shouldBe` 8080