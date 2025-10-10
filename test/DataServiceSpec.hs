{-# LANGUAGE OverloadedStrings #-}

module DataServiceSpec where

import Amazonka.S3 (ObjectKey (..))
import qualified Data.Text as T
import Data.Time
import DataService (buildYahooRequest, generateS3Key)
import Test.Hspec
import Types (FetchConfig (FetchConfig), S3Config (..))

spec :: Spec
spec = do
  describe "generateS3Key" $ do
    it "generates correct S3 key format" $ do
      let config = S3Config "my-bucket" "data"
      let ticker = "AAPL"
      let time = UTCTime (fromGregorian 2025 1 15) 0
      let ObjectKey key = generateS3Key config ticker time

      key `shouldSatisfy` T.isPrefixOf "data/2025/01/15/"
      key `shouldSatisfy` T.isInfixOf "AAPL"
      key `shouldSatisfy` T.isSuffixOf ".json"

    it "includes ticker symbol in key" $ do
      let config = S3Config "bucket" "prefix"
      let ticker = "TSLA"
      let time = UTCTime (fromGregorian 2025 1 15) 0
      let ObjectKey key = generateS3Key config ticker time

      key `shouldSatisfy` T.isInfixOf "TSLA"

    it "uses correct date format" $ do
      let config = S3Config "bucket" "prefix"
      let ticker = "MSFT"
      let time = UTCTime (fromGregorian 2025 10 9) 0
      let ObjectKey key = generateS3Key config ticker time

      key `shouldSatisfy` T.isInfixOf "2025/10/09"

    it "includes prefix in path" $ do
      let config = S3Config "bucket" "financial-data"
      let ticker = "GOOGL"
      let time = UTCTime (fromGregorian 2025 1 15) 0
      let ObjectKey key = generateS3Key config ticker time

      key `shouldSatisfy` T.isPrefixOf "financial-data/"

  describe "buildYahooRequest" $ do
    it "builds request with correct base URL" $ do
      let config = FetchConfig "https://api.example.com" "1d" "1m"
      request <- buildYahooRequest config "AAPL"

      -- Basic check that request was created
      show request `shouldContain` "api.example.com"

    it "includes ticker in URL" $ do
      let config = FetchConfig "https://api.example.com" "1d" "1m"
      request <- buildYahooRequest config "TSLA"

      show request `shouldContain` "TSLA"

  describe "FetchConfig" $ do
    it "equality works correctly" $ do
      let config1 = FetchConfig "url" "1d" "1m"
      let config2 = FetchConfig "url" "1d" "1m"
      let config3 = FetchConfig "url" "5d" "1m"

      config1 `shouldBe` config2
      config1 `shouldNotBe` config3

  describe "S3Config" $ do
    it "equality works correctly" $ do
      let config1 = S3Config "bucket" "prefix"
      let config2 = S3Config "bucket" "prefix"
      let config3 = S3Config "other-bucket" "prefix"

      config1 `shouldBe` config2
      config1 `shouldNotBe` config3