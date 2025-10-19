{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Network.HTTP.Types (status404, status500, status502)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Types
  ( CopyToS3Error
      ( ApiError,
        NetworkError,
        S3UploadError,
        TickerNotFound
      ),
    FetchConfig (FetchConfig, baseUrl, interval, range),
    S3Config (S3Config, bucket, prefix),
    errorToResponse,
  )

spec :: Spec
spec = do
  describe "errorToResponse" $ do
    it "converts TickerNotFound to 404" $ do
      let err = TickerNotFound "AAPL"
      let (status, msg) = errorToResponse err
      status `shouldBe` status404
      msg `shouldContain` "AAPL"

    it "converts NetworkError to 502" $ do
      let err = NetworkError undefined -- We don't need actual exception
      let (status, _) = errorToResponse err
      status `shouldBe` status502

    it "converts S3UploadError to 500" $ do
      let err = S3UploadError undefined
      let (status, _) = errorToResponse err
      status `shouldBe` status500

    it "converts ApiError with correct status code" $ do
      let err = ApiError 503 "Service unavailable"
      let (status, msg) = errorToResponse err
      status `shouldBe` toEnum 503
      msg `shouldBe` "Service unavailable"

  describe "FetchConfig" $ do
    it "can be created with all fields" $ do
      let config = FetchConfig "http://example.com" "1d" "1m"
      baseUrl config `shouldBe` "http://example.com"
      range config `shouldBe` "1d"
      interval config `shouldBe` "1m"

  describe "S3Config" $ do
    it "can be created with bucket and prefix" $ do
      let config = S3Config "my-bucket" "my-prefix"
      bucket config `shouldBe` "my-bucket"
      prefix config `shouldBe` "my-prefix"