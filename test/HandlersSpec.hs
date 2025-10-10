-- TODO
-- look into hspec-wai
{-# LANGUAGE OverloadedStrings #-}

module HandlersSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "healthEndpoint" $ do
    it "is defined" $ do
      True `shouldBe` True

  describe "downloadTickerData" $ do
    it "is defined" $ do
      True `shouldBe` True