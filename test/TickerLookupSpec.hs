{-# LANGUAGE OverloadedStrings #-}

module TickerLookupSpec where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Test.Hspec
import TickerLookup

spec :: Spec
spec = do
  describe "extractTotal" $ do
    it "extracts total from valid response" $ do
      let json =
            object
              [ "finance"
                  .= object
                    [ "result"
                        .= [ object ["total" .= (5 :: Int)]
                           ]
                    ]
              ]
      extractTotal json `shouldBe` Just 5

    it "returns 0 for empty results" $ do
      let json =
            object
              [ "finance"
                  .= object
                    [ "result" .= ([] :: [Value])
                    ]
              ]
      extractTotal json `shouldBe` Just 0

    it "returns Nothing for invalid structure" $ do
      let json = object ["wrong" .= ("field" :: String)]
      extractTotal json `shouldBe` Nothing

    it "returns Nothing for non-object" $ do
      let json = Aeson.String "not an object"
      extractTotal json `shouldBe` Nothing

    it "handles multiple results, uses first" $ do
      let json =
            object
              [ "finance"
                  .= object
                    [ "result"
                        .= [ object ["total" .= (3 :: Int)],
                             object ["total" .= (7 :: Int)]
                           ]
                    ]
              ]
      extractTotal json `shouldBe` Just 3