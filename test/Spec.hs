module Main (main) where

import qualified ConfigSpec
import qualified DataServiceSpec
import qualified HandlersSpec
import Test.Hspec
import qualified TickerLookupSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
  TypesSpec.spec
  ConfigSpec.spec
  TickerLookupSpec.spec
  DataServiceSpec.spec
  HandlersSpec.spec