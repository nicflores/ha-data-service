{-# LANGUAGE OverloadedStrings #-}

module TickerLookup where

import Control.Exception (try)
import Data.Aeson (Value (Object), (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.HTTP.Simple (getResponseBody, httpJSONEither, parseRequest, setRequestHeaders, setRequestQueryString)
import Types (AppConfig (yahooFinanceLookupUrl), Ticker, TickerLookupError (HttpError, InvalidResponse, JsonParseError))

-- Ticker lookup: rewrite using eitherT
tickerLookup :: Ticker -> AppConfig -> IO (Either TickerLookupError Bool)
tickerLookup ticker config = do
  result <- try $ do
    let queryUrl = T.unpack $ yahooFinanceLookupUrl config
    let headers = [("User-Agent", "Mozilla/5.0")]
    let params = [("query", Just $ BS.pack $ T.unpack ticker)]
    request <- setRequestHeaders headers . setRequestQueryString params <$> parseRequest queryUrl
    httpJSONEither request
  case result of
    Left httpEx -> return $ Left (HttpError httpEx)
    Right response ->
      case getResponseBody response of
        Left jsonEx -> return $ Left (JsonParseError jsonEx)
        Right value ->
          case extractTotal value of
            Nothing -> return $ Left (InvalidResponse "Could not parse total field")
            Just total -> return $ Right (total > 0)

extractTotal :: Value -> Maybe Int
extractTotal (Object obj) = parseMaybe parser obj
  where
    parser v = do
      finance <- v .: "finance"
      results <- finance .: "result"
      case results of
        [] -> return 0
        (firstResult : _) -> firstResult .: "total"
extractTotal _ = Nothing