{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.ByteString.Char8 as BS
import Data.Text as T (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import DataService (copyUrltoS3, productionDeps)
import Network.HTTP.Types.Status (status200, status400)
import Types (AppConfig, errorToResponse)
import Web.Scotty (ScottyM, get, json, pathParam, post, status)

-- Handlers
healthEndpoint :: ScottyM ()
healthEndpoint =
  get "/health" $ do
    json $ object ["status" .= ("healthy" :: Text)]

downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
      json $ object ["error" .= ("Invalid ticker format" :: Text)]
    else do
      result <- liftIO $ copyUrltoS3 productionDeps (decodeUtf8 ticker) config
      case result of
        Left err -> do
          let (httpStatus, message) = errorToResponse err
          status httpStatus
          json $ object ["error" .= T.pack message]
        Right _ -> do
          status status200
          json $
            object
              [ "status" .= ("success" :: Text),
                "message" .= ("Data uploaded to S3" :: Text),
                "ticker" .= decodeUtf8 ticker
              ]