{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 as BS (length, null)
import Data.Text.Encoding (decodeUtf8)
import DataService (copyUrltoS3, productionDeps)
import Network.HTTP.Types.Status (status200, status400)
import Types (AppConfig, Health (Health), errorToResponse)
import Web.Scotty (ScottyM, get, json, pathParam, post, status)

healthCheck :: ScottyM ()
healthCheck =
  get "/health" $ do
    json $ Health "OK"

downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- liftIO $ copyUrltoS3 productionDeps (decodeUtf8 ticker) config
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200