-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (loadConfig)
import Handlers (downloadTickerData, healthCheck)
import Types (AppConfig (serverPort))
import Web.Scotty (scotty)

main :: IO ()
main = do
  config <- loadConfig
  let port = serverPort config
  scotty port $ do
    healthCheck
    downloadTickerData config