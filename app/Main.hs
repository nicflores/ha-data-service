module Main where

import Config (loadConfig)
import Handlers (downloadTickerData, healthEndpoint)
import Types (AppConfig (serverPort))
import Web.Scotty (scotty)

main :: IO ()
main = do
  config <- loadConfig
  let port = serverPort config
  scotty port $ do
    healthEndpoint
    downloadTickerData config

-- TODO
-- x Add health endpoint
-- x pass in the ticker as an agrument
-- x check if the ticker exists before making a request for the data

-- return proper https resonse errors
--   x a. if ticker doesn't exist
--   x b. if we get an error back from yahoo finance
--   x c. if we get an error from AWS

-- x Get hardcoded values from ENV Vars (write code for a Config)
-- x Error handling of config (ie. if env vars are wrong or don't exist)
-- Handle error if no AWS credentials exist

-- Terraform
-- x a. write terraform
--   b. deploy terraform (probably won't do this through github actions)

-- Github workflow to
-- x a. build docker image
-- b. push docker image
-- c. deploy docker image