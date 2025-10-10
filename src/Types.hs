{-# LANGUAGE DeriveGeneric #-}

module Types (Ticker, AppConfig (..), CopyToS3Error (..), Dependencies (..), S3Config (..), FetchConfig (..), TickerLookupError (..), errorToResponse) where

import Amazonka (Env)
import qualified Amazonka as Data.ByteString.Lazy.LazyByteString
import Amazonka.S3 (BucketName, ObjectKey, PutObjectResponse)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text as T (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException, JSONException, Request)
import Network.HTTP.Types (Status, status404, status500, status502)

type Ticker = Text

data AppConfig = AppConfig
  { s3Bucket :: Text,
    s3Prefix :: Text,
    yahooFinanceBaseUrl :: Text,
    yahooFinanceLookupUrl :: Text,
    dataRange :: Text,
    dataInterval :: Text,
    serverPort :: Int,
    awsRegion :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON AppConfig

instance ToJSON AppConfig

-- Data types
data ApiResponse
  = Success {status_code :: Int}
  | Error {error_message :: String}
  deriving (Generic, Show)

instance ToJSON ApiResponse

data CopyToS3Error
  = TickerError TickerLookupError
  | TickerNotFound Ticker
  | NetworkError HttpException
  | S3UploadError HttpException
  | ApiError Int String
  deriving (Show)

data FetchConfig = FetchConfig
  { baseUrl :: Text,
    range :: Text,
    interval :: Text
  }
  deriving (Show, Eq)

data TickerLookupError
  = HttpError HttpException
  | JsonParseError JSONException
  | InvalidResponse String
  deriving (Show)

data S3Config = S3Config
  { bucket :: Text,
    prefix :: Text
  }
  deriving (Show, Eq)

data Dependencies = Dependencies
  { depFetchData :: Request -> IO (Either CopyToS3Error Data.ByteString.Lazy.LazyByteString),
    depUploadToS3 :: Env -> BucketName -> ObjectKey -> Data.ByteString.Lazy.LazyByteString -> IO (Either CopyToS3Error PutObjectResponse),
    depGetCurrentTime :: IO Data.ByteString.Lazy.LazyByteString.UTCTime,
    depNewEnv :: IO Env
  }

-- Convert errors to HTTP responses
errorToResponse :: CopyToS3Error -> (Status, String)
errorToResponse (TickerError (HttpError _)) = (status502, "Failed to verify ticker")
errorToResponse (TickerError (JsonParseError _)) = (status502, "Invalid response from ticker lookup")
errorToResponse (TickerError (InvalidResponse msg)) = (status502, msg)
errorToResponse (TickerNotFound ticker) = (status404, "Ticker not found: " ++ T.unpack ticker)
errorToResponse (NetworkError _) = (status502, "Network error fetching data")
errorToResponse (S3UploadError _) = (status500, "Failed to upload to S3")
errorToResponse (ApiError code msg) = (toEnum code, msg)