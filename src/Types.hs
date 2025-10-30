{-# LANGUAGE DeriveGeneric #-}

module Types (Ticker, AppConfig (..), CopyToS3Error (..), Dependencies (..), S3Config (..), FetchConfig (..), TickerLookupError (..), errorToResponse, Health (..), DownloadStatus (..), ObjectKey (..), BucketName) where

import qualified Aws
import Aws.S3 (PutObject)
import qualified Aws.S3 as S3
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text as T (Text, unpack)
import qualified Data.Time as Data.ByteString.Lazy.LazyByteString
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Simple (HttpException, JSONException, Request)
import Network.HTTP.Types.Status (Status, status404, status500, status502)

type Ticker = Text

type BucketName = Text

newtype ObjectKey = ObjectKey Text
  deriving (Show, Eq)

newtype Health = Health
  {health_status :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Health

instance FromJSON Health

data DownloadStatus = DownloadStatus
  { status :: Int,
    message :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON DownloadStatus

instance FromJSON DownloadStatus

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
    depGetAwsPutObj :: BucketName -> ObjectKey -> LazyByteString -> PutObject,
    depSendPutObj :: Aws.Configuration -> Manager -> PutObject -> IO (Either CopyToS3Error S3.PutObjectResponse),
    depGetCurrentTime :: IO Data.ByteString.Lazy.LazyByteString.UTCTime,
    depNewMgr :: IO Manager,
    depNewCfg :: IO Aws.Configuration
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