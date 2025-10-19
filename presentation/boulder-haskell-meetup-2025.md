---
title: Building a Haskell Cloud Service
author: Nic Flores
date: October 21st 2025
patat:
  wrap: true
  margins:
    left: 10
    right: 10
  incrementalLists: true
  slideLevel: 1
  theme:
    emph: [bold, onVividBlue]
    strong: [bold, onVividMagenta]
    code: [vividGreen]
    codeBlock: [onRgb#282a36, rgb#f8f8f2]
    header: [bold, vividCyan, underline]
    linkTarget: [vividBlue, underline]
  images:
    backend: auto
---

# Welcome

. . .

&nbsp;

&nbsp;

&nbsp;

Boulder Haskell MeetUp

. . .

&nbsp;

&nbsp;

&nbsp;

... now online and viewable world wide!

---

# In Today's Talk

&nbsp;

. . .

→ We'll build a simple financial data downloader.

. . .

→ Library choices made to help accomplish our goal.

. . .

→ Assembling web requests.

. . .

→ Learn how to interact with AWS.

. . .

→ Add basic application config.

. . .

→ Testing techniques.

. . .

→ Deploy our application to AWS ECS.

---

# Web Frameworks

. . .

`Yesod` - https://www.yesodweb.com/ - comprehnsive

. . .

`Servant` - https://www.servant.dev/ - super type safe

. . .

`Scotty` - https://github.com/scotty-web/scotty - minimalist

. . .

&nbsp;

&nbsp;

&nbsp;

I chose to use `Scotty`

- examples in documentation look simple and clear to follow

---

# Using Scotty

. . .

&nbsp;

&nbsp;

```haskell
main :: IO ()
main = scotty 8080 $ do
  get "/" $ text "Hello!"
```

---

# Using Scotty

&nbsp;

&nbsp;

```haskell
main :: IO ()
main = scotty 8080 $ do    --  <-- start the server on port 8080
  get "/" $ text "Hello!"
```

. . .

```haskell
scotty :: Port -> ScottyM () -> IO ()
```

---

# Using Scotty

&nbsp;

&nbsp;

```haskell
main :: IO ()
main = scotty 8080 $ do    --  <-- start the server on port 8080
  get "/" $ text "Hello!"  --  <-- defines a get web api endpoint
```

```haskell
scotty :: Port -> ScottyM () -> IO ()
```

. . .

```haskell
get :: RoutePattern -> ActionM () -> ScottyM ()
```

. . .

We'll be writing two handlers:

- get `/health` for the cluster to check service health
- post `/download/:ticker` to allow users to download Yahoo fianance data to S3

---

# Scotty Health Handler

. . .

Let's define the `get` handler.

. . .

```haskell
healthCheck :: ScottyM ()
```

---

# Scotty Health Handler

Let's define the `get` handler.

```haskell
healthCheck :: ScottyM ()
healthCheck =
  get "/health" $ do
  -- ??? --
```

---

# Scotty Health Handler

Let's define the `get` handler.

```haskell
healthCheck :: ScottyM ()
healthCheck =
  get "/health" $ do
    json $ Health "OK"
```

. . .

`Health` is defined as:

```haskell
newtype Health = Health
  {health_status :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Health
```

---

# Scotty Download Handler

. . .

Now we define the `post` handler.

. . .

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  -- ??? --
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  -- ??? --
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      -- ??? --
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- -- ??? ---
      -- ??? --
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
    result <- -- Either X Y
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
    result <- -- IO (Either X Y)
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do               -- <------------- ???
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
    result <- -- IO (Either X Y)
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do               -- <------------- ActionT
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
    result <- -- IO (Either X Y)
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do               -- <------------- ActionT
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
    result <- liftIO $ -- IO (Either X Y)
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 10
    then do
      status status400
    else do
      result <- -- ??? ---
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

```haskell
result <- liftIO $ copyDatatoS3 ticker config
```

---

# Scotty Download Handler

Now we define the `post` handler.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 5
    then do
      status status400
    else do
      result <- liftIO $ copyDataToS3 ticker config
      case result of
        Left err -> do
          let (httpStatus, _) = errorToResponse err
          status httpStatus
        Right _ -> do
          status status200
```

---

# HTTP Clients

. . .

`http-client`

- https://github.com/snoyberg/http-client
- minimal
- low level

. . .

`http-conduit`

- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
- add on package to `http-client`
- higher level interface

. . .

`wreq`

- http://www.serpentine.com/wreq/
- full-featured
- last release was in 2017

&nbsp;

I chose to use `http-conduit`

---

# Using conduit

. . .

```haskell
import Network.HTTP.Simple (getResponseStatusCode, httpLbs, parseRequest)

simpleHttp :: IO (Either HttpException Int)
simpleHttp = do
  try $ do
    request <- parseRequest "https://google.com"
    response <- httpLbs request
    return $ getResponseStatusCode response
```

---

# Yahoo Finance Request

. . .

Requesting data by ticker

. . .

`URL`

- https://query2.finance.yahoo.com
- /v8/finance/chart/<ticker>

. . .

`Headers`

- User-Agent: "Mozilla/5.0"
- Accept-Encoding: gzip, deflate

. . .

`Query Parameters`

- range=1d
- interval=1m
- includePrePost=true
- events=div%2Csplit

---

# Yahoo Finance Request

. . .

```haskell
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)

buildYahooRequest :: FetchConfig -> Ticker -> IO Request
buildYahooRequest config ticker = do
  let url = T.unpack $ baseUrl config <> "/" <> ticker
  let queryParams = [...]
  let headers = [...]
  setRequestHeaders headers . setRequestQueryString queryParams <$> parseRequest url
```

- `.` is function composition: `(f . g) x = f (g x)`
- `<$>` map a function over a functor: `(+1) <$> [1, 2, 3] = [2,3,4]`

---

# Yahoo Finance Request

```haskell
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)

buildYahooRequest :: FetchConfig -> Ticker -> IO Request
buildYahooRequest config ticker = do
  let url = T.unpack $ baseUrl config <> "/" <> ticker
  let queryParams = [
      ("User-Agent", "Mozilla/5.0")
    , ("Accept-Encoding", "gzip, deflate")
  ]
  let headers = [
      ("range", Just "1d")
    , ("interval", Just "1m")
    , ("includePrePost", Just "true")
    , ("events", Just "div,split")
  ]
  setRequestHeaders headers . setRequestQueryString queryParams <$> parseRequest url
```

---

# AWS Libraries

. . .

`aws` - https://github.com/aristidb/aws

- seems to uspport minimal resources

. . .

&nbsp;

`amazonka` - https://github.com/brendanhay/amazonka

- auto generated from AWS APIs

. . .

&nbsp;

In this talk I chose to use `Amazonka`

- import individal libraries
- for example `amazonka-s3` and `amazonka-sts`
- more straight forward to use

---

# Using Amazonka

. . .

Let's see how Amazonka works.

. . .

```haskell
import Amazonka (discover, newEnv)

checkAwsAuth :: IO (Either String AuthInfo)
checkAwsAuth = do
  result <- try $ do
    env <- newEnv discover
    -- ...???... --
  case result of
    Left (ex :: SomeException) -> return $ Left (show ex)
    Right authInfo -> return $ Right authInfo
```

. . .

`discover` finds credentials

- environment variables
- config files
- web identity
- ecs container agent

---

# Using Amazonka

Let's see how Amazonka works.

```haskell
import Amazonka (discover, newEnv, runResourceT, send)
import Amazonka.STS (newGetCallerIdentity)

checkAwsAuth :: IO (Either String AuthInfo)
checkAwsAuth = do
  result <- try $ do
    env <- newEnv discover
    runResourceT $ do
      response <- send env newGetCallerIdentity
      -- ...???... --
  case result of
    Left (ex :: SomeException) -> return $ Left (show ex)
    Right authInfo -> return $ Right authInfo
```

. . .

Here we send `newGetCallerIdentity`.

. . .

In order to write a file we need to send a `PutObject`.

---

# Writing to S3

. . .

```haskell
uploadToS3 :: Env
  -> BucketName
  -> ObjectKey
  -> LazyByteString
  -> IO (Either CopyToS3Error PutObjectResponse)
uploadToS3 env bucketName objectKey body = do
  let putReq = newPutObject bucketName objectKey (toBody body)
  --- ??? ---
```

---

# Writing to S3

```haskell
uploadToS3 :: Env
  -> BucketName
  -> ObjectKey
  -> LazyByteString
  -> IO (Either CopyToS3Error PutObjectResponse)
uploadToS3 env bucketName objectKey body = do
  let putReq = newPutObject bucketName objectKey (toBody body)
  uploadResult <- try $ runResourceT $ send env putReq
  --- ??? --
```

---

# Writing to S3

```haskell
uploadToS3 :: Env
  -> BucketName
  -> ObjectKey
  -> LazyByteString
  -> IO (Either CopyToS3Error PutObjectResponse)
uploadToS3 env bucketName objectKey body = do
  let putReq = newPutObject bucketName objectKey (toBody body)
  uploadResult <- try $ runResourceT $ send env putReq
  case uploadResult of
    Left s3Ex -> return $ Left (S3UploadError s3Ex)
    Right putRes -> return $ Right putRes
```

---

# At this point

We've written:

- Our handlers (the downloder handler is mostly implemented)
- Learned how to make http requests
- Saw how to write a file to s3
- We just need to assemble our copyDataToS3

---

# Building Copy Data to S3

```haskell
copyUrltoS3 :: Dependencies
  -> Ticker
  -> AppConfig
  -> IO (Either CopyToS3Error PutObjectResponse)
copyUrltoS3 deps ticker config = do
  -- extract details from the config
  let fetchConfig =
        FetchConfig
          { baseUrl = yahooFinanceBaseUrl config,
            range = dataRange config,
            interval = dataInterval config
          }
```

---

# Building Copy Data to S3

```haskell
copyUrltoS3 :: Dependencies
  -> Ticker
  -> AppConfig
  -> IO (Either CopyToS3Error PutObjectResponse)
copyUrltoS3 deps ticker config = do
  -- extract details from the config
  let fetchConfig =
        FetchConfig
          { baseUrl = yahooFinanceBaseUrl config,
            range = dataRange config,
            interval = dataInterval config
          }
  -- seen perviously
  request <- buildYahooRequest fetchConfig ticker
  -- helper to extract body from request
  fetchResult <- depFetchData deps request
```

---

# Building Copy Data to S3

```haskell
copyUrltoS3 :: Dependencies
  -> Ticker
  -> AppConfig
  -> IO (Either CopyToS3Error PutObjectResponse)
copyUrltoS3 deps ticker config = do
  -- extract details from the config
  let fetchConfig =
        FetchConfig
          { baseUrl = yahooFinanceBaseUrl config,
            range = dataRange config,
            interval = dataInterval config
          }
  -- seen perviously
  request <- buildYahooRequest fetchConfig ticker
  -- helper to extract body from request
  fetchResult <- depFetchData deps request
  case fetchResult of
    Left err -> return $ Left err
    Right body -> do
      -- get the amazonka env from dependencies
      env <- depNewEnv deps
      -- needed for storage path
      currentTime <- depGetCurrentTime deps
      -- bucket info
      let s3Config =
            S3Config
              { bucket = s3Bucket config,
                prefix = s3Prefix config
              }
```

---

# Building Copy Data to S3

```haskell
copyDataToS3 :: Dependencies
  -> Ticker
  -> AppConfig
  -> IO (Either CopyToS3Error PutObjectResponse)
copyDataToS3 deps ticker config = do
  -- extract details from the config
  let fetchConfig =
        FetchConfig
          { baseUrl = yahooFinanceBaseUrl config,
            range = dataRange config,
            interval = dataInterval config
          }
  -- seen perviously
  request <- buildYahooRequest fetchConfig ticker
  -- helper to extract body from request
  fetchResult <- depFetchData deps request
  case fetchResult of
    Left err -> return $ Left err
    Right body -> do
      -- get the amazonka env from dependencies
      env <- depNewEnv deps
      -- needed for storage path
      currentTime <- depGetCurrentTime deps
      let s3Config =
            S3Config
              { bucket = s3Bucket config,
                prefix = s3Prefix config
              }
      -- the storage path
      let objectKey = generateS3Key s3Config ticker currentTime
      -- bucket name object
      let bucketName = BucketName (bucket s3Config)
      -- seen before
      depUploadToS3 deps env bucketName objectKey body
```

---

# Application Config

Requierments

- run locally
- read environment

. . .

```haskell
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
```

. . .

```haskell
loadConfigFromFile :: FilePath -> IO (Maybe AppConfig)
```

. . .

```haskell
loadConfigFromEnv :: IO (Maybe AppConfig)
```

. . .

then use the alternative operator `<|>` between them

---

# Testing

```console
├── app
│   └── Main.hs
├── config.json
├── hs-user-mgr.cabal
├── src
│   ├── Config.hs
│   ├── DataService.hs
│   ├── Handlers.hs
│   ├── TickerLookup.hs
│   └── Types.hs
└── test
    ├── ConfigSpec.hs
    ├── DataServiceSpec.hs
    ├── HandlersSpec.hs
    ├── Spec.hs             <------ Like a main() for tests
    ├── TickerLookupSpec.hs
    └── TypesSpec.hs
```

---

# Testing

Notice that:

```haskell
copyDataToS3 :: Dependencies
  -> Ticker
  -> AppConfig
  -> IO (Either CopyToS3Error PutObjectResponse)
```

. . .

Notice that we passed in a `Dependencies` object.
Along with using various pure functions.

. . .

This allows independenly testing the various functions used `copyDataToS3`.

. . .

I used `hspec` to help structure my tests.

. . .

```haskell

```

---

# AWS Deployment

---

# Quick Demo

---

# Takeaways

. . .

→ Haskell eco system was better than I exepcted.

. . .

→ Library search is on par with Rust library search, I believe.

. . .

→ Getting used to some of the operators takes a little bit of thought.

. . .

→ Deploying haskell services takes about a long as it does in Rust.

---

# Future Ideas

. . .

Things to add

- improve tests
- improve logging
- instead of making a request to download data do it on a scheduler (ie. write a scheduler)
- security

. . .

What else could we present?

- do an analysis on download data
- turn this into a distributed system event driven system with a leader and workers
- download news given a ticker and do sentiment analysis on new snippets

---

# Thank You!

**Questions?**

Github: https://github.com/nicflores/ha-data-service
