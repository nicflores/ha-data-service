# Functional APIs in the Cloud with Haskell

## Motivation

Pragraph Summary:

1. old timer web devs
2. Web Servers Tomcat, Apache, Nginx\*

If you have been doing web developemnt for any amount of time you probably remember the times
when you had to deploy a web server that may host one more web code bases. Look up Tomcat, Apache, or Nginx. Though, Nginx is still useful for reverse proxies. I spent some time deploying and tweaking these servers long ago, little that I know that you can actually write your own web servers!

Paragraph Summary:

1. Learning about Http4s
2. Web dev perspective changed

Back when I was first learning Scala, I attended a Scala talk about web APIs. At some point during the talk I rasied my hand and I naively asked "What server do you deploy your run your code in?". To which the speaker answered "Oh, we write our own.". My head exploded! I asked "How?". He answered "Oh, we use Http4s.". It was at this point that my entire perspective of web development changed.

Paragraph Summary:

1. Other web server libs
2. Web mental model
3. Web servers are hard
4. Car analogy

I wanted to learn as much as I could about writing my own web servers. Luckily throughout my career I've had the oppotunity to work with libraries like Http4s, ZIO Https, and Rust's Axum & Actix. Over the years, though, I've realized web servers are HARD! There's a lot they need to do. So, instead learning the inner workings of all of these frameworks, I've carried a mental model of web requests and their structure in my head. So when I approached each of these libraries I asked my self - How do I write a Router? - How do I add a header? - How do I pass path parameter? - etc. Withouth needing to dig too much under the hood for each framework.

### Presentation Goal

What I hope to build, over a series of talks, is a server that periodically downloads Yahoo finance data to an S3 bucket.
Depending on how things go, we could eventually

0. add a slide info about myself
1. a web api backend to download Yahoo finance data to an S3 bucket
2. Get Programming with Haskell by `Will Kurt`. Will Kurt mentions web
   development towards the end the book (around page 500ish). This talk takes some of the
   ideas presented at the end of that book and goes a little further.

## Haskell Web Framework

In the spirit of continuing my interest in writing web servers. I want to dive into this topic usign Haskell. We'll start by with a very simple example and build up over time all the while examining the underlying functional aspects of the Haskell we are writing.

A quick search reveals several web frameworks available within the Haskell ecosystem:

1. Yesod: A full-featured, batteries-included framework with strong type safety. Uses its own templating system (Hamlet), form handling, authentication, and persistent database layer. Best for large applications where you want comprehensive features out of the box.
2. Servant: A type-level DSL where you define your API as a type, and the framework generates both server and client code from it. Extremely type-safe and composable. Great for REST APIs where you want compile-time guarantees about your endpoints.
3. Scotty: A lightweight, Sinatra-inspired micro-framework focused on simplicity. Minimal boilerplate, easy to learn, and good for small services or when you want to pick your own libraries for everything else. Backed by Haskells WAI (Web Application Interface) and WARP a fast HTTP server built on WAI.

WAI is Haskell's standard interface between web servers and applications.
Think of it like:

1. Rack in Ruby
2. WSGI in Python.

In this talk we'll be using Scotty. The only reason for this choice was because glancing at the code for a basic server and handler for each of these frameworks, Scotty looked the most straight foward and most familiar to me.

To get started, a simple get request looks like using Scotty.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty (ScottyM, get, scotty, text)

main :: IO ()
main = scotty 8080 $ do  -- <------ you start the server on port 8080
  get "/" $ text "Yay!"  -- <------ define a web endpoint/handler
```

Here `scotty` starts a server on port `8080` and the `get` line defines a handler for requests to `/`.

`ScottyM` is a scotty web app - it describes how to process requests and the responses it should send

`ActionM` describes what to do when receving a request

We'll be adding two handlers:

- a get endpoint to check the health of the service
- a put endpoint to download a file from Yahoo fianance to S3.

### Health Handler

Since

```haskell
get :: RoutePattern -> ActionM () -> ScottyM ()
```

then we can pick `healthCheck`'s signature to look like:

```haskell
healthCheck :: ScottyM ()
```

then we add the get RoutePattern

```haskell
healthCheck :: ScottyM ()
healthCheck =
  get "/health" $ do
```

we can use `do` here because of `ActionM ()`.

The action can now simply be:

```haskell
healthCheck :: ScottyM ()
healthCheck =
  get "/health" $ do
  json $ Health "OK"
```

where `Health` is the data type

```haskell
newtype Health = Health
  {health_status :: Text}
  deriving (Show, Eq, Generic)
instance ToJSON Health
```

We use `newtype` since we are only creating a type with one field.

### Download Handler

The download handler is a bit more involved.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
```

This handler will take an AppConfig parameter. Why?
Well, it needs to know which bucket to write the data and various other pieces of info in order to do it's job.

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  -- ??? --
```

Here we see the app config parameter and also the path parameter.
Again, we can use the `do` notation because the of `ActionM ()`.

First task is to extract the path parameter using `pathParam`.

We'll do a little ad-hoc validation on the passed in ticker. If that check fails
we return a 400. If validation is ok we proceed.

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

There are two stubs here. One where we ideally perform some work and get a `result`.
And the other stub, where we handle what we get in the `result`.

Let's handle the `result` first:

```haskell
downloadTickerData :: AppConfig -> ScottyM ()
downloadTickerData config = post "/download/:ticker" $ do
  ticker <- pathParam "ticker"
  if BS.null ticker || BS.length ticker > 5
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

Supposing the `result` is an `Either` we pattern match and handle each situation.

What's left to define here is the real work this function should do.

Let's build up what the type of the function that should go here (the one that does the work).

I already mentioned either. So we should expect something like `Either X Y`.
We are in a `do` block, so we can pick type that we can use here. We know that
this function need to make web requests, which a side effect ... ie. IO. so maybe the function that should go here should return an `IO (Either X Y)`.

Lastly, we're in `IO`. and the `do` notation right above `result` puts us in `ActionM` which if you look in the source code you it's a synonym for `ActionT` which dervies all the "good" type classes to `lift our IO` into `ActionT`.

So the stub above will look like:

```haskell
      result <- liftIO $ -- IO (Either X Y)
```

So, you can imagine what goes on the right hand of the `liftIO`.
It'll be something like this:

```haskell
      result <- liftIO $ copyDatatoS3 ticker config
```

since `ticker` and `config` are available to us. This should be enough for us to build what we need.

### Haskell HTTP Client

Again, a quick search reveals a few libraries we can use: `http-client`, `wreq`, and `http-conduit`.

`http-conduit` is derived from `http-client`. So let's take a stab at using `http-conduit`.
The equivalent of doing a `curl https://google.come` looks like this using `http-conduit`.

```haskell
simpleHttp :: IO (Either HttpException Int)
simpleHttp = do
  try $ do
    request <- parseRequest "https://google.com"
    response <- httpLbs request
    return $ getResponseStatusCode response
```

### Headers and Parameters

Now that we know how to make an http request we can go a little further.
The http request we are going to formulate in code is the following `curl` request:

```console
curl -A "Mozilla/5.0" --compressed "https://query2.finance.yahoo.com/v8/finance/chart/AAPL?range=1d&interval=1m&includePrePost=true&events=div%2Csplit"
```

```haskell
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)

let baseUrl = T.unpack $ "https://query2.finance.yahoo.com" <> "/" <> ticker
let headersParams = [
    ("User-Agent", "Mozilla/5.0")
  , ("Accept-Encoding", "gzip, deflate")
]
let queryParams = [
    ("range", Just "1d")
  , ("interval", Just "1m")
  , ("includePrePost", Just "true")
  , ("events", Just "div,split")
]
request <- setRequestHeaders headersParams . setRequestQueryString queryParams <$> parseRequest baseUrl
response <- httpLbs request
```

This is very similar to what we did previously, we've just added the query and header parameters assembled the request and then fired it off using `httpLbs`.

If the response code is `200` we are ready to write to S3 if not we return string mentioning the error code. At this point there's two more tasks to do:

1. Write to S3
2. Write the body of the response to S3.

### AWS Libraries

Amazonka provides a `send` function that takes the credentials and the request to send to AWS.

## Haskell AWS SDK

Since we'll be writing a file to S3 we need a Haskell AWS library. Again, a quick search reveals there are several: `aws`, `amazonka`, and `aws-sdk`. In this talk we'll be using `aws`.

To demonstrate how we write data to S3 we do the following.

```haskell
main :: IO ()
main = do
  cfg <- Aws.baseConfiguration
  mgr <- newManager tlsManagerSettings
  let sampleData = "Hello from Haskell! This is a test upload to S3."
  let byteString = LBS.pack $ map (fromIntegral . fromEnum) sampleData
  let putObj = getAwsPutObj "my-bucket" "test_data.txt" byteString
  sendResp <- sendPutObj cfg mgr putObj
```

Notice that we mainly need 4 things. The `Configuration`, an http client `Manager`.
Then we need to build a put object as well as sending the put object.

This is the general idea of writing to S3.

### Writng the body to S3

We'll use `Aws.pureAws` to send our data to S3 by passing in:

1. Configuration
2. Manager
3. PutObject
4. Aws.defServiceConfig

```haskell
Aws.pureAws cfg Aws.defServiceConfig mgr putObj
```

Building the types above will look something like this:

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
      -- the storage path looks like:
      -- /financial-data/2025/10/19/AAPL_063259.json
      let objectKey = generateS3Key s3Config ticker currentTime
      -- bucket name object
      let bucketName = bucket s3Config
      -- generate put object
      let putObj = depGetAwsPutObj deps bucketName objectKey body
      -- seen before
      depUploadToS3 deps cfg mgr putObj
```

This represents the meat of what we set out to do. But there's more to do to make our application worthy of "web service". What's missing?

1. we need write a handler that calls `copyUrltoS3` properly
2. we've hardcoded quite a few things, we should probably handle that by writing a config
3. the ticker should probably be a url path parameter
4. better error handling, since this is a web service it would be good to return proper status codes to the client

### Copy URL Handler

### Config

### Ticker URL Parameter

### Error Handling

### Exercise Deployed Service

### Additional Work

1. write some Haskell to do some kind of simple analysis on the financial data downloaded to an S3 bucket
2. write a samll distributed system and have the leader service communicate with the a worker service over grpc or a message bus, with the leader service to receive messages
3. or we can do some sentiment on company news snippets

### Notes

VS Code Hlint plugin
