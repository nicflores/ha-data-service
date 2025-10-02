# Functional APIs in the Cloud with Haskell

## Motivation

Pragraph Summary:
1. old timer web devs
2. Web Servers Tomcat, Apache, Nginx*

If you have been doing web developemnt for any amount of time you probably remember the times
when you had to deploy a web server that may host one more web code bases. Look up Tomcat, Apache, or Nginx. Though, Nginx is still useful for reverse proxies. I spent some time deploying and tweaking these servers long ago,  little that I know that you can actually write your own web servers!


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
1. a web api backend to download Yahoo finance data to an S3 bucket
2. write some Haskell to do some kind of simple analysis on the financial data downloaded to an S3 bucket
3. write a samll distributed system and have the leader service communicate with the a worker service over grpc or a message bus, with the leader service to receive messages 
4. or we can do some sentiment on company news snippets

## Haskell Web Framework

In the spirit of continuing my interest in writing web servers. I want to now dive into this topic usign Haskell. We'll start by with a very simple example and build up over time all the while examining the underlying functional aspects of the Haskell we are writing.

A quick search reveals several web frameworks available within the Haskell ecosystem: `Yesod`, `Servant`, `Scotty`.
In this talk we'll be using Scotty. The only reason for this choice was because at a glace at a basic server and handler for each of these frameworks, Scotty looked the most straight foward and most familiar to me.

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
Seems straightfoward enough to add a second handler. We'll be adding two handlers one to check the health of the service and one to download a file from Yahoo fianance to S3.

## Haskell AWS SDK
Since we'll be writing a file to S3 we need a Haskell AWS library. Again a quick search reveals there are several: `aws`, `amazonka`, and `aws-sdk`. In this talk we'll be using `amazonka` event though its a bit older I like how they've isolated the interaction with various AWS resources by publishing libraries like `amazonka-s3`, `amazonka-ec2`, `amazonka-sns` etc.

Here'e well be using `amazonka`, `amazonka-s3`, and `amazonka-sts` (security token service)
A simple example of using `amazonka` is as follows:

```haskell
checkAwsAuth :: IO (Either String AuthInfo)
checkAwsAuth = do
  result <- try $ do
    env <- newEnv discover
    runResourceT $ do
      response <- send env newGetCallerIdentity
      return $
        AuthInfo
          { account = response ^. getCallerIdentityResponse_account,
            arn = response ^. getCallerIdentityResponse_arn,
            userId = response ^. getCallerIdentityResponse_userId
          }
  case result of
    Left (ex :: SomeException) -> return $ Left (show ex)
    Right authInfo -> return $ Right authInfo
```

Here we call the `newEnv discover` which will try different ways to attempt to log into AWS.
Once we have our `env` we can use it to send requests to AWS. In this case we as for the Callers Identity to verify the identity of account we are logged in as.

We'll use this to write files to S3.

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

### Tying things together
Now that we know how to authenticate to AWS and make an http request we might have enough to put something together to:
1. download a data file from Yahoo finance
2. write the data file to S3

The http request we are going to attempt o mimic in Haskell is the following

```console
curl -vvv -A "Mozilla/5.0" --compressed "https://query2.finance.yahoo.com/v8/finance/chart/AAPL?range=1d&interval=1m&includePrePost=true&events=div%2Csplit"
```

Which we can do using:

```haskell
copyUrltoS3 :: IO (Either String ())
copyUrltoS3 = do
  env <- newEnv discover

  result <- try $ do
    let baseUrl = "https://query2.finance.yahoo.com/v8/finance/chart/AAPL"
    let queryParams = [("range", Just "1d"), ("interval", Just "1m"), ("includePrePost", Just "true"), ("events", Just "div,split")]
    let headersParams = [("User-Agent", "Mozilla/5.0"), ("Accept-Encoding", "gzip, deflate")]
    request <- parseRequest baseUrl
    let requestWithOptions = setRequestHeaders headersParams $ setRequestQueryString queryParams request
    response <- httpLbs requestWithOptions
    if getResponseStatusCode response == 200
      then do
      --- write response body to S3
      else error $ "HTTP error: " ++ show (getResponseStatusCode response)

  case result of
    Left (ex :: HttpException) -> return $ Left (show ex)
    Right _ -> return $ Right ()
```

This is very similar to what we did previously, just we've just added the query and header parameters.


Now to fill in the part where we write the file, will look something like this:

```haskell
copyUrltoS3 :: IO (Either String ())
copyUrltoS3 = do
  env <- newEnv discover

  result <- try $ do
    let baseUrl = "https://query2.finance.yahoo.com/v8/finance/chart/AAPL"
    let queryParams = [("range", Just "1d"), ("interval", Just "1m"), ("includePrePost", Just "true"), ("events", Just "div,split")]
    let headersParams = [("User-Agent", "Mozilla/5.0"), ("Accept-Encoding", "gzip, deflate")]
    request <- parseRequest baseUrl
    let requestWithOptions = setRequestHeaders headersParams $ setRequestQueryString queryParams request
    response <- httpLbs requestWithOptions
    if getResponseStatusCode response == 200
      then do
        -- write response body to S3
        currentTime <- getCurrentTime
        let datePath = formatTime defaultTimeLocale "%Y/%m/%d" currentTime
        let timeStamp = formatTime defaultTimeLocale "%H%M%S" currentTime
        let objectKey = pack $ "financial-data/" ++ datePath ++ "/data_" ++ timeStamp ++ ".json"
        let bodySource = getResponseBody response
        let putReq =
              newPutObject "nf-json-data" (ObjectKey objectKey) $
                toBody bodySource
        runResourceT $ do
          _resp <- send env putReq
          return ()
      else error $ "HTTP error: " ++ show (getResponseStatusCode response)

  case result of
    Left (ex :: HttpException) -> return $ Left (show ex)
    Right _ -> return $ Right ()
```

We formulate the path, key, where we'll be writing the file in S3 and also the name of the file.
Then we specify the bucket and again use the same `send env ...` pattern. 

