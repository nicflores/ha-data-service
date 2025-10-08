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

→ Mention library and choices made to help accomplish our goal.

. . .

→ Learn how to make http request to third party API.

. . .

→ Learn how to interact with AWS.

. . .

→ Add basic application config.

. . .

→ Look at testing techniques.

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

- it looked familiar
- simplicity

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

---

# Using Scotty

&nbsp;

&nbsp;

```haskell
main :: IO ()
main = scotty 8080 $ do    --  <-- start the server on port 8080
  get "/" $ text "Hello!"  --  <-- defines a get web api endpoint
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

```haskell
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestHeaders, setRequestQueryString)

let baseUrl = T.unpack $ "https://query2.finance.yahoo.com" <> "/" <> ticker
let headersParams = [...]
let queryParams = [...]
request <- setRequestHeaders headersParams . setRequestQueryString queryParams <$> parseRequest baseUrl
response <- httpLbs request
```

- `.` is function composition
- `<$>` is the infix operator for `fmap`

---

# Yahoo Finance Request

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

- `.` is function composition
- `<$>` is the infix operator for `fmap`

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

In our case we'll swap `newGetCallerIdentity` by request to write a file to S3.

---

# Application Config

---

# Testing

---

# AWS Deployment

---

# Quick Demo

---

# Takeaways

. . .

→ Haskell eco system was better than I exepcted.

. . .

→ Library serach is on par with Rust library search, I believe.

. . .

→ Getting used to some of the operators takes a little bit of thought.

. . .

→ Deploying haskell services takes about a long as it does in Rust.

---

# Future Ideas

. . .

Things to add

- more tests
- better logging
- instead of making a request to download data do it on a scheduler (ie. write a scheduler)

. . .

What else could we present?

- do an analysis on download data
- turn this into a distributed system event driven system with a leader and workers
- download news given a ticker and do sentiment on new snippets

---

# Thank You!

**Questions?**

Github: https://github.com/nicflores/ha-data-service
