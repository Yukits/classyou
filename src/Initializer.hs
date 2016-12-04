{-
 This module is for parsing data into input vector of Classifier.
 Please install module aeson authenticate-oauth http-conduit conduit conduit-extra ... from cabal.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Initializer
    (
    ) where

      import Web.Authenticate.OAuth
      import Data.Text (Text)
      import qualified Data.Text.IO as T
      import Data.Aeson
      import GHC.Generics
      import Network.HTTP.Conduit

      consumerKey = ""
      consumerSecret = ""
      accessToken = ""
      accessTokenSecret = ""

      numOfTweets = 100
      userId = "dave_spector"

      myOAuth :: OAuth
      myOAuth = newOAuth { oauthServerName = "api.twitter.com"
                         , oauthConsumerKey = consumerKey
                         , oauthConsumerSecret = consumerSecret
                         }

      myCred :: Credential
      myCred = newCredential accessToken accessTokenSecret

      data Tweet = Tweet { text :: !Text
                         } deriving (Show, Generic)

      instance FromJSON Tweet
      instance ToJSON Tweet

      getTweets :: String -> IO (Either String [Tweet])
      getTweets name = do
          req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
          m <- newManager tlsManagerSettings
          res <- do
                  signedreq <- signOAuth myOAuth myCred req
                  httpLbs signedreq m
          return $ eitherDecode $ responseBody res

      main :: IO ()
      main = do
          ets <- getTweets userId
          case ets of
            Left err -> putStrLn err
            Right ts -> mapM_ T.putStrLn . map text $ take numOfTweets ts
