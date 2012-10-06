{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Google.Key where

import           Network.OAuth2.OAuth2 (OAuth2 (..))

newtype GoogleOAuth = GoogleOAuth { key :: OAuth2 }

googleKey :: OAuth2
googleKey = key defaultKey

defaultKey :: GoogleOAuth
defaultKey = GoogleOAuth
            OAuth2 { oauthClientId = "886894027376.apps.googleusercontent.com"
                   , oauthClientSecret = "27w98gwGB1h8N5a6JQ2bT_nm"
                   , oauthCallback = Just "http://127.0.0.1:9988/googleCallback"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   , oauthAccessToken = Nothing
                   }

