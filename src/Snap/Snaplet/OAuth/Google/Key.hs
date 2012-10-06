{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Google.Key where

import           Network.OAuth2.OAuth2 (OAuth2 (..))


googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = ""
                   , oauthClientSecret = ""
                   , oauthCallback = Nothing
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   , oauthAccessToken = Nothing
                   }

