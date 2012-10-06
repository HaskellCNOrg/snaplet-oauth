{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Weibo.Key where

import           Network.OAuth2.OAuth2

weiboKey :: OAuth2
weiboKey = OAuth2 { oauthClientId = ""
                  , oauthClientSecret = ""
                  , oauthCallback = Nothing
                  , oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                  , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token"
                  , oauthAccessToken = Nothing
                  }
