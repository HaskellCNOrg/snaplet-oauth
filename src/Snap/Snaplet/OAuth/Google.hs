{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Google
       ( routes
       , loginWithGoogleH
       , googleCallbackH
       , userInfoH
       , module Snap.Snaplet.OAuth.Google.Api
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Data.ByteString               (ByteString)
import           Data.Maybe
import           Network.HTTP.Types            (renderSimpleQuery)
import           Prelude                       hiding ((.))
import           Snap

import           Network.OAuth2.OAuth2
import           Snap.Snaplet.OAuth.Google.Api
import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types

------------------------------------------------------------------------------
--              Google
------------------------------------------------------------------------------

-- | FIXME: How to support multiple scope??
--   according to OAuth 2.0 playround, multiple scope is supposed.
--   BS.intercalate "+"  scopes)] **does not work**
--   scopes = [googleScopeEmail, googleScopeUserInfo] **in order to get email**
--
loginWithGoogleH :: HasOauth b => Handler b v ()
loginWithGoogleH = googleOAuth
                   >>= flip loginWithOauthH scopeParam
                   where scopeParam = Just $ renderSimpleQuery False scopeQuery
                         scopeQuery = [(googleScopeKey, googleScopeUserInfo)]


googleCallbackH :: HasOauth b => Handler b v OAuth2
googleCallbackH = googleOAuth >>= oauthCallbackH


userInfoH :: HasOauth b => OAuth2 -> Handler b v (Maybe GoogleUser)
userInfoH = liftIO . userInfo

googleOAuth :: HasOauth b => Handler b v (Maybe OAuth2)
googleOAuth = lookupOAuth "google"

------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/google", loginWithGoogleH)
          ]
