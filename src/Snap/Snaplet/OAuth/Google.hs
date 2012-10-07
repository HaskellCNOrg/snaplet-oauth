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
import           Data.ByteString                      (ByteString)
import           Data.Maybe
import           Network.HTTP.Types                   (renderSimpleQuery)
import           Prelude                              hiding ((.))
import           Snap

import           Network.OAuth2.OAuth2
import           Snap.Snaplet.OAuth.Google.Api
import           Snap.Snaplet.OAuth.Internal.Handlers
import           Snap.Snaplet.OAuth.Internal.Types

------------------------------------------------------------------------------
--              Google
------------------------------------------------------------------------------

-- | FIXME: How to support multiple scope??
--   according to OAuth 2.0 playround, multiple scope is supposed.
--   BS.intercalate "+"  scopes)] **does not work**
--   scopes = [googleScopeEmail, googleScopeUserInfo] **in order to get email**
--
loginWithGoogleH :: HasOAuth b => Handler b v ()
loginWithGoogleH = loginWithOauthH Google scopeParam
                   where scopeParam = Just $ renderSimpleQuery False scopeQuery
                         scopeQuery = [(googleScopeKey, googleScopeUserInfo)]


googleCallbackH :: HasOAuth b => Handler b v OAuth2
googleCallbackH = oauthCallbackH Google


userInfoH :: HasOAuth b => OAuth2 -> Handler b v (Maybe GoogleUser)
userInfoH = liftIO . userInfo

------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/google", loginWithGoogleH)
          ]
