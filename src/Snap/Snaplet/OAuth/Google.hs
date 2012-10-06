{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Google
       ( routes
       , loginWithGoogleH
       , googleCallbackH
       , userInfoH
       , module Snap.Snaplet.OAuth.Google.Api
       , module Snap.Snaplet.OAuth.Google.Key
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
import           Snap.Snaplet.OAuth.Google.Key
import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types

------------------------------------------------------------------------------
--              Google
------------------------------------------------------------------------------

-- | FIXME: How to support multiple scope??
--   BS.intercalate "+"  scopes)] **does not work**
--   scopes = [googleScopeEmail, googleScopeUserInfo]
--
loginWithGoogleH :: HasOauth b => Handler b v ()
loginWithGoogleH = loginWithOauthH googleKey scopeParam
                   where scopeParam = Just $ renderSimpleQuery False scopeQuery
                         scopeQuery = [(googleScopeKey, googleScopeUserInfo)]


googleCallbackH :: HasOauth b => Handler b v OAuth2
googleCallbackH = oauthCallbackH googleKey


userInfoH :: HasOauth b => OAuth2 -> Handler b v (Maybe GoogleUser)
userInfoH = liftIO . userInfo

------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/google", loginWithGoogleH)
          ]
