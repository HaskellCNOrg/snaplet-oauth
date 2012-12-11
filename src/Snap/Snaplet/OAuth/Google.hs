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
import qualified Data.ByteString                      as BS
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

-- |
loginWithGoogleH :: HasOAuth b => Handler b v ()
loginWithGoogleH = loginWithOauthH Google scopeParam
                   where scopeParam = Just $ renderSimpleQuery False scopeQuery
                         scopeQuery = [(googleScopeKey, BS.intercalate " " scopes)]
                         -- | multiple scope in order to get email info
                         scopes = [googleScopeEmail, googleScopeUserInfo]


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
