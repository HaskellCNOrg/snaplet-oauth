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
import           Control.Monad
import           Data.ByteString               (ByteString)
--import qualified Data.ByteString               as BS
import           Data.Maybe
import           Network.HTTP.Types            (renderSimpleQuery)
import           Prelude                       hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Google.Api
import           Snap.Snaplet.OAuth.Google.Key
import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

-- | FIXME: How to support multiple scope??
--
loginWithGoogleH :: HasOauth b => Handler b v ()
loginWithGoogleH = loginWithOauth scopeParam
                   where scopeParam = Just $ renderSimpleQuery False scopeQuery
                         scopeQuery = [(googleScopeKey, googleScopeUserInfo)]
                                        --BS.intercalate "+"  scopes)]
                   --      scopes = [googleScopeEmail, googleScopeUserInfo]

googleCallbackH :: HasOauth b => Handler b v ()
googleCallbackH = oauthCallbackHandler

oauthToGoogleOAuth :: HasOauth b => Handler b v GoogleOAuth
oauthToGoogleOAuth = liftM GoogleOAuth readOAuthMVar

userInfoH :: HasOauth b => Handler b v (Maybe GoogleUser)
userInfoH = oauthToGoogleOAuth
            >>= liftIO . userInfo

------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/google"        , loginWithGoogleH)
          ]
