{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Google
       (
         -- * Routes
         routes
         -- * Handlers
       , googleLoginH
       , googleCallbackH
       , googleUserH
         -- * Types and API
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

import           Snap.Snaplet.OAuth.Google.Api
import           Snap.Snaplet.OAuth.Internal.Handlers
import           Snap.Snaplet.OAuth.Internal.Types

------------------------------------------------------------------------------
--              Google
------------------------------------------------------------------------------

-- |
googleLoginH :: HasOAuth b => Handler b v ()
googleLoginH = loginWithOauthH google scopeParam
  where scopeParam = Just $ renderSimpleQuery False scopeQuery
        scopeQuery = [(googleScopeKey, BS.intercalate " " scopes)]
        -- | multiple scope in order to get email info
        scopes = [googleScopeEmail, googleScopeUserInfo]


googleCallbackH :: HasOAuth b => Handler b v OAuthValue
googleCallbackH = oauthCallbackH google


googleUserH :: HasOAuth b => Handler b v (Maybe GoogleUser)
googleUserH = googleCallbackH
              >>= liftIO . userInfo

------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/google", googleLoginH)
          ]
