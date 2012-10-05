{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Types where

import           Control.Category
import           Control.Concurrent.MVar
import qualified Data.ByteString         as BS
import           Data.Lens.Common
import           Network.OAuth2.OAuth2
import           Prelude                 hiding ((.))
import           Snap

-------------------------------------------------------

-- |
--  FIXME: further, the OAuth2 should be a Map since accessToken is vary among users.
--
data OAuthSnaplet = OAuthSnaplet
                    { getOauth     :: MVar OAuth2    -- ^ This is major oauth related data
                    , getCodeParam :: BS.ByteString  -- ^ query param that oauth provider will use at callback url.
                    }                                -- ^ e.g. localhost/oauthCallback?code=123, so pick up 'code'.

emptyOAuthSnaplet :: OAuthSnaplet
emptyOAuthSnaplet = OAuthSnaplet (undefined :: MVar OAuth2) "code"

-- | TODO: just define `getOauthSnaplet` without oauthLens
--
-- | Some snaplet implementation just define a get function is because is read only snaplet.
--   Therefore, the get function could be like
--   `getXXX = with oauth Snap.get`
--   where the `oauth` here can be found at `data App = App { _oauth : xxxx, ....}
--
class HasOauth b where
  oauthLens :: Lens b (Snaplet OAuthSnaplet)

  oauthLens' :: Lens (Snaplet b) (Snaplet OAuthSnaplet)
  oauthLens' = subSnaplet oauthLens

getOauthSnaplet :: HasOauth b => Handler b v OAuthSnaplet
getOauthSnaplet = withTop oauthLens Snap.get

readOAuthMVar' :: HasOauth b => OAuthSnaplet -> Handler b v OAuth2
readOAuthMVar' = liftIO . readMVar . getOauth

readOAuthMVar :: HasOauth b => Handler b v OAuth2
readOAuthMVar = getOauthSnaplet >>= readOAuthMVar'
