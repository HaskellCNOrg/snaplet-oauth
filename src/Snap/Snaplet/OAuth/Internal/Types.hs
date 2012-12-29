{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Snaplet.OAuth.Internal.Types where

import           Data.Lens.Common
import Data.Text (Text)
import           Network.OAuth2.OAuth2
--import           Prelude               hiding ((.))
import           Data.Hashable                     (Hashable (..))
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as M
import           Snap
import           Snap.Snaplet.OAuth.Internal.Utils

----------------------------------------------------------------------
-- Snaplet
----------------------------------------------------------------------

-- |
--
data OAuthSnaplet = OAuthSnaplet { oauthKeys :: OAuthMap }

emptyOAuthSnaplet :: OAuthSnaplet
emptyOAuthSnaplet = OAuthSnaplet (OAuthMap M.empty)

-- | TODO: just define `getOauthSnaplet` without oauthLens
--
-- | Some snaplet implementation just define a get function is because is read only snaplet.
--   Therefore, the get function could be like
--   `getXXX = with oauth Snap.get`
--   where the `oauth` here can be found at `data App = App { _oauth : xxxx, ....}
--
class HasOAuth b where
  oauthLens :: Lens b (Snaplet OAuthSnaplet)


getOauthSnaplet :: HasOAuth b => Handler b v OAuthSnaplet
getOauthSnaplet = withTop oauthLens Snap.get


getOauthKeys :: HasOAuth b => Handler b v OAuthMap
getOauthKeys = liftM oauthKeys getOauthSnaplet


lookupOAuth :: HasOAuth b => OAuthKey -> Handler b v (Maybe OAuthValue)
lookupOAuth name = do
    (OAuthMap keys) <- getOauthKeys
    return $ M.lookup name keys


----------------------------------------------------------------------
-- OAuth Keys
----------------------------------------------------------------------


newtype OAuthMap = OAuthMap (HashMap OAuthKey OAuthValue)

newtype OAuthKey = OAuthKey { unkey :: String }
                   deriving (Hashable, Eq)

type OAuthValue = OAuth2

google, github, weibo :: OAuthKey
google = OAuthKey "google"
github = OAuthKey "github"
weibo = OAuthKey "weibo"

instance Show OAuthKey where
  show = unkey
