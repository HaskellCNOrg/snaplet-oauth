{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Types where

import           Data.Lens.Common
import           Network.OAuth2.OAuth2
--import           Prelude               hiding ((.))
import           Snap
import Data.Hashable (Hashable(..))
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as M
import Snap.Snaplet.OAuth.Utils

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
class HasOauth b where
  oauthLens :: Lens b (Snaplet OAuthSnaplet)

getOauthSnaplet :: HasOauth b => Handler b v OAuthSnaplet
getOauthSnaplet = withTop oauthLens Snap.get

getOauthKeys :: HasOauth b => Handler b v OAuthMap
getOauthKeys = liftM oauthKeys $ withTop oauthLens Snap.get

--lookupOAuthDefault :: HasOauth b => OAuth2 -> OAuthKey -> Handler b v OAuth2
--lookupOAuthDefault def name = do
--    (OAuthMap keys) <- getOauthKeys
--    return $ M.lookupDefault def name keys

lookupOAuth :: HasOauth b => OAuthKey -> Handler b v (Maybe OAuthValue)
lookupOAuth name = do
    (OAuthMap keys) <- getOauthKeys
    return $ M.lookup name keys

----------------------------------------------------------------------
-- OAuth Keys
----------------------------------------------------------------------


newtype OAuthMap = OAuthMap (HashMap OAuthKey OAuthValue)

type OAuthValue = OAuth2

data OAuthKey = Google | Github | Twitter | Facebook | Weibo | QQ
                deriving (Show, Eq, Enum)

instance Hashable OAuthKey where
  hash = hash . sToBS . show
