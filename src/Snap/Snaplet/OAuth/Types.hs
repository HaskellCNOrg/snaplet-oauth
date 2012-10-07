{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Types where

import           Data.Lens.Common
import           Network.OAuth2.OAuth2
import           Prelude               hiding ((.))
import           Snap

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as M

----------------------------------------------------------------------
-- Snaplet
----------------------------------------------------------------------

-- |
--
data OAuthSnaplet = OAuthSnaplet { oauthKeys :: OAuthKeys }

emptyOAuthSnaplet :: OAuthSnaplet
emptyOAuthSnaplet = OAuthSnaplet (OAuthKeys M.empty)

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

getOauthKeys :: HasOauth b => Handler b v OAuthKeys
getOauthKeys = liftM oauthKeys $ withTop oauthLens Snap.get

lookupOAuthDefault :: HasOauth b => OAuth2 -> String -> Handler b v OAuth2
lookupOAuthDefault def name = do
    (OAuthKeys keys) <- getOauthKeys
    return $ M.lookupDefault def name keys

lookupOAuth :: HasOauth b => String -> Handler b v (Maybe OAuth2)
lookupOAuth name = do
    (OAuthKeys keys) <- getOauthKeys
    return $ M.lookup name keys

----------------------------------------------------------------------
-- OAuth Keys
----------------------------------------------------------------------


newtype OAuthKeys = OAuthKeys (HashMap String OAuth2)
