{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Types where

import           Data.Lens.Common
import           Prelude                 hiding ((.))
import           Snap

-------------------------------------------------------

-- |
--
data OAuthSnaplet = OAuthSnaplet

emptyOAuthSnaplet :: OAuthSnaplet
emptyOAuthSnaplet = undefined :: OAuthSnaplet

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
