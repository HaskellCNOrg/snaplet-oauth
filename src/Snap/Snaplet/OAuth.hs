{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth
       ( initOauthSnaplet ) where

import           Data.ByteString           (ByteString)
import           Snap

import qualified Snap.Snaplet.OAuth.Google as G
import           Snap.Snaplet.OAuth.Types
import qualified Snap.Snaplet.OAuth.Weibo  as W

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
--
initOauthSnaplet :: HasOauth b
                    => Bool
                    -- ^ Add default routes or not
                    -> SnapletInit b OAuthSnaplet
initOauthSnaplet rt =
    makeSnaplet "OAuthSnaplet" "" Nothing $ do
      when rt (addRoutes routes)
      return emptyOAuthSnaplet


-- | Snap Handlers
--   TODO: add routes per config [weibo, google, github]
--
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes = W.routes
         <|>
         G.routes
