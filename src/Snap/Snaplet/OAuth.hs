{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth
       ( initOauthSnaplet
       , module OA
       , module HS
       , module SP
       , module UT
       , module Snap.Snaplet.OAuth.Types ) where

import           Data.ByteString             (ByteString)
import           Snap

import           Network.OAuth2.OAuth2       as OA
import qualified Snap.Snaplet.OAuth.Google   as G
import           Snap.Snaplet.OAuth.Handlers as HS
import           Snap.Snaplet.OAuth.Splices as SP
import           Snap.Snaplet.OAuth.Utils as UT
import           Snap.Snaplet.OAuth.Types
import qualified Snap.Snaplet.OAuth.Weibo    as W

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
--
initOauthSnaplet :: HasOauth b
                    => Bool
                    -- ^ Add default routes or not
                    -> OAuthMap
                    -- ^ Oauth Keys
                    -> SnapletInit b OAuthSnaplet
initOauthSnaplet rt oauths =
    makeSnaplet "OAuthSnaplet" "" Nothing $ do
        when rt (addRoutes routes)
        return $ emptyOAuthSnaplet { oauthKeys = oauths }

-- | Snap Handlers
--   ?? TODO: add routes per config [weibo, google, github]
--
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes = W.routes
         <|>
         G.routes
