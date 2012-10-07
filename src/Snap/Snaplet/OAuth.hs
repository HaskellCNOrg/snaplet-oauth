{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth
       ( initOauthSnaplet
       , module OA
       , module HS
       , module UT
       , module TY ) where

import           Data.ByteString                      (ByteString)
import           Snap

import           Network.OAuth2.OAuth2                as OA
import qualified Snap.Snaplet.OAuth.Google            as G
import           Snap.Snaplet.OAuth.Internal.Handlers as HS
import           Snap.Snaplet.OAuth.Internal.Types    as TY
import           Snap.Snaplet.OAuth.Internal.Utils    as UT
import qualified Snap.Snaplet.OAuth.Weibo             as W

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
--
initOauthSnaplet :: HasOAuth b
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
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes = W.routes
         <|>
         G.routes
