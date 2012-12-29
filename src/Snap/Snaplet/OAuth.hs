{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth
       ( initOauthSnaplet
       , module OA
       , module HS
       , module UT
       , module TY ) where

import           Data.ByteString                      (ByteString)
import           Data.HashMap.Strict                  (member)
import           Snap

import           Network.OAuth2.OAuth2                as OA
import qualified Snap.Snaplet.OAuth.Github            as GH
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
  makeSnaplet "OAuth" "Snaplet - OAuth Client" Nothing $ do
    when rt (addDefaultRouters oauths)
    return $ emptyOAuthSnaplet { oauthKeys = oauths }

addDefaultRouters :: HasOAuth b => OAuthMap -> Initializer b v ()
addDefaultRouters (OAuthMap maps) = addRoutes $ concat
                         [ r | (k, r) <- defaultRoutes, k `member` maps ]

defaultRoutes :: HasOAuth b => [(OAuthKey, [(ByteString, Handler b v ())])]
defaultRoutes = [ (weibo, W.routes)
                , (google, G.routes)
                , (github, GH.routes)
                ]
