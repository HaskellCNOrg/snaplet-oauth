{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth
       ( initOauthSnaplet ) where

import           Control.Concurrent.MVar
import qualified Data.ByteString          as BS
import           Data.Maybe
import           Prelude                  hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Types
import qualified Snap.Snaplet.OAuth.Weibo as W
import qualified Snap.Snaplet.OAuth.Google as G

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
--
initOauthSnaplet :: HasOauth b => SnapletInit b OAuthSnaplet
initOauthSnaplet = makeSnaplet "OAuthSnaplet" "" Nothing $ do
        --let oauth = W.weiboKey
        let oauth = G.googleKey
        mo <- liftIO $ newMVar oauth
        addRoutes routes
        return $ emptyOAuthSnaplet { getOauth = mo }


routes :: HasOauth b => [(BS.ByteString, Handler b v ())]
routes = W.routes
         <|>
         G.routes
