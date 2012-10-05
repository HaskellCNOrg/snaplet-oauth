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

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
--
initOauthSnaplet :: HasOauth b => SnapletInit b OAuthSnaplet
initOauthSnaplet
  = makeSnaplet "OAuthSnaplet" "" Nothing $ do
        let oauth = W.weiboKey
        mo <- liftIO $ newMVar oauth
        addRoutes routes
        return $ emptyOAuthSnaplet { getOauth = mo }


routes :: HasOauth b => [(BS.ByteString, Handler b v ())]
routes = W.routes


--        return $ OAuthSnaplet mo (defaultParam param)
        
        --if (isOauthDataInit oauth) then do
        --    mo <- liftIO $ newMVar oauth
        --    addRoutes routes
        --    return $ OAuthSnaplet mo (defaultParam param)
        --else fail "OAuthSnaplet is not initlized correctly. Please check."
  --  where
  --    defaultParam :: Maybe BS.ByteString -> BS.ByteString
  --    defaultParam Nothing   = "code"
  --    defaultParam (Just "") = "code"
  --    defaultParam (Just x)  = x

-- isOauthDataInit :: OAuth2 -> Bool
-- isOauthDataInit o = foldr (\ f b -> (not . BS.null $ f o) && b) True [ oauthClientId,
--                                                                        oauthClientSecret ,
--                                                                        oauthOAuthorizeEndpoint ,
--                                                                        oauthAccessTokenEndpoint ]

