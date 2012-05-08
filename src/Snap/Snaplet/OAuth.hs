{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Snap.Snaplet.OAuth where

import Control.Applicative
import Data.Lens.Common
import Data.Maybe
import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import qualified Data.ByteString as BS

import Snap

-------------------------------------------------------
-- TODO
--
-- 1. could be multiple oauth impl in one app.


-------------------------------------------------------

-- | FIXME: This is very confusing because it seems transform OAuth2 to OAuth1.
--           Give me good name.
-- 
data OAuth = OAuth { getOauth :: OAuth2             -- ^ This is major oauth related data
                   , getCodeParam :: BS.ByteString  -- ^ query param that oauth provider will use at callback url.
                   } deriving (Show)                -- ^ e.g. localhost/oauthCallback?code=123, this use 'code' here.
                   

class HasOauth b where
  oauthLens :: Lens (Snaplet b) (Snaplet OAuth)

-------------------------------------------------------

-- | Init this OAuth snaplet.
-- 
initOauthSnaplet :: OAuth2 -> BS.ByteString -> SnapletInit b OAuth
initOauthSnaplet oauth param
  = makeSnaplet "OAuth" "" Nothing $
        if (isOauthDataInit oauth) -- && (not . BS.null param)
        then return $ OAuth oauth param
        else fail "OAuth is not initlized correctly. Please check."

isOauthDataInit :: OAuth2 -> Bool
isOauthDataInit o = foldr (\ f b -> (not . BS.null $ f o) && b) True [ oauthClientId, oauthClientSecret , 
                                                     oauthOAuthorizeEndpoint , 
                                                     oauthAccessTokenEndpoint ]

-------------------------------------------------------

-- | Login via Weibo. Redirect user for authorization.
-- 
loginWithOauth :: HasOauth b => Handler b c ()
loginWithOauth = do
    oauth <- withTop' oauthLens (gets getOauth)      -- ^ FIXME: why withTop', maybe not??
    redirect $ authorizationUrl oauth

-- | Callback for oauth provider.
-- 
-- FIXME: take a Maybe parameter, either redirect to home page or defined url.
-- 
oauthCallbackHandler :: HasOauth b => Handler b c ()
oauthCallbackHandler = do
    oauth <- withTop' oauthLens (gets getOauth)      -- ^ FIXME: why withTop', maybe not??
    param <- withTop' oauthLens (gets getCodeParam)      -- ^ FIXME: why withTop', maybe not??
    code   <- decodedParam' param
    token <- liftIO $ requestAccessToken oauth code
    case token of 
        Just t -> do
            --modifySnapletState (modL oauthLens)
            writeBS $ accessToken t
                   -- FIXME: update oauth with this token
        _ -> writeBS "Error getting access token"


decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p

-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------