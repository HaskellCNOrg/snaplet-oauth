{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Handlers where

import           Control.Applicative
import           Control.Concurrent.MVar
import qualified Data.ByteString                as BS
import           Data.Maybe
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Prelude                        hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Types

-------------------------------------------------------
-- Handlers

-- | Login via OAuth. Redirect user for authorization.
--
loginWithOauth :: HasOauth b
               => Maybe BS.ByteString
               -- ^ Maybe extra query parameters,e.g., 'scope' param for google oauth.
               -> Handler b v ()
loginWithOauth param = do
    oauth <- readOAuthMVar
    redirect $ authorizationUrl oauth `BS.append` extraP param
    where extraP (Just x) = "&" `BS.append` x
          extraP Nothing  = ""


-- | Callback for oauth provider.
--
oauthCallbackHandler :: HasOauth b
                     => Handler b v ()
oauthCallbackHandler = do
    oauthSnaplet <- getOauthSnaplet
    codeParam    <- decodedParam' (getCodeParam oauthSnaplet)
    oauth        <- readOAuthMVar' oauthSnaplet
    maybeToken   <- liftIO $ requestAccessToken oauth codeParam
    case maybeToken of
        Just token -> liftIO $ modifyOAuthState token oauthSnaplet
        _ -> writeBS "Error getting access token."

-- |
defaultOAuthCallbackHandler :: HasOauth b => Handler b v ()
defaultOAuthCallbackHandler = oauthCallbackHandler >> redirect "/"

-------------------------------------------------------


modifyOAuthState :: AccessToken -> OAuthSnaplet -> IO ()
modifyOAuthState at os = modifyMVar_ (getOauth os) (modifyAccessToken at)

modifyAccessToken :: AccessToken -> OAuth2 -> IO OAuth2
modifyAccessToken (AccessToken at) origin = return $ origin { oauthAccessToken = Just at }


decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p

-------------------------------------------------------
