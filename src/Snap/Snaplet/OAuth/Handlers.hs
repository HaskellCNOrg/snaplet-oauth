{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.OAuth.Handlers
       ( loginWithOauthH
       , oauthCallbackH) where

import           Control.Applicative
import qualified Data.ByteString                as BS
import           Data.Maybe
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Prelude                        hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Types

----------------------------------------------------------------------

-- | Login via OAuth. Redirect user for authorization.
--
loginWithOauthH :: HasOauth b
               => OAuth2
               -> Maybe BS.ByteString
               -- ^ Maybe extra query parameters,e.g., 'scope' param for google oauth.
               -> Handler b v ()
loginWithOauthH oauth param = 
    redirect $ authorizationUrl oauth `BS.append` extraP param
    where extraP (Just x) = "&" `BS.append` x
          extraP Nothing  = ""


----------------------------------------------------------------------

-- | Callback for oauth provider.
--
oauthCallbackH :: HasOauth b
                     => OAuth2
                     -> Handler b v OAuth2
oauthCallbackH oauth = do
    codeParam    <- decodedParam' accessTokenKey
    maybeToken   <- liftIO $ requestAccessToken oauth codeParam
    liftIO $ print maybeToken
    case maybeToken of
        Just token -> liftIO $ modifyAccessToken token oauth
        _ -> return oauth -- FIXME: throw exception


modifyAccessToken :: AccessToken -> OAuth2 -> IO OAuth2
modifyAccessToken (AccessToken at) origin = return $ origin { oauthAccessToken = Just at }


decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p

accessTokenKey :: BS.ByteString
accessTokenKey = "code"

----------------------------------------------------------------------

-- checkLogin :: HasOauth b => OAuth2 -> Handler b v ()
-- checkLogin oa = when (isNothing $ oauthAccessToken oa) $ redirect "weibo"
