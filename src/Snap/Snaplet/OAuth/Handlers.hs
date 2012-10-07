{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Handlers
       ( loginWithOauthH
       , oauthCallbackH ) where

import           Control.Applicative
import           Control.Monad.CatchIO          (throw)
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
loginWithOauthH :: HasOAuth b
               => OAuthKey
               -> Maybe BS.ByteString
               -- ^ Maybe extra query parameters,e.g., 'scope' param for google oauth.
               -> Handler b v ()
--loginWithOauthH Nothing _ = oauthNotInitH
loginWithOauthH key param = withOAuthH key fn
    where extraP (Just x) = "&" `BS.append` x
          extraP Nothing  = ""
          fn oauth = redirect $ authorizationUrl oauth `BS.append` extraP param

----------------------------------------------------------------------

-- | Callback for oauth provider.
--
oauthCallbackH :: HasOAuth b
                  => OAuthKey
                  -> Handler b v OAuth2
oauthCallbackH key = withOAuthH key fn
    where fn oauth = do
                     codeParam  <- decodedParam' accessTokenKey
                     maybeToken <- liftIO $ requestAccessToken oauth codeParam
                     case maybeToken of
                         Just token -> liftIO $ modifyAccessToken token oauth
                         _ -> throw (OAuthException $ "Failed to request Access Token." ++ show key)
                              >> return oauth


modifyAccessToken :: AccessToken -> OAuth2 -> IO OAuth2
modifyAccessToken (AccessToken at) origin = return $ origin { oauthAccessToken = Just at }


decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p

accessTokenKey :: BS.ByteString
accessTokenKey = "code"

----------------------------------------------------------------------

withOAuthH :: HasOAuth b
              => OAuthKey
              -> (OAuthValue -> Handler b v a)
              -> Handler b v a
withOAuthH key fn = do
    value <- lookupOAuth key
    case value of
      Nothing -> failure
      Just oauth -> fn oauth
    where failure = throw $ OAuthException $ "oauth data has not been init of: " ++ show key


-- checkLogin :: HasOAuth b => OAuth2 -> Handler b v ()
-- checkLogin oa = when (isNothing $ oauthAccessToken oa) $ redirect "weibo"

