{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module Snap.Snaplet.OAuth where

import           Control.Applicative
import           Control.Category
import           Control.Concurrent.MVar
import           Data.Lens.Common
import           Data.Maybe
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Prelude hiding ((.))

import qualified Data.ByteString as BS

import Snap

-------------------------------------------------------
-- TODO
--
-- 1. Shoule be able to do
--     - allow multiple user login with various OAuth provider, e.g. Weibo/Google


-------------------------------------------------------

-- |
--  FIXME: further, the OAuth2 should be a Map since accessToken is vary among users.
-- 
data OAuthSnaplet = OAuthSnaplet 
                    { getOauth     :: MVar OAuth2             -- ^ This is major oauth related data
                    , getCodeParam :: BS.ByteString  -- ^ query param that oauth provider will use at callback url.
                    }                                -- ^ e.g. localhost/oauthCallback?code=123, so pick up 'code'.
                   
-- | TODO: just define `getOauthSnaplet` without oauthLens
-- 
-- | Some snaplet implementation just define a get function is because is read only snaplet.
--   Therefore, the get function could be like 
--   `getXXX = with oauth Snap.get`
--   where the `oauth` here can be found at `data App = App { _oauth : xxxx, ....}
-- 
class HasOauth b where
  oauthLens :: Lens b (Snaplet OAuthSnaplet)
  
  oauthLens' :: Lens (Snaplet b) (Snaplet OAuthSnaplet)
  oauthLens' = subSnaplet oauthLens

  
  --updateOAuthSnaplet :: (MonadSnaplet m) => m b OAuthSnaplet a -> m b b a
  --updateOAuthSnaplet = with oauthLens

getOauthSnaplet :: HasOauth b => Handler b b OAuthSnaplet
getOauthSnaplet = with oauthLens Snap.get

readOAuthMVar' :: HasOauth b => OAuthSnaplet -> Handler b b OAuth2
readOAuthMVar' = liftIO . readMVar . getOauth

readOAuthMVar :: HasOauth b => Handler b b OAuth2
readOAuthMVar = getOauthSnaplet >>= readOAuthMVar'

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
-- 
initOauthSnaplet :: OAuth2 -> Maybe BS.ByteString -> SnapletInit b OAuthSnaplet
initOauthSnaplet oauth param
  = makeSnaplet "OAuthSnaplet" "" Nothing $
        if (isOauthDataInit oauth) then do
            mo <- liftIO $ newMVar oauth
            return $ OAuthSnaplet mo (defaultParam param)
        else fail "OAuthSnaplet is not initlized correctly. Please check."
    where 
      defaultParam :: Maybe BS.ByteString -> BS.ByteString
      defaultParam Nothing   = "code"
      defaultParam (Just "") = "code"
      defaultParam (Just x)  = x

isOauthDataInit :: OAuth2 -> Bool
isOauthDataInit o = foldr (\ f b -> (not . BS.null $ f o) && b) True [ oauthClientId, 
                                                                       oauthClientSecret , 
                                                                       oauthOAuthorizeEndpoint , 
                                                                       oauthAccessTokenEndpoint ]

-------------------------------------------------------
-- Handlers


-- | Login via OAuth. Redirect user for authorization.
-- 
loginWithOauth :: HasOauth b 
               => Maybe BS.ByteString  -- ^ Maybe extra query parameters,e.g., 'scope' param for google oauth.
               -> Handler b b ()
loginWithOauth param = do
    oauth <- readOAuthMVar 
    redirect $ (authorizationUrl oauth ) `BS.append` extraP param
    where extraP (Just x) = "&" `BS.append` x
          extraP Nothing  = ""


-- | Callback for oauth provider.
-- 
oauthCallbackHandler :: HasOauth b 
                     => Maybe BS.ByteString   -- ^ redirect to when successfully. default to "/"
                     -> Handler b b ()
oauthCallbackHandler uri = do
    oauthSnaplet <- getOauthSnaplet
    codeParam    <- decodedParam' (getCodeParam oauthSnaplet)
    oauth        <- readOAuthMVar' oauthSnaplet
    liftIO $ print codeParam
    maybeToken   <- liftIO $ requestAccessToken oauth codeParam
    liftIO $ print maybeToken
    case maybeToken of 
        Just token -> do
             liftIO $ modifyOAuthState token oauthSnaplet
             redirect $ fromMaybe "/" uri 
        _ -> writeBS "Error getting access token."

-------------------------------------------------------


modifyOAuthState :: AccessToken -> OAuthSnaplet -> IO ()
modifyOAuthState at os = modifyMVar_ (getOauth os) (modifyAccessToken at)

modifyAccessToken :: AccessToken -> OAuth2 -> IO OAuth2
modifyAccessToken (AccessToken at _) origin = return $ origin { oauthAccessToken = Just at }


decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p


-------------------------------------------------------

-- | Update AccessToken after fetched.
--modifyOAuthState' :: AccessToken -> OAuthSnaplet -> OAuthSnaplet
--modifyOAuthState' (AccessToken at) oa = OAuthSnaplet { getOauth = newOA, getCodeParam = getCodeParam oa }
--                                       where newOA = originOA { oauthAccessToken = Just at }
--                                             originOA = getOauth oa

--modify2 token
--modify2 :: (MonadIO m, MonadState b m, HasOauth b) => AccessToken -> m ()
--modify2 token = modify (modL (snapletValue . oauthLens') (modifyOAuthState token))
