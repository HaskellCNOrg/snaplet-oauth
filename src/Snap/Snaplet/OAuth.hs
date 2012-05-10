{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module Snap.Snaplet.OAuth where

import Control.Applicative
import Data.Lens.Common
import Data.Maybe
import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Prelude hiding ((.))
import           Control.Category

import Snap

-------------------------------------------------------
-- TODO
--
-- 1. could be multiple oauth impl in one app.


-------------------------------------------------------

-- 
data OAuthSnaplet = OAuthSnaplet 
                    { getOauth :: OAuth2             -- ^ This is major oauth related data
                    , getCodeParam :: BS.ByteString  -- ^ query param that oauth provider will use at callback url.
                    } deriving (Show)                -- ^ e.g. localhost/oauthCallback?code=123, so pick up 'code'.
                   
-- | TODO: just define `getOauthSnaplet` without oauthLens
-- 
-- | Some snaplet implementation just define a get function is because is read only snaplet.
--   Therefore, the get function could be like 
--   `getXXX = with oauth Snap.get`
--   where the `oauth` here can be found at `data App = App { _oauth : xxxx, ....}
-- 
class HasOauth b where
  oauthLens' :: Lens b (Snaplet OAuthSnaplet)
  oauthLens :: Lens (Snaplet b) (Snaplet OAuthSnaplet)
  
  getOauthSnaplet :: Handler b b OAuthSnaplet
  getOauthSnaplet = with' oauthLens Snap.get

  updateOAuthSnaplet :: (MonadSnaplet m) => m b OAuthSnaplet a -> m b b a
  updateOAuthSnaplet = with' oauthLens

-------------------------------------------------------

-- | Init this OAuthSnaplet snaplet.
-- 
initOauthSnaplet :: OAuth2 -> BS.ByteString -> SnapletInit b OAuthSnaplet
initOauthSnaplet oauth param
  = makeSnaplet "OAuthSnaplet" "" Nothing $
        if (isOauthDataInit oauth)                   -- FIXME: && (not . BS.null param)
        then return $ OAuthSnaplet oauth param
        else fail "OAuthSnaplet is not initlized correctly. Please check."

isOauthDataInit :: OAuth2 -> Bool
isOauthDataInit o = foldr (\ f b -> (not . BS.null $ f o) && b) True [ oauthClientId, oauthClientSecret , 
                                                     oauthOAuthorizeEndpoint , 
                                                     oauthAccessTokenEndpoint ]

-------------------------------------------------------
-- Handlers


-- | Login via OAuth. Redirect user for authorization.
-- 
loginWithOauth :: HasOauth b => Handler b b ()
loginWithOauth = do
    oauthSnaplet <- getOauthSnaplet
    redirect $ authorizationUrl $ getOauth oauthSnaplet


-- | Callback for oauth provider.
-- 
oauthCallbackHandler :: HasOauth b 
                     => Maybe BS.ByteString   -- ^ redirect to when successfully. default to "/"
                     -> Handler b b ()
oauthCallbackHandler uri = do
    oauthSnaplet <- getOauthSnaplet
    code         <- decodedParam' (getCodeParam oauthSnaplet)
    maybeToken   <- liftIO $ requestAccessToken (getOauth oauthSnaplet) code
    liftIO $ print uri
    case maybeToken of 
        Just token -> do
             updateOAuthSnaplet (modify $ modifyOAuthState token)
             ss <- getOauthSnaplet
             writeBS $ BS8.pack (show $ getOauth ss)
             -- redirect $ fromMaybe "/" uri 
        _ -> writeBS "Error getting access token."

--modify2 token
modify2 :: (MonadIO m, MonadState b m, HasOauth b) => AccessToken -> m ()
modify2 token = modify (modL (snapletValue . oauthLens') (modifyOAuthState token))

-------------------------------------------------------

-- | Update AccessToken after fetched.
modifyOAuthState :: AccessToken -> OAuthSnaplet -> OAuthSnaplet
modifyOAuthState (AccessToken at) oa = OAuthSnaplet { getOauth = newOA, getCodeParam = getCodeParam oa }
                                       where newOA = originOA { oauthAccessToken = Just at }
                                             originOA = getOauth oa

decodedParam' :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam' p = fromMaybe "" <$> getParam p


-------------------------------------------------------
