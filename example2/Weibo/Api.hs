{-# LANGUAGE OverloadedStrings #-}


module Weibo.Api where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe (fromMaybe, fromJust)
import Data.Typeable (Typeable)
import Network.HTTP.Types (renderSimpleQuery)
import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Types as HT
--import Control.Monad.Trans (liftIO)
--import Control.Monad.IO.Class (MonadIO)

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

import Weibo.Types
import Utils


---------------------------------------------------------------

-- | User ID
-- 
requestUid :: OAuth2
           -> IO (Maybe WeiboUserId)
requestUid oa = decode <$> requestUid' accountUidUri oa

requestUid' :: URI -> OAuth2 -> IO BSL.ByteString
requestUid' uri oa = doSimpleGetRequest (BS8.unpack $ appendAccessToken uri oa) >>= handleResponse


-- | User Info
-- 
requestAccount :: OAuth2 -> WeiboUserId -> IO BSL.ByteString
requestAccount oa uid = do
    print uri
    doSimpleGetRequest uri >>= handleResponse
    where uri = (BS8.unpack $ apiUrlGet2 accountShowUri atid)
          atid = (fromJust $ oauthAccessToken oa, uid)


---------------------------------------------------------------

-- | Post anew
postNew :: BS.ByteString -> OAuth2 -> IO BSL.ByteString
postNew post oa = doPostRequst accountStatusUpdate [packPostContent post, accessTokenToParam' oa] 
                  >>= handleResponse
                        
packPostContent :: BS.ByteString -> (BS.ByteString, BS.ByteString)
packPostContent post = ("status", post)

accessTokenToParam' :: OAuth2 -> (BS.ByteString, BS.ByteString)
accessTokenToParam' oa = ("access_token", token)
                         where token = fromJust $ oauthAccessToken oa


---------------------------------------------------------------

apiUrlGet2 :: URI                         -- ^ Base URI
          -> (BS.ByteString, WeiboUserId) -- ^ Authorized Access Token and UID
          -> URI                          -- ^ Combined Result
apiUrlGet2 uri (token, uid) = uri `BS.append` renderSimpleQuery True (accessTokenToParam token ++ uidToParam uid)


handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then return $ responseBody rsp
                     else throwIO . OAuthException $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)

uidToParam :: WeiboUserId -> [(BS.ByteString, BS.ByteString)]
uidToParam (WeiboUserId uid) = [("uid", intToByteString uid)]
