{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.OAuth.Weibo.Api where

import           Control.Applicative            ((<$>))
import           Control.Exception
import           Control.Monad                  (mzero)
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Maybe                     (fromJust)
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types             as HT
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2

import           Snap.Snaplet.OAuth.Utils

----------------------------------------------------------------------
--  Weibo User ID
----------------------------------------------------------------------

-- | UID data type
data WeiboUserId = WeiboUserId { weiboUserId :: Integer } deriving (Show, Eq)

-- | Parse UID response
--
instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero


-- | Append uid for other API request
--
uidToParam :: WeiboUserId -> [(BS.ByteString, BS.ByteString)]
uidToParam (WeiboUserId uid) = [("uid", intToByteString uid)]


----------------------------------------------------------------------
--  API URI
----------------------------------------------------------------------

accountUidUri :: BS.ByteString
accountUidUri = sToBS "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = sToBS "https://api.weibo.com/2/users/show.json"

accountStatusUpdate :: String
accountStatusUpdate ="https://api.weibo.com/2/statuses/update.json"



----------------------------------------------------------------------
--  API
----------------------------------------------------------------------

-- | User ID
--
requestUid :: OAuth2 -> IO (Maybe WeiboUserId)
requestUid oa = decode <$> requestUid' accountUidUri oa

requestUid' :: URI -> OAuth2 -> IO BSL.ByteString
requestUid' uri oa = doSimpleGetRequest (BS8.unpack $ appendAccessToken uri oa)
                     >>= handleResponse


-- | User Info
--
requestAccount :: OAuth2 -> WeiboUserId -> IO BSL.ByteString
requestAccount oa uid = do
    doSimpleGetRequest uri >>= handleResponse
    where uri = (BS8.unpack $ apiUrlGet2 accountShowUri atid)
          atid = (fromJust $ oauthAccessToken oa, uid)



-- | Perform HTTP request
--
apiUrlGet2 :: URI                         -- ^ Base URI
          -> (BS.ByteString, WeiboUserId) -- ^ Authorized Access Token and UID
          -> URI                          -- ^ Combined Result
apiUrlGet2 uri (token, uid) = uri
                              `BS.append`
                              HT.renderSimpleQuery True (accessTokenToParam token ++ uidToParam uid)



handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then do
                          let body = responseBody rsp
                          return body
                     else throwIO . OAuthException
                          $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)

