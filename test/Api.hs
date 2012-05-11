{-# LANGUAGE OverloadedStrings #-}


module Api where

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


import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

import Utils

accountUidUri :: BS.ByteString
accountUidUri = pack' "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = pack' "https://api.weibo.com/2/users/show.json"

---------------------------------------------------------------



-- | UID
data WeiboUserId = WeiboUserId { weiboUserId :: Int } deriving (Show)

instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero

---------------------------------------------------------------

-- | Fetch UID
-- 
requestUid :: OAuth2
           -> IO (Maybe WeiboUserId)
requestUid oa = decode <$> requestUid' accountUidUri oa

requestUid' :: URI 
           -> OAuth2
           -> IO BSL.ByteString
requestUid' uri oa = doSimpleGetRequest (BS8.unpack $ appendAccessToken uri oa) >>= handleResponse

---------------------------------------------------------------

-- | Fetch User information
requestAccount :: OAuth2 -> WeiboUserId -> IO BSL.ByteString
requestAccount oa uid = doSimpleGetRequest (BS8.unpack $ apiUrlGet2 accountShowUri atid) >>= handleResponse
                        where atid = (fromJust $ oauthAccessToken oa, uid)


---------------------------------------------------------------

apiUrlGet2 :: URI                        -- ^ Base URI
          -> (BS.ByteString, WeiboUserId)  -- ^ Authorized Access Token and UID
          -> URI                         -- ^ Combined Result
apiUrlGet2 uri (token, uid) = uri `BS.append` (renderSimpleQuery True $ 
                                               (accessTokenToParam token ++ uidToParam uid))


handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                   then return $ responseBody rsp
                   else throwIO . OAuthException $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)

uidToParam :: WeiboUserId -> [(BS.ByteString, BS.ByteString)]
uidToParam (WeiboUserId uid) = [("uid", intToByteString uid)]


