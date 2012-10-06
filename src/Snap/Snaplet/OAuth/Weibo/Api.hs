{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.OAuth.Weibo.Api where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (mzero)
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Maybe                     (fromJust)
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


----------------------------------------------------------------------
--  API Impl
----------------------------------------------------------------------

-- | User ID
--
requestUid :: OAuth2 -> IO (Maybe WeiboUserId)
requestUid oa = doSimpleGetRequest
                (BS8.unpack $ appendAccessToken accountUidUri oa)
                >>= fmap decode . handleResponse


-- | User Info
--
requestAccount :: OAuth2 -> WeiboUserId -> IO BSL.ByteString
requestAccount oa uid =
    doSimpleGetRequest uri >>= handleResponse
    where uri = BS8.unpack query
          token = fromJust $ oauthAccessToken oa
          query = HT.renderSimpleQuery True params
          params = accessTokenToParam token ++ uidToParam uid

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



