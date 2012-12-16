{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.OAuth.Weibo.Api where

import           Data.Aeson
import qualified Data.ByteString                   as BS
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
--import qualified Network.HTTP.Types                as HT
import           Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import           Snap
import           Snap.Snaplet.OAuth.Internal.Utils
import           Snap.Snaplet.OAuth.Internal.Types

----------------------------------------------------------------------
--  Weibo User ID
----------------------------------------------------------------------

-- | UID data type
--   FIXME: chinese name doest display correctly
data WeiboUserId = WeiboUserId { weiboUserId :: Integer } deriving (Show, Eq)

data WeiboUser = WeiboUser { wUidStr      :: Text
                           , wScreenName  :: Text
                           , wName        :: Text
                           , wUrl         :: Text
                           } deriving (Show)
-- | Parse UID response
--
instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero

instance FromJSON WeiboUser where
    parseJSON (Object o) = WeiboUser
                          <$> o .: "idstr"
                          <*> o .: "screen_name"
                          <*> o .: "name"
                          <*> o .: "url"
    parseJSON _ = mzero


----------------------------------------------------------------------
--  API Impl
----------------------------------------------------------------------

-- | User ID
--
requestUid :: OAuthValue -> IO (Maybe WeiboUserId)
requestUid = apiRequestOAuth accountUidUri


-- | User Info
--
requestAccount :: OAuthValue -> WeiboUserId -> IO (Maybe WeiboUser)
requestAccount oa uid = doJSONGetRequest uri
    where uri = appendQueryParam accountShowUri params
          params = accessTokenToParam token ++ uidToParam uid
          token = fromMaybe "" (oauthAccessToken oa)

uidToParam :: WeiboUserId -> [(BS.ByteString, BS.ByteString)]
uidToParam (WeiboUserId uid) = [("uid", intToByteString uid)]


----------------------------------------------------------------------
--  API URI
----------------------------------------------------------------------

accountUidUri :: BS.ByteString
accountUidUri = sToBS "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = sToBS "https://api.weibo.com/2/users/show.json"

