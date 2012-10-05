
{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.OAuth.Weibo.Types where

import           Control.Applicative      ((<$>))
import           Control.Monad            (mzero)
import           Data.Aeson
import qualified Data.ByteString          as BS

import           Snap.Snaplet.OAuth.Utils

---------------------------------------------------------------


accountUidUri :: BS.ByteString
accountUidUri = sToBS "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = sToBS "https://api.weibo.com/2/users/show.json"

accountStatusUpdate :: String
accountStatusUpdate ="https://api.weibo.com/2/statuses/update.json"


---------------------------------------------------------------

-- | UID data type
data WeiboUserId = WeiboUserId { weiboUserId :: Integer } deriving (Show, Eq)

instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero
