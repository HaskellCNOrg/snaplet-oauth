{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Applicative ((<$>))
import           Control.Monad       (mzero)
import           Data.Aeson

---------------------------------------------------------------

-- | UID data type
data WeiboUserId = WeiboUserId { weiboUserId :: Int } deriving (Show, Eq)

instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero
