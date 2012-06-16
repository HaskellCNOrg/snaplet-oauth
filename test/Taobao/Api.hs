{-# LANGUAGE OverloadedStrings #-}


module Taobao.Api where

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

import Taobao.Types
import Utils


---------------------------------------------------------------

-- | User Info
-- 

withOAuth2 :: OAuth2 -> (OAuth2 -> IO BSL.ByteString) -> IO BSL.ByteString
withOAuth2 oa doAPI = case oauthAccessToken oa of
                      Nothing -> return "No AccessToken, login first."
                      Just _ -> doAPI oa


taobaoUserGet :: OAuth2 -> IO BSL.ByteString
taobaoUserGet oa = withOAuth2 oa action
                   where action oa' = do
                                          print uri
                                          doSimpleGetRequest uri >>= handleResponse
                                          where uri = BS8.unpack $ paramToQueryString oa' taobaoUserGetP


---------------------------------------------------------------



handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then return $ responseBody rsp
                     else throwIO . OAuthException $ "Gaining api data failed: " ++ BSL.unpack (responseBody rsp)

