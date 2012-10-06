{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Utils where

import           Control.Applicative
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.OAuth2.OAuth2

import           Control.Exception
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types         as HT
import           Snap                       (MonadSnap, getParam)
import qualified Text.Show.ByteString       as TSB

intToByteString :: Integer -> BS.ByteString
intToByteString = toStrickBS' . TSB.show

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . toStrickBS'

decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p

handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then do
                          let body = responseBody rsp
                          return body
                     else throwIO . OAuthException
                          $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)
