{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Utils where

import           Control.Applicative
import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types             as HT
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Snap                           hiding (Response)
import qualified Text.Show.ByteString           as TSB

----------------------------------------------------------------------

intToByteString :: Integer -> BS.ByteString
intToByteString = toStrickBS' . TSB.show

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . toStrickBS'

textToBS :: T.Text -> BS.ByteString
textToBS = T.encodeUtf8

decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p

----------------------------------------------------------------------

apiRequestOAuth :: FromJSON a
              => BS.ByteString     -- ^ API URL
              -> OAuth2            -- ^ For append access token
              -> IO (Maybe a)
apiRequestOAuth uri oa = do
    let url = BS8.unpack $ appendAccessToken uri oa
    res <- doSimpleGetRequest url
    str <- handleResponse res
    return $ decode str

apiRequest :: FromJSON a
              => BS.ByteString     -- ^ Full API URL
              -> IO (Maybe a)
apiRequest uri = do
    res <- doSimpleGetRequest (BS8.unpack uri)
    str <- handleResponse res
    return $ decode str

handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then do
                          let body = responseBody rsp
                          return body
                     else throwIO . OAuthException
                          $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)
