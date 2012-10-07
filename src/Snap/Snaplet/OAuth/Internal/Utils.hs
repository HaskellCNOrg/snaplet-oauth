{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Internal.Utils where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Snap                           hiding (Response)
import qualified Text.Show.ByteString           as TSB

----------------------------------------------------------------------

intToByteString :: Integer -> BS.ByteString
intToByteString = toStrickBS' . TSB.show

sToText :: Show s => s -> T.Text
sToText = T.pack . show

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
apiRequestOAuth uri oa = apiRequest $ appendAccessToken uri oa

apiRequest :: FromJSON a
              => BS.ByteString     -- ^ Full API URL
              -> IO (Maybe a)
apiRequest = doJSONGetRequest . BS8.unpack
