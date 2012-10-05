{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Utils where

import           Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Snap
import qualified Text.Show.ByteString as TSB

intToByteString :: Int -> BS.ByteString
intToByteString = toStrickBS' . TSB.show

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . toStrickBS'

decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p
