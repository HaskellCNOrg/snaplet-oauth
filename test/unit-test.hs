{-# LANGUAGE TemplateHaskell #-}

import Control.Category
import Prelude hiding ((.))
import           Snap hiding (Response)
import Network.HTTP.Conduit
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson (decode)
import Test.HUnit

import Network.OAuth2.OAuth2
import Utils

data Quux = Quux

data Foo = Foo { _quux :: Quux }

makeLenses [''Foo]

--appQuuxLens :: Lens Foo Quux
--appQuuxLens = quux . snapletValue . Foo


main :: IO ()
main = do
       print prop_getUid
       print prop_getInvalidUid
       print $ intToByteString 1234

invalidUidString :: BSL.ByteString
invalidUidString = "{\"uid\" : \"222222\" }"

prop_getInvalidUid :: Maybe WeiboUserId
prop_getInvalidUid = decode invalidUidString

uidString :: BSL.ByteString
uidString = "{\"uid\" : 222222 }"

prop_getUid :: Maybe WeiboUserId
prop_getUid = decode uidString
