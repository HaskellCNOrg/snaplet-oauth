{-# LANGUAGE TemplateHaskell #-}

import Control.Category
import Prelude hiding ((.))
import           Snap hiding (Response)
import Network.HTTP.Conduit
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as BSL


data Quux = Quux

data Foo = Foo { _quux :: Quux }

makeLenses [''Foo]

--appQuuxLens :: Lens Foo Quux
--appQuuxLens = quux . snapletValue . Foo

