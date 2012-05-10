{-# LANGUAGE TemplateHaskell #-}

import Control.Category
import Prelude hiding ((.))
import           Snap

data Quux = Quux

data Foo = Foo { _quux :: Quux }

makeLenses [''Foo]

--appQuuxLens :: Lens Foo Quux
--appQuuxLens = quux . snapletValue . Foo
