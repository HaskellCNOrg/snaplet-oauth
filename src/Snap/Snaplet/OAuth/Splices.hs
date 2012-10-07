{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.OAuth.Splices where

import           Snap.Snaplet.OAuth.Utils
import           Text.Templating.Heist

----------------------------------------------------------------------
--  Splices
----------------------------------------------------------------------


rawResponseSplices :: (Monad m, Show a) => a -> Splice m
rawResponseSplices = textSplice . sToText

bindRawResponseSplices :: (Monad m, Show a)
                          => a
                          -> HeistState m
                          -> HeistState m
bindRawResponseSplices value = bindSplice "rawResponseSplices" (rawResponseSplices value)
