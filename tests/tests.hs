{-# LANGUAGE OverloadedStrings #-}

import           Test.Framework (defaultMain)

import qualified Weibo          as W

main :: IO ()
main = testSuits

testSuits :: IO ()
testSuits = defaultMain
            [ W.uidTests
            ]
