{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?), (@?=))

import Weibo.Types

main :: IO ()
main = testSuits

testSuits :: IO ()
testSuits = defaultMain
            [ uidTests
            ]

uidTests :: Test
uidTests = testGroup "uid test cases" 
                     [ testCase "uid shall be 12345" $ getUid @?= (Just aOid)
                     , testCase "uid shall be any number" $ isJust getUid @? "uid any number test"
                     , testCase "uid is not string" $ getInvalidUid @?= Nothing
                     ]

------------------------------------------------

invalidUidString :: BSL.ByteString
invalidUidString = "{\"uid\" : \"12345\" }"

getInvalidUid :: Maybe WeiboUserId
getInvalidUid = decode invalidUidString

------------------------------------------------

aOid :: WeiboUserId
aOid = WeiboUserId 12345

uidString :: BSL.ByteString
uidString = "{\"uid\" : 12345 }"

getUid :: Maybe WeiboUserId
getUid = decode uidString
