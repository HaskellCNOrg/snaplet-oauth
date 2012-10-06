{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Weibo where

import           Data.Aeson                     (decode)
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Maybe                     (isJust, fromJust)
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?), (@?=))

import           Snap.Snaplet.OAuth.Weibo


uidTests :: Test
uidTests = testGroup "uid test cases"
                     [ testCase "uid shall be 12345" $ getUid @?= (Just aOid)
                     , testCase "uid shall be 2709495807" $ getUid2 @?= (Just aOid2)
                     , testCase "uid shall be any number" $ isJust getUid @? "uid any number test"
                     , testCase "uid is not string" $ getInvalidUid @?= Nothing
                     , testCase "uidstr is string" $ getIdStr @?= "1814581760"
                     , testCase "uidstr is string" $ getNameStr @?= "HaishengWoo"
                     ]

------------------------------------------------

invalidUidString :: BSL.ByteString
invalidUidString = "{\"uid\" : \"12345\" }"

getInvalidUid :: Maybe WeiboUserId
getInvalidUid = decode invalidUidString

------------------------------------------------

aOid :: WeiboUserId
aOid = WeiboUserId 12345

getUid :: Maybe WeiboUserId
getUid = decode ("{\"uid\" : 12345 }" :: BSL.ByteString)


aOid2 = WeiboUserId 2709495807123
getUid2 = decode ("{\"uid\" : 2709495807123 }" :: BSL.ByteString)

------------------------------------------------
getIdStr = wUidStr $ fromJust userInfo
getNameStr = wName $ fromJust userInfo

userInfo :: Maybe WeiboUser
userInfo = decode userInfoStr

userInfoStr :: BSL.ByteString
userInfoStr = BSL.pack $  "{\"id\": 1814581760, \"idstr\": \"1814581760\", \"screen_name\": \"HaishengWoo\", \"name\": \"HaishengWoo\", \"url\": \"http://freizl.github.com/\" }"
