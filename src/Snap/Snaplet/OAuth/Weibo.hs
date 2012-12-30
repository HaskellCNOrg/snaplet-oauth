{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Weibo
       ( -- * Routes
         routes
         -- * Handlers
       , weiboLoginH
       , weiboCallbackH
       , weiboUserIdH
       , weiboUserH
         -- * Types and API
       , module Snap.Snaplet.OAuth.Weibo.Api
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString                      (ByteString)
import           Data.Maybe
import           Prelude                              hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Internal.Handlers
import           Snap.Snaplet.OAuth.Internal.Types
import           Snap.Snaplet.OAuth.Weibo.Api

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

weiboLoginH :: HasOAuth b => Handler b v ()
weiboLoginH = loginWithOauthH weibo Nothing


-- | token access callback.
--   return a @OAuthValue@ having access token has been filled.
--
weiboCallbackH :: HasOAuth b => Handler b v OAuthValue
weiboCallbackH = oauthCallbackH weibo

-- | userID is must for access other datas.
--
weiboUserIdH :: HasOAuth b => Handler b v (OAuthValue, Maybe WeiboUserId)
weiboUserIdH = do
               oauth <- weiboCallbackH
               uid <- liftIO $ requestUid oauth
               return (oauth, uid)


-- | fetch weibo user info.
--
weiboUserH :: HasOAuth b => Handler b v (Maybe WeiboUser)
weiboUserH = do
  (oauth, uid) <- weiboUserIdH
  maybe failure (liftIO . requestAccount oauth) uid
    where failure = return Nothing


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/weibo" , weiboLoginH)
          ]


----------------------------------------------------------------------------
