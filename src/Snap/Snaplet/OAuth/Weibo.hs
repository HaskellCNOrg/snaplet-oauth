{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Weibo
       ( routes
       , loginWithWeiboH
       , weiboCallbackH
       , userIdH
       , accountShowH
       , module Snap.Snaplet.OAuth.Weibo.Api
       , module Snap.Snaplet.OAuth.Weibo.Key
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString              (ByteString)
import           Data.Maybe
import           Network.OAuth2.OAuth2
import           Prelude                      hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types
import           Snap.Snaplet.OAuth.Weibo.Api
import           Snap.Snaplet.OAuth.Weibo.Key

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

loginWithWeiboH :: HasOauth b => Handler b v ()
loginWithWeiboH = loginWithOauthH weiboKey Nothing

-- | token access callback.
--   return a @OAuth2@ having access token has been filled.
--
weiboCallbackH :: HasOauth b => Handler b v OAuth2
weiboCallbackH = oauthCallbackH weiboKey

-- | userID is must for access other datas.
--
userIdH :: HasOauth b => OAuth2 -> Handler b v (Maybe WeiboUserId)
userIdH = liftIO . requestUid

-- | Show Account detail info.
--
accountShowH :: HasOauth b
                => (Maybe WeiboUser -> Handler b v ())
                -> OAuth2
                -> Handler b v ()
accountShowH fn oauth =
    userIdH oauth >>= maybe failure success
    where success uid = liftIO (requestAccount oauth uid) >>= fn
          failure = writeBS "Failed at getting UID."


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/weibo" , loginWithWeiboH)
          ]


----------------------------------------------------------------------------
