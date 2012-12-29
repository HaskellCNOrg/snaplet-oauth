{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Weibo
       ( routes
       , loginWithWeiboH
       , weiboCallbackH
       , userIdH
       , accountShowH
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

loginWithWeiboH :: HasOAuth b => Handler b v ()
loginWithWeiboH = loginWithOauthH weibo Nothing


-- | token access callback.
--   return a @OAuthValue@ having access token has been filled.
--
weiboCallbackH :: HasOAuth b => Handler b v OAuthValue
weiboCallbackH = oauthCallbackH weibo

-- | userID is must for access other datas.
--
userIdH :: HasOAuth b => OAuthValue -> Handler b v (Maybe WeiboUserId)
userIdH = liftIO . requestUid

-- | Show Account detail info.
--
accountShowH :: HasOAuth b
                => (Maybe WeiboUser -> Handler b v ())
                -> OAuthValue
                -> Handler b v ()
accountShowH fn oauth =
    userIdH oauth >>= maybe failure success
    where success uid = liftIO (requestAccount oauth uid) >>= fn
          failure = writeBS "Failed at getting UID."


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/weibo" , loginWithWeiboH)
          ]


----------------------------------------------------------------------------
