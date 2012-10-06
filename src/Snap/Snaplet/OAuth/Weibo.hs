{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Weibo
       ( routes
       , userIdH
       , weiboCallbackH
       , accountShowH         
       , module Snap.Snaplet.OAuth.Weibo.Api
       , module Snap.Snaplet.OAuth.Weibo.Key
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString              (ByteString)
import           Data.Maybe
import           Prelude                      hiding ((.))
import           Snap
import Network.OAuth2.OAuth2

import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types
import           Snap.Snaplet.OAuth.Utils
import           Snap.Snaplet.OAuth.Weibo.Api
import           Snap.Snaplet.OAuth.Weibo.Key

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

loginWithWeiboH :: HasOauth b => Handler b v ()
loginWithWeiboH = loginWithOauthH weiboKey Nothing


weiboCallbackH :: HasOauth b => Handler b v OAuth2
weiboCallbackH = oauthCallbackH weiboKey

userIdH :: HasOauth b => OAuth2 -> Handler b v (Maybe WeiboUserId)
userIdH = liftIO . requestUid

-- | Show Account detail info.
--   TODO: to be JSON object
--
accountShowH :: HasOauth b => OAuth2 -> Handler b v ()
accountShowH oauth = do
  maybeUID <- userIdH oauth
  case maybeUID of
    Just uid  ->  modifyResponse (setContentType "application/json")
                  >> liftIO (requestAccount oauth uid)
                  >>= (writeText . lbsToText)
    _         ->  writeBS "Failed at getting UID."


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/weibo"        , loginWithWeiboH)
          ]


----------------------------------------------------------------------------
