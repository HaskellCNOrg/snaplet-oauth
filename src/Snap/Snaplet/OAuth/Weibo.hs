{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.OAuth.Weibo
       ( routes
       , userIdH
       , weiboCallbackH
       , module Snap.Snaplet.OAuth.Weibo.Api
       , module Snap.Snaplet.OAuth.Weibo.Key
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString              (ByteString)
import           Data.Maybe
import qualified Data.Text                    as T
import           Prelude                      hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Handlers
import           Snap.Snaplet.OAuth.Types
import           Snap.Snaplet.OAuth.Utils
import           Snap.Snaplet.OAuth.Weibo.Api
import           Snap.Snaplet.OAuth.Weibo.Key

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

loginWithWeiboH :: HasOauth b => Handler b v ()
loginWithWeiboH = loginWithOauth Nothing


weiboCallbackH :: HasOauth b => Handler b v ()
weiboCallbackH = oauthCallbackHandler

userIdH :: HasOauth b => Handler b v (Maybe WeiboUserId)
userIdH = readOAuthMVar >>= liftIO . requestUid

-- | Show Account detail info.
--   TODO: to be JSON object
--
accountShowH :: HasOauth b => Handler b v ()
accountShowH = do
  oauth <- readOAuthMVar
  maybeUID <- userIdH
  case maybeUID of
    Just uid  ->  modifyResponse (setContentType "application/json")
                  >> liftIO (requestAccount oauth uid)
                  >>= (writeText . lbsToText)
    _         ->  writeBS "Failed at getting UID."


----------------------------------------------------------------------------


-- | Redirect to login page if not login yet.
--
-- FIXME: better to notice user that redirect to login via weibo.
--
-- checkLogin :: HasOauth b => OAuth2 -> Handler b v ()
-- checkLogin oa = when (isNothing $ oauthAccessToken oa) $ redirect "weibo"


showOAuthDataH :: HasOauth b => Handler b v ()
showOAuthDataH = readOAuthMVar >>= writeText . T.pack . show


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOauth b => [(ByteString, Handler b v ())]
routes  = [ ("/weibo"        , loginWithWeiboH)
          , ("/weibo/account"  , accountShowH)
          , ("/weibo/oauthCallback", weiboCallbackH)
          , ("/test"         , showOAuthDataH)
          ]
