{-# LANGUAGE OverloadedStrings #-}

module OAuthHandlers
       ( routes ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString                     (ByteString)
import           Data.Maybe
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.OAuth
import           Text.Templating.Heist


import qualified Snap.Snaplet.OAuth.Google           as G
import           Snap.Snaplet.OAuth.Handlers
import qualified Snap.Snaplet.OAuth.Weibo            as W

import           Application


----------------------------------------------------------------------
--  Google
----------------------------------------------------------------------

-- | Logs out and redirects the user to the site index.
weiboOauthCallbackH :: AppHandler ()
weiboOauthCallbackH = W.weiboCallbackH
                      >>= W.userIdH
                      >>= getUserId
                      >>= (with auth . createOAuthUser)
                      >> redirect "/"

-- | This function is broken since we didnt store token anywhere.
--
testUidH :: AppHandler ()
testUidH = W.userIdH W.weiboKey >>= writeText . T.pack . show

getUserId :: Maybe W.WeiboUserId -> AppHandler T.Text
getUserId Nothing = return "Weibo User ID: Nothing Found" -- FIXME: Exception
getUserId (Just uid) = return . T.pack . show . W.weiboUserId $ uid

showUserId :: Maybe W.WeiboUserId -> AppHandler ()
showUserId uid = getUserId uid >>= writeText

-- | Create new user for Weibo User to local
--
createOAuthUser :: T.Text      -- ^ oauth user id
                   -> Handler App (AuthManager App) ()
createOAuthUser name = do
    let name' = textToBS name
        passwd = ClearText name'
    exists <- usernameExists name
    unless exists (void (createUser name name'))
    res <- loginByUsername name' passwd False
    either (liftIO . print) (const $ return ()) res

----------------------------------------------------------------------
--  Google
----------------------------------------------------------------------

googleOauthCallbackH :: AppHandler ()
googleOauthCallbackH = G.googleCallbackH
                 >>= G.userInfoH
                 >>= googleUserId

googleUserId :: Maybe G.GoogleUser -> AppHandler ()
googleUserId Nothing = toHome
googleUserId (Just user) = with auth (createOAuthUser (G.gid user))
                           >> toHome

----------------------------------------------------------------------
--  Routes
----------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/oauthCallback", weiboOauthCallbackH)
         , ("/googleCallback", googleOauthCallbackH)
         ]

toHome = redirect "/"

----------------------------------------------------------------------
--
----------------------------------------------------------------------

textToBS :: T.Text -> ByteString
textToBS = T.encodeUtf8

