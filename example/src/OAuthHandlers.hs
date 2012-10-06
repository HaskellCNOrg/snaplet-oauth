{-# LANGUAGE OverloadedStrings #-}

module OAuthHandlers
       ( routes ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding   as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import Snap.Snaplet.OAuth
import           Control.Monad


import Snap.Snaplet.OAuth.Handlers
import qualified Snap.Snaplet.OAuth.Weibo as W
import qualified Snap.Snaplet.OAuth.Google as G

import           Application


----------------------------------------------------------------------
--  Google
----------------------------------------------------------------------

-- | Logs out and redirects the user to the site index.
oauthCallbackH :: AppHandler ()
oauthCallbackH = W.weiboCallbackH
                 >> W.userIdH
                 >>= getUserId
                 >>= (with auth . createOAuthUser)
                 >> redirect "/"

testUidH :: AppHandler ()
testUidH = do
    uid <- W.userIdH
    writeText $ T.pack $ show uid

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
                 >> G.userInfoH
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
routes = [ ("/oauthCallback", oauthCallbackH)
         , ("googleCallback", googleOauthCallbackH)
         , ("/testuid", testUidH)
         ]

toHome = redirect "/"

----------------------------------------------------------------------
--  
----------------------------------------------------------------------

textToBS :: T.Text -> ByteString
textToBS = T.encodeUtf8

