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

------------------------------------------------------------------------------

import           Application


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
oauthCallbackH :: AppHandler ()
oauthCallbackH = W.weiboCallbackH
                 >> W.userIdH
                 >>= getUserId
                 >>= (with auth . createWeiboUser)
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
createWeiboUser :: T.Text  -- weibo userid
                   -> Handler App (AuthManager App) ()
createWeiboUser name = do
    let name' = textToBS name
        passwd = ClearText name'
    exists <- usernameExists name
    when (not exists) (createUser name name' >> return ())
    res <- loginByUsername name' passwd True
    either (liftIO . print) (const $ return ()) res

textToBS :: T.Text -> ByteString
textToBS = T.encodeUtf8

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/oauthCallback", oauthCallbackH)
         , ("/testuid", testUidH)
         ]
