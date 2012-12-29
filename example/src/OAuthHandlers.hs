{-# LANGUAGE OverloadedStrings #-}

module OAuthHandlers
       ( routes ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as BS
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


import qualified Snap.Snaplet.OAuth.Github           as GH
import qualified Snap.Snaplet.OAuth.Google           as G
import qualified Snap.Snaplet.OAuth.Weibo            as W

import           Application
import           Splices

----------------------------------------------------------------------
--  Weibo
----------------------------------------------------------------------

-- | Logs out and redirects the user to the site index.
weiboOauthCallbackH :: AppHandler ()
weiboOauthCallbackH = W.weiboUserH
                      >>= success
                      where success Nothing = writeBS "No user info found"
                            success (Just usr) = do
                                with auth $ createOAuthUser weibo $ W.wUidStr usr
                                --writeText $ T.pack $ show usr
                                toHome usr


----------------------------------------------------------------------
--  Google
----------------------------------------------------------------------

googleOauthCallbackH :: AppHandler ()
googleOauthCallbackH = G.googleUserH
                 >>= googleUserId

googleUserId :: Maybe G.GoogleUser -> AppHandler ()
googleUserId Nothing = redirect "/"
googleUserId (Just user) = with auth (createOAuthUser google (G.gid user))
                           >> toHome user

----------------------------------------------------------------------
--  Github
----------------------------------------------------------------------

githubOauthCallbackH :: AppHandler ()
githubOauthCallbackH = GH.githubUserH
                 >>= githubUser

githubUser :: Maybe GH.GithubUser -> AppHandler ()
githubUser Nothing = redirect "/"
githubUser (Just user) = with auth (createOAuthUser github uid)
                           >> toHome user
                         where uid = intToText $ GH.gid user
                               intToText = T.pack . show


----------------------------------------------------------------------
--  Create User per oAuth response
----------------------------------------------------------------------

-- | Create new user for Weibo User to local
--
createOAuthUser :: OAuthKey
                   -> T.Text      -- ^ oauth user id
                   -> Handler App (AuthManager App) ()
createOAuthUser key name = do
    let name' = textToBS name
        passwd = ClearText name'
        role = Role (BS.pack $ show key)
    exists <- usernameExists name
    unless exists (void (createUser name name'))
    res <- loginByUsername name' passwd False
    case res of
      Left l -> liftIO $ print l
      Right r -> do
                 res2 <- saveUser (r {userRoles = [ role ]})
                 return ()
                 --either (liftIO . print) (const $ return ()) res2

----------------------------------------------------------------------
--  Routes
----------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/oauthCallback", weiboOauthCallbackH)
         , ("/googleCallback", googleOauthCallbackH)
         , ("/githubCallback", githubOauthCallbackH)
         ]

-- | NOTE: when use such way to show callback result,
--         the url does not change, which can not be invoke twice.
--         This is quite awkful thing and only for testing purpose.
--
toHome a = heistLocal (bindRawResponseSplices a) $ render "index"

----------------------------------------------------------------------
--
----------------------------------------------------------------------
