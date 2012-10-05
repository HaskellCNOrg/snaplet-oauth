{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

{-
   templates must follow a kind of scalfold
   check template dir for reference
-}

module Main where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Network.HTTP.Conduit (responseBody)
import           Network.HTTP.Types (renderSimpleQuery)
import           Prelude hiding ((.))
import           Snap
import           Snap.Core
import           Snap.Snaplet.Config
import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.OAuth
import           Snap.Util.FileServe
import           System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif

import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient

import Weibo.Key
import Taobao.Key
import Weibo.Api
import Taobao.Api
import Utils

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _weibo   :: Snaplet OAuthSnaplet
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasOauth App where
   oauthLens = weibo
   

type AppHandler = Handler App App


------------------------------------------------------------------------------

weiboOAuth :: OAuth2
weiboOAuth = weiboKey

taobaoOAuth :: OAuth2
taobaoOAuth = taobaoKey

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
--              Weibo
------------------------------------------------------------------------------

loginWithWeiboH :: Handler App App ()
loginWithWeiboH = loginWithOauth Nothing


weiboCallbackH :: Handler App App ()
weiboCallbackH = oauthCallbackHandler (Just "/")

-- | Show Account detail info.
-- 
accountShowH :: Handler App App ()
accountShowH = do
  oauth <- readOAuthMVar
  checkLogin oauth
  maybeUID <- liftIO $ requestUid oauth
  case maybeUID of
    Just uid  ->  modifyResponse (setContentType "application/json")
                  >> liftIO (requestAccount oauth uid)
                  >>= (writeText . lbsToText)
    _         ->  writeBS "Failed at getting UID."

-- | Post a new msg.
--
postnewH:: Handler App App ()
postnewH = do
  oauth <- readOAuthMVar
  checkLogin oauth
  content <- decodedParam "content"
  rsp <- liftIO $ postNew content oauth
  writeLBS rsp 

----------------------------------------------------------------------------
--              Taobao
------------------------------------------------------------------------------


loginWithTaobaoH :: Handler App App ()
loginWithTaobaoH = loginWithOauth Nothing

taobaoUserGetH :: Handler App App ()
taobaoUserGetH = readOAuthMVar >>= liftIO . taobaoUserGet >>= writeLBS


----------------------------------------------------------------------------


-- | Redirect to login page if not login yet.
--
-- FIXME: better to notice user that redirect to login via weibo.
-- 
checkLogin :: OAuth2 -> Handler App App ()
checkLogin oa = when (isNothing $ oauthAccessToken oa) $ redirect "weibo"


showOAuthDataH :: Handler App App ()
showOAuthDataH = readOAuthMVar >>= writeText . T.pack . show


------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("/weibo"        , loginWithWeiboH)
          , ("/weibo/new"      , postnewH)
          , ("/weibo/account"  , accountShowH)
          , ("/taobao"       , loginWithTaobaoH)
          , ("/taobao/user"  , taobaoUserGetH)
          , ("/oauthCallback", weiboCallbackH)
          , ("/test"         , showOAuthDataH)
          , ("", with heist heistServe)
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    w <- nestSnaplet "weiboOAuth" weibo $ initOauthSnaplet weiboOAuth Nothing
--    w <- nestSnaplet "taobaoOAuth" weibo $ initOauthSnaplet taobaoOAuth Nothing
    addRoutes routes
    return $ App h w


------------------------------------------------------------------------------

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["heist/templates"])

    _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
