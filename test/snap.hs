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
import           Snap.Loader.Devel
#else
import           Snap.Loader.Prod
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
weiboOAuth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }

taobaoOAuth :: OAuth2
taobaoOAuth = taobaoKey { oauthOAuthorizeEndpoint = "https://oauth.taobao.com/authorize"
                      , oauthAccessTokenEndpoint = "https://oauth.taobao.com/token" 
                      , oauthAccessToken = Nothing
                      }

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------

loginWithWeiboHandler :: Handler App App ()
loginWithWeiboHandler = loginWithOauth Nothing


weiboCallbackHandler :: Handler App App ()
weiboCallbackHandler = oauthCallbackHandler $ Just "/"

-- | Show Account detail info.
accountShowHandler :: Handler App App ()
accountShowHandler = do
  oauth <- readOAuthMVar
  redirectToLogin oauth
  maybeUID <- liftIO $ requestUid oauth
  case maybeUID of
    Just uid  ->  (writeText . lbsToText) =<< liftIO (requestAccount oauth uid)
    _         ->  writeBS "Failed at getting UID."

postnewHandler:: Handler App App ()
postnewHandler = do
  oauth <- readOAuthMVar
  redirectToLogin oauth
  content <- decodedParam "content"
  rsp <- liftIO $ postNew content oauth
  writeLBS rsp 

----------------------------------------------------------------------------


loginWithTaobaoHandler :: Handler App App ()
loginWithTaobaoHandler = loginWithOauth Nothing

taobaoUserGetH :: Handler App App ()
taobaoUserGetH = readOAuthMVar >>= liftIO . taobaoUserGet >>= writeLBS


----------------------------------------------------------------------------


-- | Redirect to login page if not login yet.
--
-- FIXME: better to notice user that redirect to login via weibo.
-- 
redirectToLogin :: OAuth2 -> Handler App App ()
redirectToLogin oa = when (isNothing $ oauthAccessToken oa) $ redirect "weibo"


showOAuthDataHandler :: Handler App App ()
showOAuthDataHandler = readOAuthMVar >>= writeText . T.pack . show


------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe)
          , ("/weibo"        , loginWithWeiboHandler)
          , ("/postnew"  , postnewHandler)
          , ("/accountShow"  , accountShowHandler)
          , ("/taobao"        , loginWithTaobaoHandler)
          , ("/taobao/user"        , taobaoUserGetH)
          , ("/oauthCallback", weiboCallbackHandler)
          , ("/test", showOAuthDataHandler)
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    w <- nestSnaplet "weiboOAuth" weibo $ initOauthSnaplet weiboOAuth Nothing
    --w <- nestSnaplet "taobaoOAuth" weibo $ initOauthSnaplet taobaoOAuth Nothing
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

getConf :: IO (Config Snap ())
getConf = commandLineConfig defaultConfig

getActions :: Config Snap () -> IO (Snap (), IO ())
getActions _ = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)


------------------------------------------------------------------------------
--                    Working notes                                   --
------------------------------------------------------------------------------

-- modifySnapletState :: (Snaplet v -> Snaplet v) -> Handler b v ()
-- getL :: (Lens a b) -> a -> b
-- setL :: (Lens a b) -> b -> a -> a
-- modL :: (Lens a b) -> (b -> b) -> a -> a
-- app :: SnapletInit App App
-- snapletConfig :: Lens (Snaplet a) SnapletConfig
-- snapletValue :: Lens (Snaplet a) a

{-
 getMongoDB :: app -> MongoDB
 getMongoDB = getL (snapletValue . database)

-- | translate as
--   MonadState App (Handler App App)
--   thus: `get` could be used for getting `App` instances inside `Handler b v` monad.
-- 
instance MonadState v (Handler b v) where
    get = getsSnapletState _snapletValue
    put v = modifySnapletState (setL snapletValue v)


--
with
withTop
  at first grance, there have same type signature. 
  so any differences?? dig into detail but not understand it yet.
-}
