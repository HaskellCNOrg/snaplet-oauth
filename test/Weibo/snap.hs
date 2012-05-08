{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes  #-}

{-
   templates must follow a kind of scalfold
   check template dir for reference
-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import           Network.HTTP.Conduit (responseBody)


#ifdef DEVELOPMENT
import           Snap.Loader.Devel
#else
import           Snap.Loader.Prod
#endif

import           Snap.Core
import           Snap.Http.Server
import           System.IO
import           Snap
import           Snap.Util.FileServe
import           Snap.Snaplet.Heist

import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient

import Snap.Snaplet.OAuth

import Key
import Api

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _oauth   :: Snaplet OAuth
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasOauth App where
  oauthLens = subSnaplet oauth


type AppHandler = Handler App App


------------------------------------------------------------------------------

weibooauth :: OAuth2
weibooauth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }


--                   -- getting UID. FIXME: res could be error
--                   res <- liftIO $ requestUid accountUidUri token'
--                   liftIO $ print $ BS.unpack $ apiUrlGet2 accountShowUri (token', fromJust res)
--                   res2 <- liftIO $ doSimpleGetRequest . BS.unpack $ apiUrlGet2 accountShowUri (token', fromJust res)
--                   writeLBS $ "user: " `LBS.append` responseBody res2
--    _ -> writeBS "Error getting access token"


--+    modifySnapletState (setL (oauth . snapletConfig) (Just p))

-- modifySnapletState :: (Snaplet v -> Snaplet v) -> Handler b v ()
 --setL :: (Lens a b) -> b -> a -> a
--snapletConfig :: Lens (Snaplet a) SnapletConfig
--snapletValue :: Lens (Snaplet a) a
--  modL :: (Lens a b) -> (b -> b) -> a -> a

--modifyOAuthAccessToken :: AccessToken -> Snaplet OAuth -> Snaplet OAuth
--modifyOAuthAccessToken at = 

  

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

weibo :: Handler App App ()
weibo = loginWithOauth

weiboCallback :: Handler App App ()
weiboCallback = oauthCallbackHandler

------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe) -- ^ FIXME: maybe no need heist
          , ("/", writeBS "It works!<a href='/weibo'>test</a>")
          , ("/weibo", weibo)
          , ("/oauthCallback", weiboCallback )
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    o <- nestSnaplet "oauth" oauth $ initOauthSnaplet weibooauth "code"
    addRoutes routes
    return $ App h o


------------------------------------------------------------------------------

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap ())
getConf = commandLineConfig defaultConfig

getActions :: Config Snap () -> IO (Snap (), IO ())
getActions _ = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
