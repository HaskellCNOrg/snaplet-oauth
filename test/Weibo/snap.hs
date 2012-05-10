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
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit (responseBody)
import           Prelude hiding ((.))
import           Control.Category

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
import qualified Snap as Snap


import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient

import Snap.Snaplet.OAuth

import Key
import Api

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _oauth   :: Snaplet OAuthSnaplet
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasOauth App where
   oauthLens' = oauth
   oauthLens = subSnaplet oauth

type AppHandler = Handler App App


------------------------------------------------------------------------------

weibooauth :: OAuth2
weibooauth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }


decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

weibo :: Handler App App ()
weibo = loginWithOauth

weiboCallback :: Handler App App ()
weiboCallback = oauthCallbackHandler $ Just "/getUid"

test :: Handler App App ()
test = do
  ss <- getOauthSnaplet
  writeText $ T.pack (show $ getOauth ss)


------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe) -- ^ FIXME: maybe no need heist
          , ("/", writeBS "It works!<a href='/weibo'>login via weibo</a>")  -- FIXME: parseHTML
          , ("/weibo", weibo)
          , ("/oauthCallback", weiboCallback )
          , ("/getUid", test)
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