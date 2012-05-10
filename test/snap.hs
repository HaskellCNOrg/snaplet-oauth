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
import           Control.Category
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe, fromJust)
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
import qualified Snap as Snap

#ifdef DEVELOPMENT
import           Snap.Loader.Devel
#else
import           Snap.Loader.Prod
#endif

import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient

import Key
import Api

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _weibo   :: Snaplet OAuthSnaplet
    , _google  :: Snaplet OAuthSnaplet
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- | FIXME : how to allow multiple OAuthSnaplets ??
instance HasOauth App where
   oauthLens' = google
   oauthLens = subSnaplet google

type AppHandler = Handler App App


------------------------------------------------------------------------------

weiboOAuth :: OAuth2
weiboOAuth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }

googleOAuth :: OAuth2
googleOAuth = googleKeys { oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token" 
                   , oauthAccessToken = Nothing
                   }

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

loginWithWeibo :: Handler App App ()
loginWithWeibo = loginWithOauth Nothing

weiboCallback :: Handler App App ()
weiboCallback = oauthCallbackHandler $ Just "/test"

loginWithGoogle :: Handler App App ()
loginWithGoogle = loginWithOauth $ Just googleScopeStr

googleCallback :: Handler App App ()
googleCallback = oauthCallbackHandler $ Just "/test"

test :: Handler App App ()
test = do
  ss <- getOauthSnaplet
  writeText $ T.pack (show $ getOauth ss)

-- | this is special for google.
googleScopeStr = renderSimpleQuery False [("scope", "https://www.googleapis.com/auth/userinfo.email")]


------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe) -- ^ FIXME: maybe no need heist
          , ("/", writeBS "It works!<a href='/weibo'>login via weibo</a>, <a href='/google'>login via google</a>")  -- FIXME: parseHTML
          , ("/weibo", loginWithWeibo)
          , ("/google", loginWithGoogle)
          , ("/oauthCallback", weiboCallback )
          , ("/googleCallback", googleCallback)
          , ("/test", test)
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    w <- nestSnaplet "weiboOAuth" weibo $ initOauthSnaplet weiboOAuth "code"
    g <- nestSnaplet "googleOAuth" google $ initOauthSnaplet googleOAuth "code"    
    addRoutes routes
    return $ App h w g


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