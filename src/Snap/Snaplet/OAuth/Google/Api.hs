{-# LANGUAGE OverloadedStrings #-}

{-

This is basically very manual test. Check following link for details.

google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Snap.Snaplet.OAuth.Google.Api where

import           Control.Applicative
import           Control.Monad                     (mzero)
import           Data.Aeson
import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)

import           Snap.Snaplet.OAuth.Internal.Utils
import           Snap.Snaplet.OAuth.Internal.Types


----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------

data GoogleUser = GoogleUser { gid    :: Text
                             , gname  :: Text
                             , glink  :: Text
                             , gemail :: Text
                             } deriving (Show, Eq)

instance FromJSON GoogleUser where
    parseJSON (Object o) = GoogleUser
                           <$> o .: "id"
                           <*> o .: "name"
                           <*> o .: "link"
                           <*> o .: "email"
    parseJSON _ = mzero

----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------


userInfo :: OAuthValue -> IO (Maybe GoogleUser)
userInfo = apiRequestOAuth uriUserInfor



----------------------------------------------------------------------
--  APIs URI
----------------------------------------------------------------------

googleScopeKey :: BS.ByteString
googleScopeKey = "scope"

-- | this is special for google Gain read-only access to the user's email address.
googleScopeEmail :: BS.ByteString
googleScopeEmail = "https://www.googleapis.com/auth/userinfo.email"

-- | Gain read-only access to basic profile information.
googleScopeUserInfo :: BS.ByteString
googleScopeUserInfo = "https://www.googleapis.com/auth/userinfo.profile"

-- | Possible operations of UserInfo
--
uriUserInfor :: BS.ByteString
uriUserInfor = "https://www.googleapis.com/oauth2/v2/userinfo"
