{-# LANGUAGE OverloadedStrings #-}

{-

This is basically very manual test. Check following link for details.

google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Snap.Snaplet.OAuth.Google.Api where

import qualified Data.ByteString                as BS
import           Control.Applicative
import           Control.Monad                  (mzero)
import           Data.Aeson
import           Data.Text                      (Text)

import           Network.OAuth2.OAuth2
import           Snap.Snaplet.OAuth.Utils


----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------

data GoogleUser = GoogleUser { gid   :: Text
                             , gname :: Text
                             , glink :: Text
--                             , gemail :: Text
                             }

instance FromJSON GoogleUser where
    parseJSON (Object o) = GoogleUser
                           <$> o .: "id"
                           <*> o .: "name"
                           <*> o .: "link"
--                           <*> o .: "email"
    parseJSON _ = mzero

----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------


userInfo :: OAuth2 -> IO (Maybe GoogleUser)
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


{-
samples - userinfo

{
 "id": "xxxxxxxxxxx",
 "name": "Haisheng Wu",
 "given_name": "Haisheng",
 "family_name": "Wu",
 "link": "https://plus.google.com/xxxxxxxxxxxxx",
 "picture": "https://lh3.googleusercontent.com/-jDSzU-Od2IA/AAAAAAAAAAI/AAAAAAAABsM/aAN1D9g0u5g/photo.jpg",
 "gender": "male",
 "locale": "en"
}
-}
