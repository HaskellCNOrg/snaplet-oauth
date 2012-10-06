{-# LANGUAGE OverloadedStrings #-}

{-

This is basically very manual test. Check following link for details.

google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Snap.Snaplet.OAuth.Google.Api where

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy.Char8     as BSL

import           Control.Applicative
import           Data.Aeson
import           Data.Text                      (Text)
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types             as HT
--import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import           Control.Exception
import           Control.Monad                  (mzero)

import           Network.OAuth2.HTTP.HttpClient
import           Network.OAuth2.OAuth2
import           Snap.Snaplet.OAuth.Google.Key


----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------

data GoogleUser = GoogleUser { gid    :: Text
                             , gname  :: Text
                             , glink  :: Text
--                             , gemail :: Text
                             }

instance FromJSON GoogleUser where
    parseJSON (Object o) = GoogleUser
                           <$> o .: "id"
                           <*> o .: "name"
                           <*> o .: "link"
--                           <*> o .: "email"
    parseJSON _ = mzero


userInfo :: GoogleOAuth -> IO (Maybe GoogleUser)
userInfo oa = doSimpleGetRequest (BS8.unpack url)
              >>= fmap decode .  handleResponse
              where url = uriUserInfor `BS.append` HT.renderSimpleQuery True params
                    params = [("access_token", token)]
                    token = case oauthAccessToken (key oa) of
                              Just x  -> x
                              Nothing -> ""


handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp = if (HT.statusCode . responseStatus) rsp == 200
                     then do
                          let body = responseBody rsp
                          return body
                     else throwIO . OAuthException
                          $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)


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
 "id": "103351924087869283689",
 "name": "Haisheng Wu",
 "given_name": "Haisheng",
 "family_name": "Wu",
 "link": "https://plus.google.com/103351924087869283689",
 "picture": "https://lh3.googleusercontent.com/-jDSzU-Od2IA/AAAAAAAAAAI/AAAAAAAABsM/aAN1D9g0u5g/photo.jpg",
 "gender": "male",
 "locale": "en"
}
-}