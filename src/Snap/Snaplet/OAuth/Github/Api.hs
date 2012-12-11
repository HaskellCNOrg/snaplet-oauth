{-# LANGUAGE OverloadedStrings #-}

{-


-}

module Snap.Snaplet.OAuth.Github.Api where

import           Control.Applicative
import           Control.Monad                     (mzero)
import           Data.Aeson
import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)

import           Snap.Snaplet.OAuth.Internal.Types
import           Snap.Snaplet.OAuth.Internal.Utils


----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------

-- | TODO: maybe need email infomation
--
data GithubUser = GithubUser { gid    :: Integer
                             , gname  :: Text
                             , glogin :: Text
                             , glink  :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "name"
                           <*> o .: "login"
                           <*> o .: "html_url"
    parseJSON _ = mzero

----------------------------------------------------------------------
--  APIs Impl
----------------------------------------------------------------------


apiUser :: OAuthValue -> IO (Maybe GithubUser)
apiUser = apiRequestOAuth apiUrlUser


----------------------------------------------------------------------
--  APIs URI
----------------------------------------------------------------------

-- | Possible operations of UserInfo
--
apiUrlUser :: BS.ByteString
apiUrlUser = "https://api.github.com/user"
