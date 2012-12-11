{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Github
       ( routes
       , loginWithGithubH
       , githubCallbackH
       , user
       , module Snap.Snaplet.OAuth.Github.Api
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString                      (ByteString)
import           Data.Maybe
import           Prelude                              hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Internal.Handlers
import           Snap.Snaplet.OAuth.Internal.Types
import           Snap.Snaplet.OAuth.Github.Api

------------------------------------------------------------------------------
--              Github
------------------------------------------------------------------------------

loginWithGithubH :: HasOAuth b => Handler b v ()
loginWithGithubH = loginWithOauthH Github Nothing


-- | token access callback.
--   return a @OAuthValue@ having access token has been filled.
--
githubCallbackH :: HasOAuth b => Handler b v OAuthValue
githubCallbackH = oauthCallbackH Github

-- | user
--
user :: HasOAuth b => OAuthValue -> Handler b v (Maybe GithubUser)
user = liftIO . apiUser


------------------------------------------------------------------------------

-- | The application's routes.
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/github" , loginWithGithubH)
          ]


----------------------------------------------------------------------------
