{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.OAuth.Github
       ( routes
       , githubLoginH
       , githubCallbackH
       , githubUserH
       , module Snap.Snaplet.OAuth.Github.Api
       ) where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Data.ByteString                      (ByteString)
import           Data.Maybe
import           Prelude                              hiding ((.))
import           Snap

import           Snap.Snaplet.OAuth.Github.Api
import           Snap.Snaplet.OAuth.Internal.Handlers
import           Snap.Snaplet.OAuth.Internal.Types

------------------------------------------------------------------------------
--              Github
------------------------------------------------------------------------------

githubLoginH :: HasOAuth b => Handler b v ()
githubLoginH = loginWithOauthH github Nothing


-- | token access callback.
--   return a @OAuthValue@ having access token has been filled.
--
githubCallbackH :: HasOAuth b => Handler b v OAuthValue
githubCallbackH = oauthCallbackH github

-- | user
--
githubUserH :: HasOAuth b => Handler b v (Maybe GithubUser)
githubUserH = githubCallbackH
              >>= liftIO . apiUser


------------------------------------------------------------------------------

-- | The application's routes.
--
routes :: HasOAuth b => [(ByteString, Handler b v ())]
routes  = [ ("/github" , githubLoginH)
          ]
          -- where the callback handler is suppose to be added by user.


----------------------------------------------------------------------------
