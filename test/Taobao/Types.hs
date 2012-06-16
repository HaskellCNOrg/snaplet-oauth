
{-# LANGUAGE OverloadedStrings #-}


module Taobao.Types where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import Network.HTTP.Types (renderSimpleQuery)

import Network.OAuth2.OAuth2

import Utils

----------------------------------------------------------


-- | Params for request besides access_token.
-- 
-- http://open.taobao.com/doc/detail.htm?id=994
-- 
data TaobaoApiParam = TaobaoApiParam 
                      { method :: BS.ByteString
                      , format :: BS.ByteString
                      , version :: BS.ByteString    -- ^ oauth version(2.0)
                      }
paramToQuery (TaobaoApiParam m f v) = [ ("method", m)
                                  , ("format", f)
                                  , ("v", v) ] 

----------------------------------------------------------

defaultTaobaoApiParam :: TaobaoApiParam
defaultTaobaoApiParam = TaobaoApiParam "" "json" "2.0"

paramToQueryString :: OAuth2 -> TaobaoApiParam -> URI
paramToQueryString oa ps = 
    baseUri
    `appendQueryParam` ( (paramToQuery ps) 
                         ++ (accessTokenToParam $ fromJust $ oauthAccessToken oa)
                         ++ [("fields", "user_id,uid,nick,sex")])

---------------------------------------------------------------


baseUri :: BS.ByteString
baseUri = sToBS "https://eco.taobao.com/router/rest"
--baseUri = sToBS "http://gw.api.taobao.com/router/rest"


--taobaoUserGetMethod = "taobao.user.get"
taobaoUserGetP = defaultTaobaoApiParam {method = "taobao.user.get"}

---------------------------------------------------------------

