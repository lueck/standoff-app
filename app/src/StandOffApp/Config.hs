{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Config
  where

import Reflex.Dom
import Data.Text
import Control.Lens
import Data.Monoid ((<>))

import StandOffApp.ConfigClassDefs

import qualified StandOffApp.Auth.Model as Auth
import qualified StandOffApp.Auth.PostgRest as PG


data AppConfig
  = AppConfig
  { _cfg_baseUri :: Text -- ^ base uri
  , _cfg_loginPath :: Text -- ^ uri of login
  , _cfg_loginMethod :: Text -- ^ method of login request
  , _cfg_headlineDepth :: Int -- ^ Depth of a headline for a major view.
  , _cfg_loginRequest :: Text -> Text -> XhrRequestConfig Text -- ^ make a request from user and pwd
  , _cfg_parseToken :: Either XhrException XhrResponse -> Maybe Auth.AuthToken -- ^ parse the authentication token from xhr response
  }

makeLenses ''AppConfig


-- | A default configuration. It works with a locally running
-- PostgREST app.
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
  { _cfg_baseUri = "http://127.0.0.1:3000"
  , _cfg_loginPath = "/rpc/login"
  , _cfg_loginMethod = "POST"
  , _cfg_headlineDepth = 3
  , _cfg_loginRequest = PG.makeLoginRequest
  , _cfg_parseToken = PG.parseJwt
  }
