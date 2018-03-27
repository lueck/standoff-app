{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Config
  where

import Data.Text
import Control.Lens
import Data.Monoid ((<>))

import StandOffApp.ConfigClassDefs

-- import StandOffApp.Auth.Model


data AppConfig
  = AppConfig
  { _cfg_baseUri :: Text -- ^ base uri
  , _cfg_loginPath :: Text -- ^ uri of login
  , _cfg_loginMethod :: Text -- ^ method of login request
  } deriving (Show)

makeLenses ''AppConfig


-- | A default configuration. It works with a locally running
-- PostgREST app.
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
  { _cfg_baseUri = "http://127.0.0.1:3000"
  , _cfg_loginPath = "/rpc/login"
  , _cfg_loginMethod = "POST"
  }
