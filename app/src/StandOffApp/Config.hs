{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Config
  where

import Data.Text
import Control.Lens
import Data.Monoid ((<>))


data StandOffAppConfig
  = StandOffAppConfig
  { _cfg_baseUri :: Text -- ^ base uri
  , _cfg_loginPath :: Text -- ^ uri of login
  , _cfg_loginMethod :: Text -- ^ method of login request
  } deriving (Show)

makeLenses ''StandOffAppConfig


-- | A default configuration. It works with a locally running
-- PostgREST app.
defaultConfig :: StandOffAppConfig
defaultConfig =
  StandOffAppConfig
  { _cfg_baseUri = "http://127.0.0.1:3000"
  , _cfg_loginPath = "/rpc/login"
  , _cfg_loginMethod = "POST"
  }


-- | Return a uri for a record field (path) and a config.
configUri :: (StandOffAppConfig -> Text) -> StandOffAppConfig -> Text
configUri fld cfg = _cfg_baseUri cfg <> fld cfg

