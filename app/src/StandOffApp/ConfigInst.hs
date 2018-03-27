module StandOffApp.ConfigInst where

import Reflex
import Data.Monoid ((<>))

import StandOffApp.Config
import StandOffApp.Model
import StandOffApp.ConfigClassDefs

import StandOffApp.Auth.Model
import StandOffApp.Auth.PostgRest

-- * Instances

-- | In order to decouple the apps submodules AppConfig is an instance
-- of the submodules' configurations.

-- | The app has a base uri.
instance AppConfigC AppConfig where
  baseUri = _cfg_baseUri

-- | The authentication module is used.
instance (Reflex t) => AuthModel (Model t) where
  loginUri = (\c -> _cfg_baseUri c <> _cfg_loginPath c) . _model_config
  loginMethod = _cfg_loginMethod . _model_config
  -- authToken = (_auth_token . _model_auth) -- m -- :: AuthData t)
  --authEventBubble = (const evBub_authBubble)
  parseAuthToken = (const parseJwt)
