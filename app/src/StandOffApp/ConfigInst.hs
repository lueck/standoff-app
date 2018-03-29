{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StandOffApp.ConfigInst where

import Reflex
import Data.Monoid ((<>))
import Data.Default.Class
import Control.Lens

import StandOffApp.Config
import StandOffApp.Model
import StandOffApp.ConfigClassDefs

import StandOffApp.Auth.Model

-- | In order to decouple the app's submodules 'AppConfig' is an instance
-- of the submodules' configurations.

-- * Basic

-- | The app has a base uri.
instance BaseConfig AppConfig where
  baseUri = _cfg_baseUri

-- * Authentication

-- | Make the 'Model' an instance of 'AuthConfig'. This is mostly
-- boilerplate.
instance (Reflex t) => AuthConfig (Model t) where
  loginUri = (\c -> _cfg_baseUri c <> _cfg_loginPath c) . _model_config
  loginMethod = _cfg_loginMethod . _model_config
  parseAuthToken = _cfg_parseToken . _model_config
  parseLoginError = _cfg_parseLoginError . _model_config
  loginRequest = _cfg_loginRequest . _model_config
  authHeadlineDepth = _cfg_headlineDepth . _model_config

instance (Reflex t) => AuthModel (Model t) t where
  authToken = _auth_token . _model_auth

instance (Reflex t) => OuterBubble (EventBubble t) t where
  getAuthBubble = evBub_authBubble
