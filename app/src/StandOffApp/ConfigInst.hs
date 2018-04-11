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
import qualified StandOffApp.Auth.PostgRest as AuthPG

import StandOffApp.Bibliography.Model hiding (OuterBubble)
import qualified StandOffApp.Bibliography.Model as Biblio (OuterBubble)


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


-- * Bibliography
instance (Reflex t) => BiblioConfig (Model t) where
  baseUri = _cfg_baseUri . _model_config
  biblioHeadlineDepth = _cfg_headlineDepth . _model_config
  parseError = _cfg_biblioParseError . _model_config
  parseEntries = _cfg_biblioParseEntries . _model_config
  entriesRequest = _cfg_biblioEntriesRequest . _model_config
  parseFields = _cfg_biblioParseFields . _model_config
  fieldsRequest = _cfg_biblioFieldsRequest . _model_config

instance (Reflex t) => BiblioModel (Model t) t where
  authRequestConfig = fmap AuthPG.cfgAuthRq . _auth_token . _model_auth
  biblioEntryTypes = _biblio_entryTypes . _model_biblio
  biblioFieldTypes = _biblio_fieldTypes . _model_biblio

instance (Reflex t) => Biblio.OuterBubble (EventBubble t) t where
  getBiblioBubble = evBub_biblioBubble
  extractBiblioDataEvent = evBub_authBubble . authEvBub_evToken -- FIXME: move to config
