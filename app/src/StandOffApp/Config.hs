{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Config
  where

import Reflex.Dom
import Data.Text
import Control.Lens
import Data.Monoid ((<>))

import StandOffApp.ConfigClassDefs
import StandOffApp.Xhr
import qualified StandOffApp.PostgRest as PG

import qualified StandOffApp.Auth.Model as Auth
import qualified StandOffApp.Auth.PostgRest as AuthPG

import qualified StandOffApp.Bibliography.Model as Bib
import qualified StandOffApp.Bibliography.PostgRest as BibPG

-- import StandOffApp.Auth.Widget

data AppConfig
  = AppConfig
  { _cfg_baseUri :: Text -- ^ base uri
  , _cfg_confAuthRequest :: Maybe Text -> XhrRequestConfig ()
  -- * Authentication
  , _cfg_loginPath :: Text -- ^ uri of login
  , _cfg_loginMethod :: Text -- ^ method of login request
  , _cfg_headlineDepth :: Int -- ^ Depth of a headline for a major view.
  , _cfg_loginRequest :: Text -> Text -> XhrRequestConfig Text -- ^ make a request from user and pwd
  , _cfg_parseToken :: Either XhrException XhrResponse -> Maybe Auth.AuthToken -- ^ parse the authentication token from xhr response
  , _cfg_parseLoginError :: Either XhrException XhrResponse -> Maybe Text
  -- * Bibliography
  , _cfg_biblioTypeFieldsRequest :: Text -- ^ base uri
                                 -> XhrRequestConfig () -- ^ config for authenticated request
                                 -> Text -- ^ entry type
                                 -> XhrRequest () -- bbb
  , _cfg_biblioFieldsRequest :: Text -- ^ base uri
                             -> XhrRequestConfig () -- ^ config for authenticated request
                             -> XhrRequest ()
  , _cfg_biblioParseError :: Either XhrException XhrResponse -> Maybe Text
  , _cfg_biblioParseFields :: Either XhrException XhrResponse -> Bib.Fields
  }

makeLenses ''AppConfig


-- | A default configuration. It works with a locally running
-- PostgREST app.
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
  { -- Basic
    _cfg_baseUri = "http://127.0.0.1:3000"
  , _cfg_confAuthRequest = AuthPG.cfgAuthRq
  -- Authentication
  , _cfg_loginPath = "/rpc/login"
  , _cfg_loginMethod = "POST"
  , _cfg_headlineDepth = 3
  , _cfg_loginRequest = AuthPG.makeLoginRequest
  , _cfg_parseToken = AuthPG.parseJwt
  , _cfg_parseLoginError = PG.parseError
  -- Bibliography
  , _cfg_biblioTypeFieldsRequest = BibPG.makeTypeFieldsRequest -- bbb
  , _cfg_biblioFieldsRequest = BibPG.makeFieldsRequest
  --, _cfg_biblioFieldsEventExtractor = FIXME
  , _cfg_biblioParseError = PG.parseError
  , _cfg_biblioParseFields = BibPG.parseFieldsResponse
  }
