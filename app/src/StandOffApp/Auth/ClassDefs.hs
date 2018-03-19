module StandOffApp.Auth.ClassDefs
  where

import Data.Text

import StandOffApp.ConfigClassDefs

class (AppConfigC c) => AuthConfig c where
  loginPath :: (c -> Text)
  loginMethod :: (c -> Text)
