{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Model
  where

import Reflex.Dom
import Reflex
import Data.Semigroup
import Data.Text
import Control.Monad
import Control.Lens

import StandOffApp.Config
import StandOffApp.Auth.Model

data Model t
  = Model
  { _model_config :: AppConfig
  , _model_auth :: AuthData t
  }

makeLenses ''Model

data EventBubble t
  = EventBubble
  { _evBub_authBubble :: Event t (AuthEventBubble t)
  }

makeLenses ''EventBubble

instance (Reflex t) => Semigroup (EventBubble t) where
  (<>) a b = EventBubble
             { _evBub_authBubble = _evBub_authBubble a <> _evBub_authBubble b
             }

appModel :: MonadWidget t m => Event t (EventBubble t) -> AppConfig -> m (Model t)
appModel bub conf = do
  let evAuth = coincidence (_evBub_authBubble <$> bub)
  auth <- authModel evAuth
  return $ Model
    { _model_config = conf
    , _model_auth = auth
    }
