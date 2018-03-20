{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Model
  where

import Reflex.Dom
import Reflex
import Data.Monoid
import Data.Text
import Control.Monad

import StandOffApp.Config


type AuthToken = Text

data Model t
  = Model
  { _model_authToken :: Dynamic t (Maybe AuthToken)
  , _model_config :: AppConfig
  }

data Aggregate t
  = Aggregate
  { _agg_authToken :: Dynamic t (Maybe AuthToken)
  }

instance (Reflex t) => Monoid (Aggregate t) where
  mempty = Aggregate (constDyn Nothing)
  mappend e _ = e

appModel :: MonadWidget t m => Dynamic t (Aggregate t) -> AppConfig -> m (Model t)
appModel aggregate conf = do
  let token = join $ fmap _agg_authToken aggregate
  --tok <- holdDyn Nothing evToken
  return $ Model token conf
    
