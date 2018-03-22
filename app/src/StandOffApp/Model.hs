{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Model
  where

import Reflex.Dom
import Reflex
import Data.Semigroup
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
  { _agg_evAuthToken :: Event t (Maybe AuthToken)
  }

instance (Reflex t) => Semigroup (Aggregate t) where
  (<>) a _ = a

appModel :: MonadWidget t m => Event t (Aggregate t) -> AppConfig -> m (Model t)
appModel aggregate conf = do
  let evToken = coincidence (_agg_evAuthToken <$> aggregate)
  token <- holdDyn Nothing evToken
  return $ Model token conf
    
