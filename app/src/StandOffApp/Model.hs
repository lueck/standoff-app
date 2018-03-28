{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StandOffApp.Model
  where

import Reflex.Dom
import Reflex
import Data.Semigroup
import Data.Text
import Control.Monad
import Control.Lens
import Data.Default.Class

import StandOffApp.Config

import StandOffApp.Auth.Model

-- | The Model accessible throughout the app's widgets. It consists of
-- static configuraton data and dynamic data, i.e. data, that changes
-- over time.
data Model t
  = Model
  { _model_config :: AppConfig -- ^ configuration
  , _model_auth :: AuthData t -- ^ dynamic data related to authentication
  }

makeLenses ''Model

-- | Make dynamic data from propagated events.
appModel :: MonadWidget t m => Event t (EventBubble t) -> AppConfig -> m (Model t)
appModel bub conf = do
  let evAuth = fmap _evBub_authBubble bub
  auth <- authModel evAuth
  return $ Model
    { _model_config = conf
    , _model_auth = auth
    }

-- * Event propagation

-- | Events are propagated to the top level widget via the
-- 'EventBubble'. For convienent construction of the bubble
-- 'EventBubble' is an instance of 'Default' and a lens.
data EventBubble t
  = EventBubble
  { _evBub_authBubble :: AuthEventBubble t
  }

makeLenses ''EventBubble

instance (Reflex t) => Default (EventBubble t) where
  def = EventBubble
        { _evBub_authBubble = def
        }

-- | The events are combined monoidally (semigroup'isch). That's
-- important for getting simultaneous events right. See
-- https://www.reddit.com/r/reflexfrp/comments/85ov8a/how_to_share_auth_tokens_throughout_all_the_app
instance (Reflex t) => Semigroup (EventBubble t) where
  (<>) a b = EventBubble
             { _evBub_authBubble = _evBub_authBubble a <> _evBub_authBubble b
             }
