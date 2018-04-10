{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Model
  where

import Reflex.Dom
import Reflex
import Data.Semigroup
import Control.Lens
import Data.Default.Class
import Control.Monad.Trans.Reader

import StandOffApp.Config

import StandOffApp.Auth.Model
import StandOffApp.Bibliography.Model

-- * Model

-- | The Model is accessible in all widgets throughout the app. It
-- consists of static configuraton data and dynamic data, i.e. data,
-- that changes over time.
data Model t
  = Model
  { _model_config :: AppConfig -- ^ configuration
  , _model_auth :: AuthData t -- ^ dynamic data related to authentication
  , _model_biblio :: BiblioData t -- ^ dynamic data related to the bibliography
  }

makeLenses ''Model

-- | Make dynamic data from propagated events.
appModel :: MonadWidget t m => Event t (EventBubble t) -> AppConfig -> m (Model t)
appModel bub conf = do
  let evAuth = fmap _evBub_authBubble bub
  auth <- authModel evAuth
  let evBiblio = fmap _evBub_biblioBubble bub
  biblio <- biblioModel evBiblio
  return $ Model
    { _model_config = conf
    , _model_auth = auth
    , _model_biblio = biblio
    }

-- * Event propagation

-- | Events are propagated to the top level widget via the
-- 'EventBubble'. For convienent construction of the bubble
-- 'EventBubble' is an instance of 'Default' and a lens.
data EventBubble t
  = EventBubble
  { _evBub_authBubble :: AuthEventBubble t
  , _evBub_biblioBubble :: BiblioEventBubble t
  }

makeLenses ''EventBubble

instance (Reflex t) => Default (EventBubble t) where
  def = EventBubble
        { _evBub_authBubble = def
        , _evBub_biblioBubble = def
        }

-- | The events are combined monoidally (semigroup'isch). That's
-- important for getting simultaneous events right. See
-- https://www.reddit.com/r/reflexfrp/comments/85ov8a/how_to_share_auth_tokens_throughout_all_the_app
instance (Reflex t) => Semigroup (EventBubble t) where
  (<>) a b = EventBubble
             { _evBub_authBubble = _evBub_authBubble a <> _evBub_authBubble b
             , _evBub_biblioBubble = _evBub_biblioBubble a <> _evBub_biblioBubble b
             }

-- * Widget Types

-- | A type alias for transformations with a 'ReaderT' and
-- 'EventWriterT' that can be used with a 'MonadWidget'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> yourWidget :: MonadWidget t m => AppWidget t m ()
type AppWidget t m a = ReaderT
                       (Model t)
                       (EventWriterT t (EventBubble t) m)
                       a

