module StandOffApp.ConfigClassDefs
  where

import Data.Text
import Data.Monoid ((<>))
import Data.Semigroup hiding ((<>))


-- | Very basic and common features of the configuration.
class AppConfigC c where
  -- | Returns the base uri.
  baseUri :: c -> Text
  -- | Returns an absolute path for a given getter. Usage:
  -- | @ uri <- asks (absPath loginUri) @ 
  absPath :: (c -> Text) -- ^ a getter for a relative path
          -> c           -- ^ the config
          -> Text        -- ^ the absolute Path is returned
  absPath rel cfg = baseUri cfg <> rel cfg


class EventBubbleC b where
  authEventBubble :: (Semigroup a) => b -> a
