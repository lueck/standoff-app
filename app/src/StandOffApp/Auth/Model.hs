-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Auth.Model
  where

import Reflex
import Reflex.Dom
import Data.Text
import Data.Default.Class
import Control.Lens
import Data.Semigroup
import Data.Profunctor.Unsafe

--import StandOffApp.Model.ClassDefs (AppModel, EventBubble)
-- Do we realy need that? Yes: EventBubble, which must be an instance
-- of Default, is needed in the login widget. No: We just take
-- Semigroup, there.


-- * Model

type AuthToken = Text

-- | A record of values that are accessible throughout the whole app.
data AuthData t = AuthData
  { _auth_token :: Dynamic t (Maybe AuthToken)
  --, user :: Dynamic t (Maybe Text)
  }

-- | Return 'AuthData' from a bubble.
authModel :: MonadWidget t m => Event t (AuthEventBubble t) -> m (AuthData t)
authModel bubble = do
  let evToken = coincidence (_authEvBub_evToken <$> bubble)
  tok <- holdDyn Nothing evToken
  return $ AuthData
    { _auth_token = tok
    }

-- * Config

-- | Aspects of the apps model that has to be defined in order to make
-- this authentication module work.

--class (AppModel m) => AuthConfig m where
class AuthModel m where
  loginUri :: m -> Text -- ^ uri (base uri + path) of login rpc
  loginMethod :: m -> Text -- ^ http method for login rpc
  --authToken :: m -> Dynamic t (Maybe AuthToken) -- ^ getter for the token
  -- authEventBubble :: m -> (m -> AuthEventBubble t) -- FIXME
  --authEventBubble :: (Reflex t, Functor f, Semigroup w, Default w) => m -> ((AuthEventBubble t -> f (AuthEventBubble t)) -> w -> f w)
  --authEventBubble :: (Reflex t, Functor f, Profunctor p) =>
  --                   m -> (p (Event t (AuthEventBubble t)) (f (Event t (AuthEventBubble t))) -> p (w t) (f (w t)))
  parseAuthToken :: (m -> (Either XhrException XhrResponse -> Maybe AuthToken))

-- * EventBubble

-- | A record for all the events (values) that have to go up in the
-- dom to be made accessible throughout the app.
data AuthEventBubble t = AuthEventBubble
  { _authEvBub_evToken :: Event t (Maybe AuthToken) -- ^ fired when xhr returned a token
  }

makeLenses ''AuthEventBubble

-- | The EventBubble must be an instance of 'Default', so that it can
-- easily be created by events pushed into the event writer by other
-- modules.
instance (Reflex t) => Default (AuthEventBubble t) where
  def = AuthEventBubble
        { _authEvBub_evToken = never
        }

defaultAuthEventBubble :: Reflex t => AuthEventBubble t
defaultAuthEventBubble = def

-- | Combination of events in the event writer.
instance (Reflex t) => Semigroup (AuthEventBubble t) where
  (<>) a b = AuthEventBubble
             { _authEvBub_evToken = a^.authEvBub_evToken <> b^.authEvBub_evToken
             }

class (Semigroup o) => OuterBubble o where
  getAuthBubble :: (Reflex t) => o -> AuthEventBubble t
