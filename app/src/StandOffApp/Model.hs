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
--import StandOffApp.Auth.Model

type AuthToken = Text

-- | A record of values that are accessible throughout the whole app.
data AuthData t = AuthData
  { _auth_token :: Dynamic t (Maybe AuthToken)
  --, user :: Dynamic t (Maybe Text)
  }

data Model t
  = Model
  { _model_config :: AppConfig
  , _model_auth :: AuthData t
  }


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


makeLenses ''Model

data EventBubble t
  = EventBubble
  { _evBub_authBubble :: AuthEventBubble t
  }

makeLenses ''EventBubble

instance (Reflex t) => Semigroup (EventBubble t) where
  (<>) a b = EventBubble
             { _evBub_authBubble = _evBub_authBubble a <> _evBub_authBubble b
             }

instance (Reflex t) => Default (EventBubble t) where
  def = EventBubble
        { _evBub_authBubble = def
        }

defaultEventBubble :: (Reflex t) => EventBubble t
defaultEventBubble = EventBubble
                     { _evBub_authBubble = def
                     }

appModel :: MonadWidget t m => Event t (EventBubble t) -> AppConfig -> m (Model t)
appModel bub conf = do
  let evAuth = fmap _evBub_authBubble bub
  auth <- authModel evAuth
  return $ Model
    { _model_config = conf
    , _model_auth = auth
    }


-- ############# taken from auth module ############


-- * Model

-- | Return 'AuthData' from a 'AuthEventBubble'.
authModel :: MonadWidget t m => Event t (AuthEventBubble t) -> m (AuthData t)
authModel bubble = do
  let evToken = coincidence (_authEvBub_evToken <$> bubble)
  tok <- holdDyn Nothing evToken
  return $ AuthData
    { _auth_token = tok
    }

-- * Config

-- | Aspects of the app's model that has to be defined in order to make
-- this authentication module work.
class AuthModel m where
  loginUri :: m -> Text -- ^ uri (base uri + path) of login rpc
  loginMethod :: m -> Text -- ^ http method for login rpc
  parseAuthToken :: (m -> (Either XhrException XhrResponse -> Maybe AuthToken)) -- ^ Function for parsing the token from a xhr response.

-- | Like 'AuthModel' but parametrized with reflex time line.
class AuthModelTime m t where
  authToken :: m -> Dynamic t (Maybe AuthToken) -- ^ getter for the token

-- | The event bubble containing the 'AuthEventBubble'. It must have a
-- setter for a field containing the 'AuthEventBubble'.
class (Reflex t, Semigroup o) => OuterBubble o t where
  getAuthBubble :: ASetter o o (AuthEventBubble t) (AuthEventBubble t)
