{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StandOffApp.Auth.Model
  where

import Reflex
import Reflex.Dom
import Data.Text
import Data.Default.Class
import Control.Lens
import Data.Semigroup


-- * Model

type AuthToken = Text

-- | A record of values that are accessible throughout the whole app.
data AuthData t = AuthData
  { _auth_token :: Dynamic t (Maybe AuthToken)
  --, user :: Dynamic t (Maybe Text)
  }

-- | Configuration of the authentication module, i.e. aspects of the
-- app's model that has to be defined in order to make this
-- authentication module work. Note, that the functions are intended
-- to be passed the model, not the model.config. Functions
-- parametrized with a reflex time line should go to 'AuthModel'.
class AuthConfig m where
  loginUri :: m -> Text -- ^ uri (base uri + path) of login rpc
  loginMethod :: m -> Text -- ^ http method for login rpc
  parseAuthToken :: m -> (Either XhrException XhrResponse -> Maybe AuthToken) -- ^ Function for parsing the token from a xhr response.
  parseLoginError :: m -> (Either XhrException XhrResponse -> Maybe Text) -- ^ Function for parsing the xhr response to an error message.
  loginRequest :: m -> (Text -> Text -> XhrRequestConfig Text)
  authHeadlineDepth :: m -> Int -- ^ Whether "h2", "h3" in widgets.

-- | Like 'AuthConfig' but parametrized with reflex time line.
class AuthModel m t where
  authToken :: m -> Dynamic t (Maybe AuthToken) -- ^ getter for the token

-- | Return 'AuthData' from a 'AuthEventBubble'.
authModel :: MonadWidget t m => Event t (AuthEventBubble t) -> m (AuthData t)
authModel bubble = do
  let evToken = coincidence (_authEvBub_evToken <$> bubble)
  tok <- holdDyn Nothing evToken
  return $ AuthData
    { _auth_token = tok
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

-- | Combination of events in the event writer.
instance (Reflex t) => Semigroup (AuthEventBubble t) where
  (<>) a b = AuthEventBubble
             { _authEvBub_evToken = a^.authEvBub_evToken <> b^.authEvBub_evToken
             }

-- | The event bubble containing the 'AuthEventBubble'. It must have a
-- setter for a field containing the 'AuthEventBubble'.
class (Reflex t, Semigroup o) => OuterBubble o t where
  getAuthBubble :: ASetter o o (AuthEventBubble t) (AuthEventBubble t)
