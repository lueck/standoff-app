{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Auth.PostgRest where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad
import Data.Monoid ((<>))
import Control.Lens
import Control.Monad.Reader
import Data.Maybe

import StandOffApp.Auth.Model

-- * /rpc/login

-- | Build a login request.
makeLoginRequest :: T.Text -> T.Text -> XhrRequestConfig T.Text
makeLoginRequest usr pwd =
  def
  & xhrRequestConfig_sendData .~ credJson
  & xhrRequestConfig_headers .~ Map.fromList [("Content-Type", "application/json")]
  where
    credJson = "{ \"login\": \"" <> usr <> "\", \"pwd\": \"" <> pwd <> "\" }"


-- | Parse the JSON web token returned by PostgREST on /rpc/login.
parseJwt :: Either XhrException XhrResponse -> Maybe AuthToken
parseJwt rsp =
  either
  (const Nothing) -- when the request fails
  (\r -> fmap T.pack $ -- convert String to Text
    join $  -- Just Nothing -> Nothing etc.
    fmap
    (Map.lookup "token") -- get the token
    -- parse the response to a map
    ((decodeXhrResponse r) :: Maybe (Map.Map String String)))
  rsp


-- | Return an 'XhrRequestConfig' for an token.
cfgAuthRq :: Maybe AuthToken -> XhrRequestConfig ()
cfgAuthRq tok = def
  & xhrRequestConfig_headers .~ Map.fromList[("Authorization",
                                              "Bearer " <> (fromMaybe "" tok))]

cfgAuthRqWidget :: (MonadWidget t m,
                    MonadReader l m, AuthModel l t, AuthConfig l)
                   => m (Dynamic t (XhrRequestConfig ()))
cfgAuthRqWidget = do
  tok <- asks authToken
  return $ fmap cfgAuthRq tok
