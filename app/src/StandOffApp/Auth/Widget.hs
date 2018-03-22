{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module StandOffApp.Auth.Widget
  where

import Reflex
import Reflex.Dom hiding (element)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import Control.Monad
import Data.Monoid ((<>))
import Data.Aeson (decode)
import Control.Monad.Reader

import StandOffApp.Model
import StandOffApp.ConfigClassDefs
import StandOffApp.Auth.ClassDefs
import StandOffApp.DomUtils

import StandOffApp.Config


loginWidget :: (MonadReader (Model t) m, EventWriter t (Aggregate t) m, MonadWidget t m) => m ()
loginWidget = el "div" $ do
  user <- labelWidget "User Name" "login.username" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "User name")
  pwd <- labelWidget "Password" "login.password" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Password")
  evLogin <- button "Login"
  -- See https://obsidian.systems/reflex-nyhug/#/step-26
  let loginData = tag (current (liftM2 (,) (value user) (value pwd))) evLogin
  meth <- asks (loginMethod . _model_config)
  uri <- asks ((absPath loginPath) . _model_config)
  evRsp <- performRequestAsyncWithError $
    (XhrRequest meth uri . uncurry rqCfg) <$> loginData
  let evToken = -- :: Event t (Maybe T.Text) =
        (fmap (either
                (const Nothing) -- when the request fails
                (\r -> fmap T.pack $ -- convert String to Text
                       join $  -- Just Nothing -> Nothing etc.
                       fmap
                       (Map.lookup "token") -- get the token
                       -- parse the response to a map
                       ((decodeXhrResponse r) :: Maybe (Map.Map String String))))
         evRsp)
  tellEvent $ fmap (const $ Aggregate evToken) evRsp
  
  -- print the response
  let evResult = (either (T.pack . show) (fromMaybe "" . _xhrResponse_responseText)) <$> evRsp
  el "div" $ do
    text "Response:"
    el "br" blank
    dynText =<< holdDyn "" evResult
  
  return ()
  where
    rqCfg usr pwd = def
      & xhrRequestConfig_sendData .~ (credJson usr pwd)
      & xhrRequestConfig_headers .~ Map.fromList [("Content-Type", "application/json")]
    credJson usr pwd = "{ \"login\": \""
                       <> usr
                       <> "\", \"pwd\": \""
                       <> pwd
                       <> "\" }"

showToken :: (Reflex t, MonadReader (Model t) m, EventWriter t (Aggregate t) m, MonadWidget t m) => m ()
showToken = el "div" $ do
  el "h2" $ do
    text "Your authentication token"
  tok <- asks _model_authToken
  dynText $ fmap (fromMaybe "") tok
