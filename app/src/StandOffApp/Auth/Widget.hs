{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Auth.Widget
  ( login
  , showToken
  ) where

import Reflex
import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import qualified Data.Aeson as J
import Control.Monad
import Data.Monoid ((<>))
import Data.Default.Class
import Control.Monad.Reader
import Control.Lens

import StandOffApp.Auth.Model

login :: (MonadWidget t m,
          MonadReader l m, AuthConfig l,
          EventWriter t w m, OuterBubble w t, Default w)
         => m ()
login = elClass "div" "form login" $ do
  -- draw login form
  depth :: Int <- asks authHeadlineDepth
  el ("h" <> (T.pack $ show depth)) $ text "Login"
  user <- labelWidget "User Name" "formfield login.username" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "User name")
  pwd <- labelWidget "Password" "formfield login.password" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Password")
  evLogin <- button "Login"
  
  -- Tag in time: See https://obsidian.systems/reflex-nyhug/#/step-26
  let loginData = tag (current (liftM2 (,) (value user) (value pwd))) evLogin

  -- make login request
  meth <- asks loginMethod
  uri <- asks loginUri
  rqCfg <- asks loginRequest
  evRsp <- performRequestAsyncWithError $
    (XhrRequest meth uri . uncurry rqCfg) <$> loginData

  -- parse token form response and pass it to the event writer
  parseTok <- asks parseAuthToken
  let evToken = -- :: Event t (Maybe T.Text) =
        (fmap parseTok evRsp)
  tellEvent $ fmap (const (def & getAuthBubble .~ (AuthEventBubble evToken))) evRsp
  
  -- print the response
  parseErr <- asks parseLoginError
  let evResult = fmap parseErr evRsp
  err <- holdDyn Nothing evResult
  elClass "div" "formerror" $ do
    dynText $ fmap (fromMaybe "") err
  
  return ()


-- | Widget that displays the authentication token.
showToken :: (MonadWidget t m,
              MonadReader l m, AuthModel l t, AuthConfig l)
             => m ()
showToken = elClass "div" "showToken" $ do
  depth :: Int <- asks authHeadlineDepth
  el ("h" <> (T.pack $ show depth)) $ text "Your authentication token"
  tok <- asks authToken
  dynText $ fmap (fromMaybe "") tok


labelWidget :: MonadWidget t m => T.Text -> T.Text -> m a -> m a
labelWidget label clas widget = do
  elClass "div" clas $ do
    elClass "div" "label" $ text label
    widget
