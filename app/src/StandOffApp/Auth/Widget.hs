{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Auth.Widget
  where

import Reflex
import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import Control.Monad
import Data.Monoid ((<>))
import Data.Default.Class
import Control.Monad.Reader
import Control.Lens

import StandOffApp.Auth.Model

loginWidget :: (MonadWidget t m,
                MonadReader l m, AuthConfig l,
                EventWriter t w m, OuterBubble w t, Default w) => m ()
loginWidget = el "div" $ do
  user <- labelWidget "User Name" "login.username" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "User name")
  pwd <- labelWidget "Password" "login.password" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Password")
  evLogin <- button "Login"
  -- See https://obsidian.systems/reflex-nyhug/#/step-26
  let loginData = tag (current (liftM2 (,) (value user) (value pwd))) evLogin
  meth <- asks loginMethod
  uri <- asks loginUri
  evRsp <- performRequestAsyncWithError $
    (XhrRequest meth uri . uncurry rqCfg) <$> loginData
  parseTok <- asks parseAuthToken
  let evToken = -- :: Event t (Maybe T.Text) =
        (fmap parseTok evRsp)

  tellEvent $ fmap (const (def & getAuthBubble .~ (AuthEventBubble evToken))) evRsp
  
  -- -- print the response
  -- parseErr <- asks parseError
  -- let evResult = fmap parseErr evRsp
  -- el "div" $ do
  --   text "Response:"
  --   el "br" blank
  --   dynText =<< holdDyn "" evResult
  
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

-- | Widget that displays the authentication token.
showToken :: (AuthModel l t, MonadWidget t m, MonadReader l m) => m ()
showToken = el "div" $ do
  el "h2" $ text "Your authentication token"
  tok <- asks authToken
  dynText $ fmap (fromMaybe "") tok

--labelWidget :: MonadWidget t m => T.Text -> T.Text -> k -> m a
labelWidget label xid widget = do
  el "div" $ do
    el "div" $ text label
    widget

-- | Show the whole response body.
showRsp :: Either XhrException XhrResponse -> Maybe T.Text
showRsp rsp =
  either
  (Just . T.pack . show)
  (Just . T.pack . show . _xhrResponse_responseText)
  rsp
