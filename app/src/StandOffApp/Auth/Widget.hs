{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Auth.Widget
  where

import Reflex.Dom hiding (element)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import Control.Monad
import Data.Monoid ((<>))

import StandOffApp.Config
import StandOffApp.DomUtils


loginWidget :: MonadWidget t m => m ()
loginWidget = el "div" $ do
  user <- labelWidget "User Name" "login.username" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "User name")
  pwd <- labelWidget "Password" "login.password" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Password")
  evLogin <- button "Login"
  -- See https://obsidian.systems/reflex-nyhug/#/step-26
  let loginData = tag (current (liftM2 (,) (value user) (value pwd))) evLogin 
  evRsp <- performRequestAsync $ uncurry buildReq <$> loginData
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  el "h5" $ text "Response:"
  dynText =<< holdDyn "" evResult
  where
    buildReq usr pwd = XhrRequest "POST" (configUri _cfg_loginPath cfg) rqCfg
      where
        rqCfg = def
                -- & xhrRequestConfig_user .~ (Just usr)
                -- & xhrRequestConfig_password .~ (Just pwd)
                & xhrRequestConfig_sendData .~ (credJson usr pwd)
                & xhrRequestConfig_headers .~ Map.fromList [("Content-Type", "application/json")]
    credJson usr pwd = "{ \"login\": \""
                       <> usr
                       <> "\", \"pwd\": \""
                       <> pwd
                       <> "\" }"
    cfg = defaultConfig -- TODO
