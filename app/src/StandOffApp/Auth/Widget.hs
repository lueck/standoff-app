{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Auth.Widget
  where

import Reflex.Dom hiding (element)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import Control.Monad
import Data.Monoid ((<>))
import Data.Aeson (decode)


import StandOffApp.Config
import StandOffApp.DomUtils


loginWidget :: MonadWidget t m => m (Dynamic t (Maybe T.Text))
loginWidget = el "div" $ do
  user <- labelWidget "User Name" "login.username" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "User name")
  pwd <- labelWidget "Password" "login.password" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Password")
  evLogin <- button "Login"
  -- See https://obsidian.systems/reflex-nyhug/#/step-26
  let loginData = tag (current (liftM2 (,) (value user) (value pwd))) evLogin
  evRsp <- performRequestAsyncWithError $ uncurry buildReq <$> loginData
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
  token :: Dynamic t (Maybe T.Text) <- holdDyn Nothing evToken

  -- print the response
  let evResult = (either (T.pack . show) (fromMaybe "" . _xhrResponse_responseText)) <$> evRsp
  el "div" $ do
    text "Response:"
    el "br" blank
    dynText =<< holdDyn "" evResult
  el "div" $ do
    text "Your Authentication Token: "
    el "br" blank
    dynText $ fmap (fromMaybe "") token
  
  -- return the token
  return token
  where
    buildReq usr pwd = XhrRequest "POST" (configUri _cfg_loginPath cfg) rqCfg
      where
        rqCfg = def
                & xhrRequestConfig_sendData .~ (credJson usr pwd)
                & xhrRequestConfig_headers .~ Map.fromList [("Content-Type", "application/json")]
    credJson usr pwd = "{ \"login\": \""
                       <> usr
                       <> "\", \"pwd\": \""
                       <> pwd
                       <> "\" }"
    cfg = defaultConfig -- TODO
