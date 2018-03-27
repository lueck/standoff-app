{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module StandOffApp.Auth.Widget
  where

import Reflex
import Reflex.Dom hiding (element)
--import Lens.Micro
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)
import Control.Monad
import Data.Monoid ((<>))
import Data.Aeson (decode)
import Data.Default.Class
import Control.Monad.Reader
import Data.Semigroup hiding ((<>))

import StandOffApp.Auth.Model
import StandOffApp.ConfigClassDefs
import StandOffApp.Model


loginWidget :: (AuthModel l, MonadWidget t m, Semigroup w, Default w, EventBubbleC w, MonadReader l m, EventWriter t w m) => m ()
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

  -- push the token to the event writer
  --getBubble <- asks authEventBubble
  -- tellEvent $ fmap (const $ def -- (def :: (Semigroup w) => w)
  --                    & getBubble .~ (def
  --                                     & authEvBub_evToken .~ evToken)) evRsp
  let authBub =
        fmap (const $ defaultAuthEventBubble & authEvBub_evToken .~ evToken) evRsp --  :: (Reflex t) => AuthEventBubble t
  -- tellEvent $ fmap (const $ def
  --                    & evBub_authBubble .~ ) evRsp
  
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
showToken :: (AuthModel l, MonadWidget t m, Semigroup w, MonadReader l m, EventWriter t w m) => m ()
showToken = el "div" $ do
  el "h2" $ do
    text "Your authentication token"
  -- tok <- asks authToken
  -- dynText $ fmap (fromMaybe "") tok

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
