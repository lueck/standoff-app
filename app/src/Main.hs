{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Reflex.Dom
import Reflex
import Control.Monad.Reader

import StandOffApp.Config
import StandOffApp.Model

import StandOffApp.Bibliography.Widget
import StandOffApp.Bibliography.TypeDefs (emptyEntry)
import StandOffApp.Bibliography.Bibtex
import StandOffApp.Bibliography.Format
import StandOffApp.Auth.Widget

main :: IO ()
main = mainWidget (appWidget defaultConfig)

-- | The App's main widget.
appWidget :: MonadWidget t m => AppConfig -> m ()
appWidget conf = do
  rec
    model <- appModel aggregate conf
    (_, aggregate) <- runDynamicWriterT $ flip runReaderT model $ do
      appView
  pure ()
  --return ()

appView :: (MonadReader (Model t) m, MonadDynamicWriter t (Aggregate t) m, MonadWidget t m) => m ()
--appView :: (MonadReader (Model t) m, EventWriter t (Aggregate t) m, MonadWidget t m) => m ()
appView = do
  el "h2" $ text "Login"
  loginWidget

  showToken
  
  -- el "h2" $ text "Create New Bibliography Entry"
  -- entry <- biblioWidget emptyEntry
  -- -- live output
  -- el "div" $ do
  --   format entry
  -- el "div" $ do
  --   bibtexEntry entry

  return ()
