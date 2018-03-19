{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Reflex.Dom
import Control.Monad.Trans.Reader

import StandOffApp.Config

import StandOffApp.Bibliography.Widget
import StandOffApp.Bibliography.TypeDefs (emptyEntry)
import StandOffApp.Bibliography.Bibtex
import StandOffApp.Bibliography.Format
import StandOffApp.Auth.Widget

main :: IO ()
main = mainWidget $ do
  runReaderT appWidget defaultConfig

-- | The App's main widget.
appWidget :: MonadWidget t m => ReaderT AppConfig m ()
appWidget = do

  el "h2" $ text "Login"
  loginWidget
  
  el "h2" $ text "Create New Bibliography Entry"
  entry <- biblioWidget emptyEntry
  -- live output
  el "div" $ do
    format entry
  el "div" $ do
    bibtexEntry entry

  return ()
