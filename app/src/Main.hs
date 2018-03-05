{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Reflex.Dom

import StandOffApp.Bibliography.Widget
import StandOffApp.Bibliography.TypeDefs (emptyEntry)
import StandOffApp.Bibliography.Bibtex

main :: IO ()
main = mainWidget $ do
  el "h2" $ text "Create New Bibliography Entry"
  entry <- biblioWidget emptyEntry
  -- live output
  el "div" $ do
    bibtexEntry entry
  return ()
