{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.Bibtex
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Fix
import Control.Lens

import StandOffApp.Bibliography.TypeDefs

-- | Format a bibliographic entry to bibtex. This is a widget and
-- creates a dynamic dom.

-- bibtexEntry :: ( DomBuilder t m
--                , DomBuilderSpace m ~ GhcjsDomSpace
--                , MonadFix m
--                , MonadHold t m
--                , PostBuild t m
--                )
--                => Dynamic t Entry
--             -> m ()
bibtexEntry :: MonadWidget t m => Dynamic t Entry -> m ()
bibtexEntry e = el "div" $ do
  text "@"
  dynText $ fmap (^.entryType) e
  text "{"
  dynText $ fmap (^.entryKey) e
  text ",\n"
  el "table" $ do
    simpleList (fmap (^.entryFields) e) fFld
  text "}"
  where
    fFld :: MonadWidget t m => Dynamic t (T.Text, T.Text) -> m ()
    fFld f = el "tr" $ do
      el "td" $ do
        dynText $ fmap fst f
      el "td" $ do
        text "="
      el "td" $ do
        text "{"
        dynText $ fmap snd f
        text "},\n"

-- | A pure Funktion that formats a bibliographic entry to bibtex.
bibtexEntryTxt :: Entry -> T.Text
bibtexEntryTxt (Entry key typ flds) = T.concat
  ["@", typ,
  "{", key, ",\n",
  (T.concat $ map (\(k, v) -> T.concat ["\t", k, "\t= {", v, "},\n"]) flds),
  "}"]
  
