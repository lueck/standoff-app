{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.Bibtex
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader
import Control.Lens

import StandOffApp.Bibliography.Model

-- | Format a bibliographic entry to bibtex. This is a widget and
-- creates a dynamic dom.

bibtexEntry :: (MonadWidget t m,
                MonadReader l m, BiblioModel l t, BiblioConfig l
               ) => Dynamic t Entry -> m ()
bibtexEntry e = el "div" $ do
  text "@"
  dynText $ fmap _entry_type e
  text "{"
  dynText $ fmap _entry_key e
  text ",\n"
  el "table" $ do
    simpleList (fmap (Map.toList . _entry_fields) e) fFld
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
bibtexEntryTxt e = T.concat
  ["@", typ,
  "{", key, ",\n",
  (T.concat $ map (\(k, v) -> T.concat ["\t", k, "\t= {", v, "},\n"]) $ Map.toList flds),
  "}"]
  where
    key = _entry_key e
    typ = _entry_type e
    flds = _entry_fields e
