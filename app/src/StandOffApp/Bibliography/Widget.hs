{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}
module StandOffApp.Bibliography.Widget
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe --(fromJust)
import Control.Monad
import Control.Monad.Fix
import Control.Lens

import StandOffApp.DomUtils
import StandOffApp.Bibliography.Xhr
import StandOffApp.Bibliography.TypeDefs
import StandOffApp.Bibliography.Bibtex
import StandOffApp.DynamicList


-- not working with this signature: --
-- bibInputWizard :: ( DomBuilder t m
--                   , DomBuilderSpace m ~ GhcjsDomSpace
--                   , MonadFix m
--                   , MonadHold t m
--                   , PostBuild t m
--                   )
--                   => m ()
bibInputWizard :: MonadWidget t m => m ()
bibInputWizard = el "div" $ do
  let initFlds = []
  rec
    el "h2" $ text "Create New Bibliography Entry"
    key <- labelWidget "Entry Key" "bibInputWizard.entryKey" $
           textInput $ def & attributes .~ constDyn ("placeholder" =: "Key")
    typ <- labelWidget "Entry Type" "bibInputWizard.entryType" $
           dropdown "book" (constDyn entryTypes) def

    -- Fields
    fields :: Dynamic t [(T.Text, T.Text)] <-
      labelWidget "Entry Fields" "bibInputWizard.entryFields" $ do
      rec
        fldCnt :: Dynamic t Int <-
          foldDyn (+) (-1 :: Int) (1 <$ evIncrFldCnt)
        rows :: Dynamic t [(Dynamic t T.Text, Dynamic t T.Text, Event t ())] <-
          dynamicList fieldWidget extractRemoveEv (const never) (addRow fldCnt) initFlds
        evIncrFldCnt :: Event t () <-
          button "+"
      return $ join $ fmap (sequence . (map (\(k, v, _) -> (liftM2 (,) k v)))) rows
      
    let entry = -- :: Dynamic t Entry =
          liftM3 Entry (value typ) (value key) fields

    -- live output
    --el "div" $ do
    --  bibtexEntry entry
  return ()
  where
    entryFieldsMap = Map.fromList entryFields
    fieldWidget
      :: MonadWidget t m
      => Int
      -> (T.Text, T.Text)
      -> Event t (T.Text, T.Text)
      -> m (Dynamic t T.Text, Dynamic t T.Text, Event t ())
    fieldWidget n initVal changedVal = el "div" $ do
      fld <- dropdown (fieldKey n) (constDyn entryFieldsMap) def
      val <- textInput def -- $
             --def & attributes .~ constDyn ("placeholder" =: "field value")
      evDel <- button "-"
      return ((value fld), (value val), evDel)
    extractRemoveEv :: (Dynamic t T.Text, Dynamic t T.Text, Event t ())
                    -> Event t ()
    extractRemoveEv = (\(_, _, e) -> e)
    addRow :: (Reflex t) => Dynamic t Int -> Event t (T.Text, T.Text)
    addRow cnt = fmap (\n -> (fieldKey n, "")) $ updated cnt
    fieldKey :: Int -> T.Text
    fieldKey n = fromMaybe "unkown" (fmap fst (entryFields ^? element n))
