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


--bibInputWizard :: MonadWidget t m => m ()
bibInputWizard :: ( DomBuilder t m
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , MonadFix m
                  , MonadHold t m
                  , PostBuild t m
                  )
                  => m ()
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
      
    let entry :: Dynamic t Entry =
          liftM3 Entry (value typ) (value key) fields

    -- live output
    --el "div" $ do
    --  bibtexEntry entry
  return ()
  where
    entryFieldsMap = Map.fromList entryFields
    fieldWidget :: Int
                -> (T.Text, T.Text)
                -> Event t (T.Text, T.Text)
                -> Dynamic t (Dynamic t T.Text, Dynamic t T.Text, Event t ()) 
    fieldWidget n initVal changedVal = el "div" $ do
      --text $ T.pack $ show n
      fld <- dropdown (fieldKey n) (constDyn entryFieldsMap) def
      val <- textInput def -- $
             --def & attributes .~ constDyn ("placeholder" =: "field value")
      evDel <- button "-"
      --delFldN :: Dynamic t Int <- foldDyn (*) (1 :: Int) (n <$ evDel)
      return ((value fld), (value val), evDel)
    extractRemoveEv :: (Dynamic t T.Text, Dynamic t T.Text, Event t ())
                    -> Event t ()
    extractRemoveEv = (\(_, _, e) -> e)
    addRow :: Dynamic t Int -> Event t (T.Text, T.Text)
    addRow cnt = fmap (\n -> (fieldKey n, "")) $ updated cnt
    fieldKey :: Int -> T.Text
    fieldKey n = fromMaybe "unkown" (fmap fst (entryFields ^? element n))

--bibInputWizard :: MonadWidget t m => m ()
bibInputWizardOFF :: ( DomBuilder t m
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , MonadFix m
                  , MonadHold t m
                  , PostBuild t m
                  )
                  => m ()
bibInputWizardOFF = el "div" $ do
  let entryFieldsMap = Map.fromList entryFields
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
        -- starting at -1 to get index on entryFields right
        fldCnt :: Dynamic t Int <- foldDyn (+) (-1 :: Int) (1 <$ evIncrFldCnt)
        -- On each addNewField event add a new field with Just "blank"
        -- field type.
        let -- modifyChildren :: Event t (Map.Map Int (Maybe ()))
          modifyChildren = fmap (\n -> n =: Just ()) $ updated fldCnt
        rows :: Dynamic t (Map.Map Int (Dynamic t T.Text, Dynamic t T.Text, Dynamic t Int)) <-
          listHoldWithKey mempty modifyChildren $ \n _ -> el "div" $ do
          -- Draw a row of dropdown, text input and deletion button.
          let k = fromMaybe "unkown" (fmap fst (entryFields ^? element n))
          el "div" $ do
            fld <- dropdown k (constDyn entryFieldsMap) def
            val <- textInput def -- $
                   --def & attributes .~ constDyn ("placeholder" =: "field value")
            evDel <- button "-"
            text $ T.pack $ show n
            delFldN :: Dynamic t Int <- foldDyn (*) (1 :: Int) (n <$ evDel)
            return ((value fld), (value val), (n <$ delFldN))
        -- Draw button to add a new row.
        evIncrFldCnt <- button "+"
        rowDels :: Dynamic t [Dynamic t Int] <-
          return $ fmap (Map.foldr (\(_, _, d) acc -> d : acc) []) rows
      return $ join $ fmap (sequence . (Map.foldr (\(k, v, _) acc -> (liftM2 (,) k v) : acc) [])) rows
    let entry = liftM3 Entry (value typ) (value key) fields
    -- live output
    el "div" $ do
      bibtexEntry entry
  return ()

-- | A dynamically changing set of widgets

-- Alternatives: reflex's collections use a Map of key/value pairs for
-- a list of widgets. Should we use a) field type as key or b) an
-- integer as key and a then tuple of Text Text as value?
--
-- b) would make widgets with the same field type possible. Then we
-- have to combine values.
--
-- a) makes logic complicated. How to handle same field type? Making
-- same field type impossible would be possible but complicated. Would
-- that be user-friendly?
bibInputFields :: MonadWidget t m => TextInputConfig t -> m ()
--bibInputFields :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => TextInputConfig t -> m ([(T.Text, T.Text)] t)
bibInputFields (TextInputConfig inputType initial eSetValue dAttrs) = do
  let entryFieldsMap = Map.fromList entryFields
  rec
    -- starting at -1 to get index on entryFields right
    fldCnt :: Dynamic t Int <- foldDyn (+) (-1 :: Int) (1 <$ evIncrFldCnt)
    -- On each addNewField event add a new field with Just "blank"
    -- field type.
    let -- modifyChildren :: Event t (Map.Map Int (Maybe ()))
        modifyChildren = fmap (\n -> n =: Just ()) $ updated fldCnt
    -- Draw the dynamic collection of widgets.
    rows :: Dynamic t (Map.Map Int (Dynamic t T.Text, Dynamic t T.Text, Dynamic t Int)) <-
      listHoldWithKey mempty modifyChildren $ \n _ -> el "div" $ do
      -- Draw a row of dropdown, text input and deletion button.
      let k = fromMaybe "unkown" (fmap fst (entryFields ^? element n))
      el "div" $ do
        fld <- dropdown k (constDyn entryFieldsMap) def
        val <- textInput def -- $
               --def & attributes .~ constDyn ("placeholder" =: "field value")
        evDel <- button "-"
        text $ T.pack $ show n
        delFldN :: Dynamic t Int <- foldDyn (*) (1 :: Int) (n <$ evDel)
        return ((value fld), (value val), (n <$ delFldN))
    -- Draw button to add a new row.
    evIncrFldCnt <- button "+"
    -- get deletion events from field rows
    rowDels :: Dynamic t [Dynamic t Int] <-
      return $ fmap (Map.foldr (\(_, _, d) acc -> d : acc) []) rows
    fields :: Dynamic t [(T.Text, T.Text)] <-
      -- howto lift [] into Dynamic t for (liftM2 (:) (liftM2 (,) k v) acc)?
      return $ join $ fmap (sequence . (Map.foldr (\(k, v, _) acc -> (liftM2 (,) k v) : acc) [])) rows
  return ()


bibFieldInput k n fldsMap = do
  el "div" $ do
    text $ T.pack $ show n
    fld <- dropdown k (constDyn fldsMap) def
    val <- textInput def -- $
           --def & attributes .~ constDyn ("placeholder" =: "field value")
    evDel <- button "-"
    return ((value fld), (value val), (n <$ evDel))

