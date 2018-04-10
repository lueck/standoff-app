{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module StandOffApp.Bibliography.Form
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe
import Data.Monoid ((<>))
import Control.Monad
import Control.Lens
import Control.Monad.Reader
import Data.List
import Data.Default.Class

import StandOffApp.Bibliography.Model
import StandOffApp.Bibliography.Xhr
import StandOffApp.Bibliography.DynamicList


-- | A form for creating or modifying a bibliographic entry. This is
-- to be reused by a from with a submit button.
biblioForm
  :: (MonadWidget t m,
      MonadReader l m, BiblioModel l t, BiblioConfig l,
      EventWriter t w m, OuterBubble w t, Default w
     ) => Entry                           -- ^ initial entry
  -> m (Dynamic t Entry) -- ^ returns the modified/new entry
biblioForm initEntry = elClass "div" "form bibliographicEntry" $ do
  -- Draw form fields for key and entry type 
  key <- labelWidget "Entry Key" "bibInputWizard.entryKey" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Key")
  typEv <- labelWidget "Entry Type" "bibInputWizard.entryType" $
         dropdown "book" (constDyn entryTypes) def

  -- get the field types for this entry type
  allFlds :: Dynamic t FieldTypes <- asks biblioFieldTypes
  let typeFlds = zipDynWith
                 (\t fs -> fromMaybe [] $ Map.lookup t fs)
                 (value typEv)
                 allFlds
      -- add labels. TODO: add real labels
      typeFldsList = fmap (map (\k -> (k, k))) typeFlds
      typeFldsMap = fmap Map.fromList  typeFldsList

  --addRow :: (Reflex t) => Dynamic t Int -> Event t (T.Text, T.Text)
  let addRow cnt = updated $ zipDynWith (\fs n ->
                                            (\f -> (f, "")) $
                                            (fromMaybe "unknown") $
                                            (fmap fst) $
                                            (fs ^? element n)
                                        ) typeFldsList cnt

  -- draw the form of fields
  fields :: Dynamic t [(T.Text, T.Text)] <-
    labelWidget "Entry Fields" "bibInputWizard.entryFields" $ do
    rec
      fldCnt :: Dynamic t Int <-
        foldDyn (+) (-1 :: Int) (1 <$ evIncrFldCnt)
      evRows :: Event t (Dynamic t [(Dynamic t T.Text, Dynamic t T.Text, Event t ())]) <-
        -- dyn wraps it into an event
        dyn ((\fs -> dynamicList (fieldWidget fs typeFldsMap) extractRemoveEv (const never) (addRow fldCnt) initFields) <$> typeFldsList)
      evIncrFldCnt :: Event t () <-
        button "+"
      -- unwrap the event, by 1) making a dynamic out of it, 2) join it
      rows <- holdDyn (constDyn []) evRows
    return $ join $ fmap (sequence . (map (\(k, v, _) -> (liftM2 (,) k v)))) $ join rows

  -- make a map from the list returned by the fields widget and make the entry
  let fieldsMap = fmap (Map.fromListWith (\ a b -> a <> " and " <> b)) fields
  let entry = -- :: Dynamic t Entry =
        liftM3 Entry (value key) (value typEv) fieldsMap
  return entry
  where
    initKey = _entry_key initEntry
    initType = _entry_type initEntry
    initFields = Map.toList $ _entry_fields initEntry
    extractRemoveEv :: (Dynamic t T.Text, Dynamic t T.Text, Event t ())
                    -> Event t ()
    extractRemoveEv = (\(_, _, e) -> e)

-- | A subform for a bibliographic field.
fieldWidget :: MonadWidget t m
               => [(T.Text, T.Text)] -- ^ the list of fields and labels
            -> Dynamic t (Map.Map T.Text T.Text) -- ^ fields and labels as map
            -> Int -- ^ number of this field
            -> (T.Text, T.Text) -- ^ default values
            -> Event t (T.Text, T.Text) -- ^ 
            -> m (Dynamic t T.Text, Dynamic t T.Text, Event t ())
fieldWidget flds fldsMap n initVal changedVal = el "div" $ do
  fld <- dropdown (fieldKey n) fldsMap def
  val <- textInput def -- $
         --def & attributes .~ constDyn ("placeholder" =: "field value")
  evDel <- button "-"
  return ((value fld), (value val), evDel)
  where
    fieldKey :: Int -> T.Text
    fieldKey n = fromMaybe "unkown" (fmap fst (flds ^? element n))
    --fieldKey n = fmap (fromMaybe "unkown" . (fmap fst) . (^? element n)) flds

labelWidget :: MonadWidget t m => T.Text -- ^ Label for user interface
            -> T.Text -- ^ css class of div container
            -> m a -- ^ widget
            -> m a
labelWidget label clas widget = do
  elClass "div" clas $ do
    elClass "div" "label" $ text label
    widget
