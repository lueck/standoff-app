{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Bibliography
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe --(fromJust)
import Control.Monad
import Control.Lens

--import qualified StandOff.Bibliography as CB

import StandOffApp.DomUtils

data Entry
  = Entry
  { _entryType :: T.Text           -- ^ e.g. book, article
  , _entryKey :: T.Text            -- ^ the entry's (bibtex) key
  , _fields :: [(T.Text, T.Text)]  -- ^ the fields. This is a list of
                                  -- key-value tuples.
  }
makeLenses ''Entry


bibInputWizard :: MonadWidget t m => m ()
bibInputWizard = el "div" $ do
  let entryFieldsMap = Map.fromList entryFields
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
      formatEntryBibtex entry
      -- dynText $ entry2BibtexPure entry
  return ()

formatEntryBibtex :: MonadWidget t m => Dynamic t Entry -> m ()
formatEntryBibtex e = do
  text "@"
  dynText $ fmap (^.entryType) e
  text "{"
  dynText $ fmap (^.entryKey) e
  text ",\n"
  el "br" blank
  -- let flds :: Dynamic t [(T.Text, T.Text)]
  --     flds = fmap (^.fields) e
  -- flds :: Dynamic t [(T.Text, T.Text)] ->
  --         fmap (^.fields) e
  -- let z =  fmap (mapM_ formatField) $ fmap (^.fields) e
  mapDynM formatField $ fmap (^.fields) e
  text "}"

-- formatField :: MonadWidget t m => (T.Text, T.Text) -> m ()
formatField :: forall t m. (Reflex t, MonadSample t m) => (T.Text, T.Text) -> m ()
formatField fld = el "div" $ do
  --dynText $ fmap $ fst fld
  -- _fld <- return $ fst fld
  -- dynText $ wrapDyn $ fst fld
  text "= {"
  -- dynText $ fmap $ snd fld
  -- _val <- snd fld
  -- dynText $ wrapDyn $ snd fld
  text "},"
  return ()

wrapDyn :: MonadWidget t m => a -> m (a)
wrapDyn v = do
  return v


entry2BibtexPure :: Entry -> T.Text
entry2BibtexPure (Entry key typ flds) = T.concat
  ["@", typ,
  "{", key, ",\n",
  (T.concat $ map (\(k, v) -> T.concat ["\t", k, "\t= {", v, "},\n"]) flds),
  "}"]
  

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


entryTypes :: Map.Map T.Text T.Text
entryTypes = Map.fromList [("book", "Book"), ("article", "Article"), ("inproceedings", "Inproceedings")]

-- entryType :: T.Text -> T.Text
-- entryType key = fromJust (Map.lookup key entryTypes)

entryFields :: [(T.Text, T.Text)]
entryFields = [("author", "Author"), ("title", "Title"), ("location" , "Location"), ("year", "Year")]
