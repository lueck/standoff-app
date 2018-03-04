{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}
module StandOffApp.Bibliography
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe --(fromJust)
import Control.Monad
import Control.Monad.Fix
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


--bibInputWizard :: MonadWidget t m => m ()
bibInputWizard :: ( DomBuilder t m
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , MonadFix m
                  , MonadHold t m
                  , PostBuild t m
                  )
                  => m ()
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
  return ()

--formatEntryBibtex :: MonadWidget t m => Dynamic t Entry -> m ()
formatEntryBibtex e = do
  text "@"
  dynText $ fmap (^.entryType) e
  text "{"
  dynText $ fmap (^.entryKey) e
  text ",\n"
  el "br" blank
  let flds = fmap (^.fields) e
  -- fmap ((mapM formatFld) . (^.fields)) e
  -- fmap (\fs -> (map (formatFields flds)) [0 .. (length fs)]) flds
  -- fmap (sample . current) mapDynM (formatFieldsMap flds) (fmap length flds)
  -- _ <- mapDynM (formatField flds) $ fmap length flds

  --_ <- fmap (sample . current) $
  mapDynM (formatFieldRec flds) $ fmap length flds
  -- (formatFieldRec flds) =<< sample (current (fmap length flds)) 
  
  n :: Int <- sample $ current $ fmap length flds
  --formatField flds =<< (sample $ current $ fmap length flds)
  --fmap (sample . current) $

  --dynText $ fmap (T.pack . show . length) flds
  
  text "}"
  -- where

formatFieldsMap fs n = do
  mapM_ (formatFieldNth fs) [0..n]

-- formatFields :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- formatFields :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m' ()
formatFieldNth fs n = el "div" $ do
  dynText $ fmap (fst . (!! n)) fs
  text "= {"
  dynText $ fmap (snd . (!! n)) fs
  text "},"

-- formatTest :: MonadWidget t m -> Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- formatTest _ 0 = do el "br" blank
formatTest fs n = do 
  text $ T.pack $ show n
  el "br" blank 

--formatField :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
--formatField :: forall m t. (MonadSample t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
--formatField :: forall m t. (DomBuilder t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- formatField :: forall t m. (MonadSample t m, DomBuilder t m, PostBuild t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
formatFieldRec _ 0 = do
  text "no more fields"
  el "br" blank
formatFieldRec fs n = el "div" $ do
  dynText $ fmap (fst . head) fs
  text "= {"
  dynText $ fmap (snd . head) fs
  text "},"
  formatFieldRec (fmap tail fs) (n - 1)
  -- el "br" blank
  

-- formatField :: MonadWidget t m => (T.Text, T.Text) -> m ()
--formatField :: forall t m. (Reflex t, MonadSample t m) => Dynamic t (T.Text, T.Text) -> m ()
formatFieldOFF fld = el "div" $ do
  dynText $ fmap fst fld
  -- _fld <- return $ fst fld
  -- dynText $ wrapDyn $ fst fld
  text "= {"
  dynText $ fmap snd fld
  -- _val <- snd fld
  -- dynText $ wrapDyn $ snd fld
  text "},"
  --return ()

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
