{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module StandOffApp.Bibliography
  where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)
import Control.Monad

import StandOffApp.DomUtils



bibInputWizard :: MonadWidget t m => m ()
bibInputWizard = el "div" $ do
  el "h2" $ text "Create New Bibliography Entry"
  key <- labelWidget "Entry Key" "bibInputWizard.entryKey" $
         textInput $ def & attributes .~ constDyn ("placeholder" =: "Key") 
  typ <- labelWidget "Entry Type" "bibInputWizard.entryType" $
         dropdown "book" (constDyn entryTypes) def
  let selTyp = entryType <$> value typ
  el "div" $ do
    text "You selected '"
    dynText $ value key
    text "' to be of type '"
    dynText selTyp
    text "'."
  (fld1, val1) <- bibFieldInput "author"
  (fld2, val2) <- bibFieldInput "title"
  (fld3, val3) <- bibFieldInput "year"
  el "br" blank
  dynText fld1
  text " = {"
  dynText val1
  text "}"
  --fldsCnt <- count =<< button "+"
  flds <- replicateM 4 $ bibFieldInput "location" -- Works
  -- flds <- (flip replicateM $ bibFieldInput "location") =<< fldsCnt -- Err
  text "Hello"
  --
  el "br" blank
  -- Create the button.
  addNewField :: Event t () <- button "+"
  -- Number of clicks of the button.
  latestId :: Dynamic t Int <- count addNewField
  -- On each addNewField event add a new field with Just "blank" field
  -- type.
  let modifyChildren :: Event t (Map.Map Int (Maybe ()))
      modifyChildren = fmap (\n -> n =: Just ()) $ updated latestId
  -- Draw the list of elements.
  fields :: Dynamic t (Map.Map Int (Dynamic t T.Text,  Dynamic t T.Text)) <- listHoldWithKey mempty modifyChildren $ \n _ -> do
    -- Draw a single textbox, and return its current value as a Dynamic t Text
    bibFieldInput "blank"
  -- Combine the inner Dynamics into the outer one
  -- let combinedValues :: Dynamic t (Map.Map Int (Dynamic t T.Text, Dynamic t T.Text))
  --     combinedValues = joinDynThroughMap fields
  -- -- Show the results as text
  -- display combinedValues
  el "br" blank
  

bibFieldInput k = do
  el "div" $ do
    fld <- dropdown k (constDyn entryFields) def
    val <- textInput def -- $
           --def & attributes .~ constDyn ("placeholder" =: "field value")
    let _fld = entryField <$> value fld
        _val = _textInput_value val
    return (_fld, _val)


entryTypes :: Map.Map T.Text T.Text
entryTypes = Map.fromList [("book", "Book"), ("article", "Article"), ("inproceedings", "Inproceedings")]

entryType :: T.Text -> T.Text
entryType key = fromJust (Map.lookup key entryTypes)

entryFields :: Map.Map T.Text T.Text
entryFields = Map.fromList [("author", "Author"), ("title", "Title"), ("location" , "Location"), ("year", "Year")]

entryField :: T.Text -> T.Text
entryField key = fromJust (Map.lookup key entryFields)
