{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)

import StandOffApp.Form

main :: IO ()
main = mainWidget bibInputWizard

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

