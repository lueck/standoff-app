{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.Xhr
  where

import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe --(fromJust)

entryTypes :: Map.Map T.Text T.Text
entryTypes = Map.fromList [("book", "Book"), ("article", "Article"), ("inproceedings", "Inproceedings")]

-- entryType :: T.Text -> T.Text
-- entryType key = fromJust (Map.lookup key entryTypes)

entryFields :: [(T.Text, T.Text)]
entryFields = [("author", "Author"), ("title", "Title"), ("location" , "Location"), ("year", "Year")]
