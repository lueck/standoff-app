{-# LANGUAGE TemplateHaskell #-}
module StandOffApp.Bibliography.TypeDefs
  where

import qualified Data.Text as T
import Control.Lens

data Entry
  = Entry
  { _entryType :: T.Text           -- ^ e.g. book, article
  , _entryKey :: T.Text            -- ^ the entry's (bibtex) key
  , _fields :: [(T.Text, T.Text)]  -- ^ the fields. This is a list of
                                   -- key-value tuples.
  }

makeLenses ''Entry

