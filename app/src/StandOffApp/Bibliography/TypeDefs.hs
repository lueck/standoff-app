{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.TypeDefs
  where

import qualified Data.Text as T
import Control.Lens

data Entry
  = Entry
  { _entryKey :: T.Text            -- ^ the entry's (bibtex) key
  , _entryType :: T.Text           -- ^ e.g. book, article
  , _entryFields :: [(T.Text, T.Text)]  -- ^ the fields. This is a list of
                                   -- key-value tuples.
  }

makeLenses ''Entry

emptyEntry :: Entry
emptyEntry = Entry "" "" []
