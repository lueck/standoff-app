{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.TypeDefs
  where

import Data.Text
import qualified Data.Map as Map
import Control.Lens

-- | A bibliographic record. Like in bibtex this has a key and type
-- and a set of key value pairs.
data Entry
  = Entry
  { _entry_key :: Text                 -- ^ the entry's (bibtex) key
  , _entry_type :: Text                -- ^ e.g. book, article
  , _entry_fields :: Map.Map Text Text -- ^ the fields. This is a list
                    -- of key value pairs.
  }

makeLenses ''Entry

emptyEntry :: Entry
emptyEntry = Entry "" "" Map.empty
