{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.PostgRest where

import Reflex.Dom
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Aeson as J
import Data.Aeson ((.:))
import Data.Maybe
import Control.Monad

import StandOffApp.Bibliography.Model


-- * Field types

data PGFieldType =
  PGFieldType
  { _pgfield_entryType :: T.Text
  , _pgfield_fieldType :: T.Text
  , _pgfield_weight :: Int
  } deriving Show

instance J.FromJSON PGFieldType where
  parseJSON = J.withObject "entry_type_field_type" $ \v -> PGFieldType
    <$> v .: "entry_type"
    <*> v .: "field_type"
    <*> v .: "weight"

-- | Create a 'XhrRequest' for getting the bibliographic fields for a
-- specific entry type.
makeTypeFieldsRequest :: T.Text -- ^ base uri
                      -> XhrRequestConfig () -- ^ authenticated request config
                      -> T.Text -- ^ entry type
                      -> XhrRequest ()
makeTypeFieldsRequest baseUri authRq typ =
  XhrRequest "GET" uri rqCfg
  where
    uri = baseUri
          <> "/entry_type_field_type?entry_type=eq."
          <> typ
          <> "&order=weight.nullslast"
    rqCfg = authRq

-- | Create a 'XhrRequest' for getting the bibliographic fields.
makeFieldsRequest :: T.Text -- ^ base uri
                  -> XhrRequestConfig () -- ^ authenticated request config
                  -> XhrRequest ()
makeFieldsRequest baseUri authRq =
  XhrRequest "GET" uri rqCfg
  where
    uri = baseUri <> "/entry_type_field_type"
    rqCfg = authRq

-- | Parse the response from the backend into a mapping of
-- bibliographic field types.
parseFieldsResponse :: Either XhrException XhrResponse -> FieldTypesMap
parseFieldsResponse rsp =
  either
  (const Map.empty)
  (\r -> fromMaybe Map.empty $
     fmap makeFieldsMap
     ((decodeXhrResponse r) :: Maybe [PGFieldType]))
  rsp
  where
    makeFieldsMap = foldr insertField Map.empty
    insertField (PGFieldType e f w) acc = Map.insertWith (<>) e (Map.singleton f w) acc


-- * Entry types

data PGEntryType =
  PGEntryType
  { _pgentry_type :: T.Text
  , _pgentry_weight :: Int
  } deriving Show

instance J.FromJSON PGEntryType where
  parseJSON = J.withObject "entry_type" $ \v -> PGEntryType
    <$> v .: "entry_type"
    <*> v .: "weight"

-- | Create a 'XhrRequest' for getting the bibliographic entries.
makeEntriesRequest :: T.Text -- ^ base uri
                  -> XhrRequestConfig () -- ^ authenticated request config
                  -> XhrRequest ()
makeEntriesRequest baseUri authRq =
  XhrRequest "GET" uri rqCfg
  where
    uri = baseUri <> "/entry_type"
    rqCfg = authRq

-- | Parse the response from the backend into a mapping of
-- bibliographic entry types.
parseEntriesResponse :: Either XhrException XhrResponse -> EntryTypesMap
parseEntriesResponse rsp =
  either
  (const Map.empty)
  (\r -> fromMaybe Map.empty $
     fmap makeEntriesMap
     ((decodeXhrResponse r) :: Maybe [PGEntryType]))
  rsp
  where
    makeEntriesMap = foldr insertEntry Map.empty
    insertEntry (PGEntryType e w) acc = Map.insertWith (const) e w acc
    --insertEntry (PGEntryType e) acc = Map.insertWith const e 1 acc
