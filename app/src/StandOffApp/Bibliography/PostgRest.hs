{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOffApp.Bibliography.PostgRest where

import Reflex.Dom
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics
import qualified Data.Aeson as J
import Data.Maybe
import Control.Monad

import StandOffApp.Bibliography.Model


-- * Field types

data PGFieldType =
  PGFieldType
  { entry_type :: T.Text
  , field_type :: T.Text
  , weight :: Int
  } deriving (Generic, Show)

instance J.ToJSON PGFieldType where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON PGFieldType

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
parseFieldsResponse :: Either XhrException XhrResponse -> Fields
--parseFieldsResponse rsp = Map.empty -- FIXME
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
      
