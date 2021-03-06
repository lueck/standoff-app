{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Bibliography.Model where


import Reflex
import Reflex.Dom
import Data.Text hiding (map)
import Data.Default.Class
import Control.Lens
import Data.Semigroup
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List


-- * Model

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

-- | A list of entry types ordered by weight. 
type EntryTypes = [Text]

-- | Map of entry types and weights for the widget.
type EntryTypesMap = Map.Map Text Int

-- | A mapping of lists field types (values) per entry type (keys).
type FieldTypes = Map.Map Text [Text]

-- | Map of entry types, which's values are maps of field name and
-- field weight.
--
-- [("book", [("author", 1), ("title", 10), ...]), ("article", [...]),
-- ...]
type FieldTypesMap = Map.Map Text (Map.Map Text Int)

-- | Map of field names and field labels
type FieldLabels = Map.Map Text Text

-- | A record of values that are accessible throughout the whole app.
data BiblioData t = BiblioData
  { _biblio_entryTypes :: Dynamic t EntryTypes
  , _biblio_entryTypesMap :: Dynamic t EntryTypesMap
  , _biblio_fieldTypes :: Dynamic t FieldTypes
  , _biblio_fieldTypesMap :: Dynamic t FieldTypesMap
  }

-- | Configuration of the bibliography module. Note, that the
-- functions are intended to be passed the model, not the
-- model.config. Functions parametrized with a reflex time line should
-- go to 'BiblioModel'.
class BiblioConfig m where
  baseUri :: m -> Text -- ^ uri (base uri + path) of login rpc
  biblioHeadlineDepth :: m -> Int -- ^ Whether "h2", "h3" in widgets.
  parseError :: m -> (Either XhrException XhrResponse -> Maybe Text) -- ^ Function for parsing the xhr response to an error message.
  -- * entries
  parseEntries :: m -> (Either XhrException XhrResponse -> EntryTypesMap) -- ^ Function for parsing the entries from a xhr response.
  entriesRequest :: m -> (Text -> XhrRequestConfig () -> XhrRequest ())
  -- * fields
  parseFields :: m -> (Either XhrException XhrResponse -> FieldTypesMap) -- ^ Function for parsing the fields from a xhr response.
  fieldsRequest :: m -> (Text -> XhrRequestConfig () -> XhrRequest ())

-- | Like 'BiblioConfig' but parametrized with reflex time line.
class BiblioModel m t where
  authRequestConfig :: m -> Dynamic t (XhrRequestConfig ()) -- ^ config of an authenticated request
  biblioEntryTypes :: m -> Dynamic t EntryTypes -- ^ the types of bibliographic entries
  biblioFieldTypes :: m -> Dynamic t FieldTypes -- ^ the types of bibliographic fields

-- | Return 'BiblioData' from a 'BiblioEventBubble'.
biblioModel :: MonadWidget t m => Event t (BiblioEventBubble t) -> m (BiblioData t)
biblioModel bubble = do
  -- extract entry types event
  let evEntryTypes = coincidence (_biblioEvBub_evEntryTypes <$> bubble)
  entTypesMap <- holdDyn Map.empty evEntryTypes
  let entTypesWeightOrder = fmap ((sortOn snd) . (Map.foldrWithKey (\k w acc -> (k,w):acc) [])) entTypesMap
      entTypesList = fmap (map fst) entTypesWeightOrder
  -- extract field types event
  let evFieldTypes = coincidence (_biblioEvBub_evFieldTypes <$> bubble)
  fldTypesMap <- holdDyn Map.empty evFieldTypes
  let fldTypesWeightOrder = fmap (Map.map ((sortOn snd) . (Map.foldrWithKey (\k w acc -> (k,w):acc) []))) fldTypesMap
      fldTypesList = fmap (Map.map (map fst)) fldTypesWeightOrder
  -- to model
  return $ BiblioData
    { _biblio_entryTypesMap = entTypesMap
    , _biblio_entryTypes = entTypesList
    , _biblio_fieldTypesMap = fldTypesMap
    , _biblio_fieldTypes = fldTypesList
    }

-- * EventBubble

-- | A record for all the events (values) that have to go up in the
-- dom to be made accessible throughout the app.
data BiblioEventBubble t = BiblioEventBubble
  { _biblioEvBub_evEntryTypes :: Event t EntryTypesMap -- ^ fired when xhr returned entries
  , _biblioEvBub_evFieldTypes :: Event t FieldTypesMap -- ^ fired when xhr returned fields
  }

makeLenses ''BiblioEventBubble

-- | The EventBubble must be an instance of 'Default', so that it can
-- easily be created by events pushed into the event writer by other
-- modules.
instance (Reflex t) => Default (BiblioEventBubble t) where
  def = BiblioEventBubble
        { _biblioEvBub_evEntryTypes = never
        , _biblioEvBub_evFieldTypes = never
        }

-- | Combination of events in the event writer.
instance (Reflex t) => Semigroup (BiblioEventBubble t) where
  (<>) a b = BiblioEventBubble
             { _biblioEvBub_evEntryTypes =
                 a^.biblioEvBub_evEntryTypes <> b^.biblioEvBub_evEntryTypes
             , _biblioEvBub_evFieldTypes =
                 a^.biblioEvBub_evFieldTypes <> b^.biblioEvBub_evFieldTypes
             }

-- | The event bubble containing the 'BiblioEventBubble'. It must have a
-- setter for a field containing the 'BiblioEventBubble'.
class (Reflex t, Semigroup o) => OuterBubble o t where
  getBiblioBubble :: ASetter o o (BiblioEventBubble t) (BiblioEventBubble t)
  -- FIXME: Getter o (Event t a). This is not decoupled!
  extractBiblioDataEvent :: Getter o (Event t (Maybe Text))


-- * Get model data from the backend.

-- | For capsulation the 'bibliographyHook' propagates the app's
-- events to all functions for getting configuration/model data from
-- the backend. The first parameter is the outer event bubble, so that
-- the data may be requested on a specific event generated by the
-- application.
bibliographyHook :: (MonadWidget t m,
                     MonadReader l m, BiblioModel l t, BiblioConfig l,
                     EventWriter t w m, OuterBubble w t, Default w
                    ) => Event t w -- ^ the outer event bubble
                 -> m ()
bibliographyHook bub = do
  bibliographicEntries bub
  bibliographicFields bub
  return ()

-- | Get Fields from backend.
bibliographicFields :: (MonadWidget t m,
                        MonadReader l m, BiblioModel l t, BiblioConfig l,
                        EventWriter t w m, OuterBubble w t, Default w
                       ) => Event t w -- ^ the outer event bubble
                    -> m ()
bibliographicFields bub = do
  -- extract the event for getting the fields
  let evGet = coincidence (fmap (^.extractBiblioDataEvent) bub)
  -- make a request with a authentication (or no authentication)
  authRq :: Dynamic t (XhrRequestConfig ()) <- asks authRequestConfig
  baseUri :: Text <- asks baseUri
  mkFldsRq <- asks fieldsRequest
  evRsp <- performRequestAsyncWithError $
    tagPromptlyDyn (fmap (mkFldsRq baseUri) authRq) evGet
  -- parse result
  parseFields <- asks parseFields
  let evFlds = fmap parseFields evRsp
  -- pass the result up to the app's main widget using the event writer
  tellEvent $ fmap (const (def & getBiblioBubble .~
                           (BiblioEventBubble never evFlds))) evFlds
  return ()


-- | Get Entries from backend.
bibliographicEntries :: (MonadWidget t m,
                        MonadReader l m, BiblioModel l t, BiblioConfig l,
                        EventWriter t w m, OuterBubble w t, Default w
                       ) => Event t w -- ^ the outer event bubble
                    -> m ()
bibliographicEntries bub = do
  -- extract the event for getting the entries
  let evGet = coincidence (fmap (^.extractBiblioDataEvent) bub)
  -- make a request with a authentication (or no authentication)
  authRq :: Dynamic t (XhrRequestConfig ()) <- asks authRequestConfig
  baseUri :: Text <- asks baseUri
  mkEntsRq <- asks entriesRequest
  evRsp <- performRequestAsyncWithError $
    tagPromptlyDyn (fmap (mkEntsRq baseUri) authRq) evGet
  -- parse result
  parseEntries <- asks parseEntries
  let evEnts = fmap parseEntries evRsp
  -- pass the result up to the app's main widget using the event writer
  tellEvent $ fmap (const (def & getBiblioBubble .~
                           (BiblioEventBubble evEnts never))) evEnts
  return ()
