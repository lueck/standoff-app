{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MonoLocalBinds #-}
module StandOffApp.Bibliography.Format
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import           Data.Maybe --(fromJust)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Fix
import Control.Lens

import StandOffApp.Bibliography.TypeDefs

-- | Formatted output of a bibliographic 'Entry'.

-- format ::
--   ( DomBuilder t m
--   , DomBuilderSpace m ~ GhcjsDomSpace
--   , MonadFix m
--   , MonadHold t m
--   , PostBuild t m
--   )
--   => Dynamic t Entry
--   -> m ()
format :: MonadWidget t m => Dynamic t Entry -> m ()
format entry = do
  -- FIXME: typ must be dynamic in the case statement, otherwise no
  -- reformatting when the type is updated. So this does no update:
  formattedType =<< (sample $ current $ fmap (^.entryType) entry)
  -- Use mapDynM ? How?
  --mapDynM formattedType $ fmap (^.entryType) entry
  return ()
  where
    --formattedType :: MonadWidget t m => T.Text -> m ()
    formattedType t = do
      case (T.toLower t) of
        "article" -> do
          { author
          ; titleQuoted
          ; inStr
          }
        otherwise -> do
          { author
          ; title
          ; locPublYear
          }
      return ()
    --flds :: (Reflex t) => Dynamic t (Map.Map T.Text T.Text)
    flds = fmap (Map.fromList . (^.entryFields)) entry
    --fdom :: MonadWidget t m => T.Text -> T.Text -> T.Text -> T.Text -> m ()
    fdom k dflt beforeSep afterSep = do
      el "span" $ dynText $
        fmap ((fromMaybe dflt) . (fmap (T.append beforeSep)) . (fmap (`T.append` afterSep)) . (Map.lookup k)) flds
    author = fdom "author" "" "" ": "
    title = fdom "title" "" "" ", "
    titleQuoted = fdom "title" "" "»" "«, "
    locPublYear = do { fdom "location" "" "" ": "; fdom "publisher" "" "" ", "; fdom "year" "" "" ", " }
    inStr = text "in: "
