{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandOffApp.Bibliography.Format
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import           Data.Maybe --(fromJust)
import qualified Data.Map as Map
import Control.Monad
import Control.Lens

import StandOffApp.Bibliography.TypeDefs

-- | Formatted output of a bibliographic 'Entry'.
format :: MonadWidget t m => Dynamic t Entry -> m ()
format entry = do
  -- FIXME: typ must be dynamic in the case statement, otherwise no
  -- reformatting when the type is updated.
  typ :: T.Text <- sample $ current $ fmap (^.entryType) entry
  case (T.toLower typ) of
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
  where
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
    inStr = do text "in: "
