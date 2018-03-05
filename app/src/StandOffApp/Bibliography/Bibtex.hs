{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module StandOffApp.Bibliography.Bibtex
  where

import Reflex.Dom hiding (element)
import qualified Data.Text as T
import           Data.Maybe --(fromJust)
import Control.Monad
import Control.Monad.Fix
import Control.Lens

import StandOffApp.Bibliography.TypeDefs


bibtexEntry :: ( DomBuilder t m
               , DomBuilderSpace m ~ GhcjsDomSpace
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
               )
               => Dynamic t Entry
            -> m ()
bibtexEntry = dynText . (fmap bibtexPure) -- fields output without <br/>
--bibtexEntry = bibtexEntryDom -- with <br/>

bibtexEntryDom :: ( DomBuilder t m
               , DomBuilderSpace m ~ GhcjsDomSpace
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
               )
               => Dynamic t Entry
            -> m ()
bibtexEntryDom e = do
  text "@"
  dynText $ fmap (^.entryType) e
  text "{"
  dynText $ fmap (^.entryKey) e
  text ",\n"
  el "br" blank
  let flds = fmap (^.fields) e
  
  -- mapDynM (bibtexFieldsMap flds) (fmap length flds)
  -- _ <- mapDynM (bibtexFieldMap flds) $ fmap length flds

  -- Take this one! With optional  --_ <- fmap (sample . current) $
  --mapDynM (bibtexFieldRec flds) $ fmap length flds

  -- This has a static number of fields (=0)
  -- (bibtexFieldRec flds) =<< sample (current (fmap length flds))
  
  n :: Int <- sample $ current $ fmap length flds
  --bibtexField flds =<< (sample $ current $ fmap length flds)
  --fmap (sample . current) $

  --dynText $ fmap (T.pack . show . length) flds
  
  text "}"
  -- where

bibtexFieldsMap fs n = do
  mapM_ (bibtexFieldNth fs) [0..n]

-- bibtexFields :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- bibtexFields :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m' ()
bibtexFieldNth fs n = el "div" $ do
  dynText $ fmap (fst . (!! n)) fs
  text "= {"
  dynText $ fmap (snd . (!! n)) fs
  text "},"

--bibtexField :: MonadWidget t m => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
--bibtexField :: forall m t. (MonadSample t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
--bibtexField :: forall m t. (DomBuilder t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- bibtexField :: forall t m. (MonadSample t m, DomBuilder t m, PostBuild t m) => Dynamic t [(T.Text, T.Text)] -> Int -> m ()
bibtexFieldRec _ 0 = do
  text "no more fields"
  el "br" blank
bibtexFieldRec fs n = el "div" $ do
  dynText $ fmap (fst . head) fs
  text "= {"
  dynText $ fmap (snd . head) fs
  text "},"
  bibtexFieldRec (fmap tail fs) (n - 1)
  -- el "br" blank
  
-- bibtexTest :: MonadWidget t m -> Dynamic t [(T.Text, T.Text)] -> Int -> m ()
-- bibtexTest _ 0 = do el "br" blank
bibtexTest fs n = do 
  text $ T.pack $ show n
  el "br" blank 


-- bibtexField :: MonadWidget t m => (T.Text, T.Text) -> m ()
--bibtexField :: forall t m. (Reflex t, MonadSample t m) => Dynamic t (T.Text, T.Text) -> m ()
bibtexFieldOFF fld = el "div" $ do
  dynText $ fmap fst fld
  -- _fld <- return $ fst fld
  -- dynText $ wrapDyn $ fst fld
  text "= {"
  dynText $ fmap snd fld
  -- _val <- snd fld
  -- dynText $ wrapDyn $ snd fld
  text "},"
  --return ()


bibtexPure :: Entry -> T.Text
bibtexPure (Entry key typ flds) = T.concat
  ["@", typ,
  "{", key, ",\n",
  (T.concat $ map (\(k, v) -> T.concat ["\t", k, "\t= {", v, "},\n"]) flds),
  "}"]
  
