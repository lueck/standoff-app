{-# LANGUAGE OverloadedStrings #-}

module StandOffApp.DomUtils
  where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))


labelWidget :: MonadWidget t m => T.Text -- ^ Label for user interface
            -> T.Text -- ^ css class of div container
            -> m a -- ^ widget
            -> m a
labelWidget label clas widget = do
  elClass "div" clas $ do
    elClass "div" "label" $ text label
    widget
