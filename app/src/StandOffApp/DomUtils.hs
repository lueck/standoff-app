{-# LANGUAGE OverloadedStrings #-}

module StandOffApp.DomUtils
  where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))


-- labelWidget :: forall k t m a. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
--                => T.Text
--             -> T.Text
--             -> k -> m a
labelWidget label xid widget = do
  el "div" $ do
    el "div" $ text label
    widget
