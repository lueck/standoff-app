{-# LANGUAGE RecursiveDo #-}
module StandOffApp.App where

import Reflex.Dom
import Reflex
import Control.Monad.Reader

import StandOffApp.Config
import StandOffApp.Model
import StandOffApp.ConfigInst

import StandOffApp.Bibliography.Widget
import StandOffApp.Bibliography.TypeDefs (emptyEntry)
import StandOffApp.Bibliography.Bibtex
import StandOffApp.Bibliography.Format
import StandOffApp.Auth.Widget


-- | The App's main widget. It's like a basic controller, because it
-- pushes events, that where propagated to this top level widget, into
-- the model. As arguments it takes the app's config and the main
-- view.
appWidget :: MonadWidget t m => AppConfig -- ^ the app's config
          -> AppWidget t m a              -- ^ the app's main view
          -> m ()
appWidget conf view = do
  rec
    model <- appModel bubble conf
    (_, bubble) <- runEventWriterT $ flip runReaderT model $ do
      view -- appView
  pure ()

appView :: (MonadWidget t m) => AppWidget t m ()
appView = do
  loginWidget
  
  showToken
  
  -- entry <- biblioWidget emptyEntry
  -- -- live output
  -- format entry
  -- bibtexEntry entry

  return ()
