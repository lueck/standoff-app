{-# LANGUAGE RecursiveDo #-}
module StandOffApp.App where

import Reflex.Dom
import Reflex
import Control.Monad.Reader

import StandOffApp.Config
import StandOffApp.Model
import StandOffApp.ConfigInst

import StandOffApp.Bibliography.Form
import StandOffApp.Bibliography.Model
import StandOffApp.Bibliography.TypeDefs (emptyEntry)
--import StandOffApp.Bibliography.Bibtex
--import StandOffApp.Bibliography.Format
import StandOffApp.Auth.Widget


-- | The App's main widget. It's like a basic controller, because it
-- pushes events, that where propagated to this top level widget, into
-- the model. As arguments it takes the app's config and the main
-- view.
appWidget :: MonadWidget t m => AppConfig -- ^ the app's config
          -> AppWidget t m a              -- ^ the app's main view
          -> (Event t (EventBubble t) -> AppWidget t m a) -- ^ the app's event propagation widget
          -> m ()
appWidget conf view propEv = do
  rec
    model <- appModel bubble conf
    (_, bubble) <- runEventWriterT $ flip runReaderT model $ do
      view -- appView
      propEv bubble -- propagate events
  pure ()

-- | The app's main view.
appView :: (MonadWidget t m) => AppWidget t m ()
appView = do
  login
  
  showToken
  
  entry <- biblioForm emptyEntry
  -- -- live output
  -- format entry
  -- bibtexEntry entry

  return ()


-- | Propagte events form the 'EventBubble' to the widgets listed in
-- this function. This may be used to hook functions e.g. to the login
-- event.
appEventsHook :: (MonadWidget t m) => Event t (EventBubble t) -> AppWidget t m ()
appEventsHook bub = do
  bibliographyHook bub
  return ()
