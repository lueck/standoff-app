module StandOffApp.Dynamic
  where

import Reflex.Dom
import Control.Monad


-- | Bind a dynamic value to a monadic function taking the same value
-- unwrapped from it's dynamic context.

-- By guaraqe,
-- https://www.reddit.com/r/reflexfrp/comments/6r4qqe/binding_with_dynamic/
bindDynM ::
  MonadWidget t m =>
  (a -> m (Dynamic t b)) -> Dynamic t a -> m (Dynamic t b)
bindDynM f x =
  let
    start = do
      init <- sample (current x)
      f init
  in
    fmap join $ widgetHold start (fmap f (updated x))
