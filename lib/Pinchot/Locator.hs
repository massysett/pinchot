module Pinchot.Locator where

import qualified Control.Monad.Trans.State as State

import Pinchot.Types

-- | Runs a 'Locator' computation.
-- Starts out with the line, column, and position all set to 1.
locate
  :: (a -> Locator b)
  -> a
  -> b
locate k a = State.evalState (k a) (Loc 1 1 1)

