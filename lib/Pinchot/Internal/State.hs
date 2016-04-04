module Pinchot.Internal.State where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State

-- | Monomorphises 'State.get' to eliminate ambiguous type errors.
get :: State s s
get = State.get

-- | Monomorphises 'State.put' to eliminate ambiuous type errors.
put :: s -> State s ()
put = State.put

