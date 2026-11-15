module Parun.Types.StepOfSeats where

import qualified Data.Set as Set
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 611.2a: the payload of Parun.Types.Expiry's AtStepOf arm: a step, and
-- the seats whose turn's beginning of it ends the effect.
data StepOfSeats = MkStepOfSeats
  { step :: Phase.Phase,
    seats :: Set.Set PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
