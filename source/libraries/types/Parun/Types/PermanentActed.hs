module Parun.Types.PermanentActed where

import qualified Parun.Types.PermanentAction as PermanentAction

-- | CR 603.2: a permanent performed a Parun.Types.PermanentAction, beside the
-- permanent the `permanent` parameter names -- the actor's id on
-- Parun.Types.GameEvent's PermanentActed, and a Parun.Types.ActingPermanent on
-- Parun.Types.TriggerCondition's PermanentActs -- and the card the act revealed,
-- which the `revealed` parameter carries: CR 701.44a's explore reveal on the
-- event, a Filter on that card on the trigger ("explores a land card"), and
-- nothing on Parun.Types.Effect's CounterAndMark.
data PermanentActed revealed permanent = MkPermanentActed
  { action :: PermanentAction.PermanentAction,
    permanent :: permanent,
    revealed :: revealed
  }
  deriving (Eq, Ord, Show)
