module Parun.Types.PlayerDrawsNthCard where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.StepBegins as StepBegins

-- | CR 603.2 over CR 121's draw: whose draw counts, and WHICH draw of the turn
-- fires the ability -- Erudite Wizard's "second card". No ordinal is every draw,
-- Teferi, Hero of Dominaria's emblem's "whenever you draw a card".
data PlayerDrawsNthCard = MkPlayerDrawsNthCard
  { player :: PlayerRelation.PlayerRelation,
    nth :: Maybe Natural.Natural,
    -- | CR 500.1: the step the draw must happen in, `nth` then counting the
    -- draws since it began -- Wiretapping's "your first card during each of
    -- your draw steps". Nothing counts across the turn.
    within :: Maybe StepBegins.StepBegins
  }
  deriving (Eq, Ord, Show)
