module Parun.Types.ExtraPhase where

-- | CR 500.8: one phase an effect adds to a turn. The rule does not fix WHICH,
-- and printed cards vary -- Relentless Assault adds a combat and a main phase,
-- Aurelia, the Warleader only a combat phase, Sphinx of the Second Sun a
-- beginning phase.
--
-- A whole PHASE, never a step: Parun.Engine.Turn.expandExtraPhase inserts the
-- steps each is made of, so the payload says what the card says and what a
-- phase is made of stays in Parun.Engine.Turn.
data ExtraPhase
  = -- | CR 501.1's three steps.
    ExtraBeginning
  | -- | CR 506.1's five steps.
    ExtraCombat
  | -- | CR 505.1a: an added main phase is a POSTCOMBAT main phase.
    ExtraMain
  deriving (Bounded, Enum, Eq, Ord, Show)
