module Parun.Types.SkipWindow where

import qualified Parun.Types.PhaseSelector as PhaseSelector
import qualified Parun.Types.TurnKind as TurnKind

-- | CR 500.11 / 614.10: what a skip names, and what Parun.Engine.Engine offers
-- up as it is about to begin. The two sides of CR 614.1b are compared by
-- EQUALITY on this type, but that a skip of any turn takes an extra one too
-- (Parun.Engine.Replacement's PhaseR arm).
--
-- A type of its own rather than a fourth PhaseSelector arm: a PhaseSelector
-- also names an activation window and an expiry, where a whole turn is no
-- window, and an extra turn's own skips (Parun.Types.TakeExtraTurn) can't name
-- the turn itself.
data SkipWindow
  = -- | A step or phase of the turn (CR 500.1).
    PartOfTurn PhaseSelector.PhaseSelector
  | -- | The whole turn, of the kind named (CR 500.11).
    WholeTurn TurnKind.TurnKind
  deriving (Eq, Ord, Show)
