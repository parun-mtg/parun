module Parun.Types.PlanarDieRolled where

import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlanarRollCause as PlanarRollCause
import qualified Parun.Types.PlayerId as PlayerId

-- | The payload of Parun.Types.GameEvent's PlanarDieRolled arm: CR 901.9's roll,
-- who rolled and the roll's result (CR 706.2), or none for a reroll of it,
-- which modifies that result (CR 706.2b; docs/decisions.md, "Which face of a
-- rerolled planar die a trigger reads"), and what rolled it: a reroll is an
-- effect's.
data PlanarDieRolled = MkPlanarDieRolled
  { roller :: PlayerId.PlayerId,
    face :: Maybe PlanarDieFace.PlanarDieFace,
    cause :: PlanarRollCause.PlanarRollCause
  }
  deriving (Eq, Ord, Show)
