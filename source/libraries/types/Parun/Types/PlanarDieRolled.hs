module Parun.Types.PlanarDieRolled where

import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlayerId as PlayerId

-- | The payload of Parun.Types.GameEvent's PlanarDieRolled arm: CR 901.9's roll,
-- who rolled and the roll's result (CR 706.2), or none for a reroll of it,
-- which modifies that result (CR 706.2b; docs/decisions.md, "Whether a reroll
-- of the planar die is a roll of the planar die").
data PlanarDieRolled = MkPlanarDieRolled
  { roller :: PlayerId.PlayerId,
    face :: Maybe PlanarDieFace.PlanarDieFace
  }
  deriving (Eq, Ord, Show)
