module Parun.Types.PlanarDieTrigger where

import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 901.8 / 901.9: whose planar die roll, and which face it must show ---
-- the planeswalking ability's Planeswalker symbol, Pompeii's blank, or any
-- face (Stairs to Infinity's "whenever you roll the planar die").
data PlanarDieTrigger = MkPlanarDieTrigger
  { -- | The roller, read against CR 109.5's "you".
    player :: PlayerRelation.PlayerRelation,
    -- | Nothing matches every face.
    face :: Maybe PlanarDieFace.PlanarDieFace
  }
  deriving (Eq, Ord, Show)
