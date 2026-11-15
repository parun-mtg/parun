module Parun.Types.TransformRewrite where

import qualified Data.Sequence as Seq
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | What an "as this creature transforms" ability does as the permanent
-- transforms (CR 701.27i).
data TransformRewrite effect
  = -- | CR 701.27i: perform these effects (Sephiroth, One-Winged Angel).
    RunEffects (Seq.Seq effect)
  | -- | CR 701.27i: choose a player in this relation to the controller, written
    -- to Object.chosenPlayers (Shinryu, Transcendent Rival).
    ChoosePlayer PlayerRelation.PlayerRelation
  deriving (Eq, Ord, Show)
