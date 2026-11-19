module Parun.Types.CountedAct where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerId as PlayerId

-- | The payload of Parun.Types.GameEvent's CountedAct arm: a
-- Parun.Types.PlayerAction performed for a number, which a trigger binds as
-- its event amount. CR 701.57b's discover, with its N; CR 701.22a's scry that
-- put cards on the bottom, with how many.
data CountedAct = MkCountedAct
  { action :: PlayerAction.PlayerAction,
    player :: PlayerId.PlayerId,
    value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
