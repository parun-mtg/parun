module Parun.Types.JoinAttack where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef

-- | What Effect.JoinAttack states is attacking, and whose attack it joins
-- (CR 508.4).
data JoinAttack = MkJoinAttack
  { -- | The creatures stated to be attacking.
    creature :: ObjectRef.ObjectRef,
    -- | The attacking player whose attacked players and planeswalkers are the
    -- resolving controller's choices: Tahngarth, First Mate's "that opponent".
    alongside :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
