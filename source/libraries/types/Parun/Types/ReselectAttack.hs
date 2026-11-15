module Parun.Types.ReselectAttack where

import qualified Data.Set as Set
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.ObjectRef as ObjectRef

-- | What Effect.ReselectAttack moves, and to which kinds of target (CR 508.7).
data ReselectAttack = MkReselectAttack
  { -- | The attacking creatures whose target may be reselected.
    creature :: ObjectRef.ObjectRef,
    -- | The kinds of target the reselection may pick: Capricopian's "which
    -- player" is OfPlayer alone, Portal Mage's "which player or permanent"
    -- all three.
    kinds :: Set.Set AttackTargetKind.AttackTargetKind
  }
  deriving (Eq, Ord, Show)
