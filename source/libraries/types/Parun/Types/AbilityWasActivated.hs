module Parun.Types.AbilityWasActivated where

import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 602.2b / 601.2i: an ability became activated -- who activated it, the
-- ability on the stack, its source, whether it is a loyalty ability (CR
-- 606.3's once-per-turn limit reads that off the log) and whether a mana one.
data AbilityWasActivated = MkAbilityWasActivated
  { player :: PlayerId.PlayerId,
    -- | Nothing for an activation that put nothing on the stack: CR 605.3b's
    -- mana ability, or one activated inside a die roll's window.
    ability :: Maybe ObjectId.ObjectId,
    source :: ObjectId.ObjectId,
    loyalty :: Bool,
    kind :: AbilityKind.AbilityKind
  }
  deriving (Eq, Ord, Show)
