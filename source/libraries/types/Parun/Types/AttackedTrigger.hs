module Parun.Types.AttackedTrigger where

import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackedSubject as AttackedSubject
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 508.3b / 508.3e: what was attacked, and who has to have declared the
-- attack -- Nothing for CR 508.3b's "is attacked", once per target, and a
-- relation for CR 508.3e's "[a player] attacks", once per attacking player
-- and target (Lulu, Stern Guardian's "whenever an opponent attacks you"). The
-- count narrows the creatures attacking that target to those its Filter
-- admits (Attuma, Atlantean Warlord's "one or more Merfolk you control").
data AttackedTrigger = MkAttackedTrigger
  { attacker :: Maybe PlayerRelation.PlayerRelation,
    attacked :: AttackedSubject.AttackedSubject,
    attackers :: Maybe AttackCount.AttackCount
  }
  deriving (Eq, Ord, Show)
