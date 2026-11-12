module Parun.Types.AbilityWasActivated where

import qualified Parun.Types.AbilityFacts as AbilityFacts
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 602.2b / 601.2i: an ability became activated -- who activated it, the
-- ability on the stack, its source, the facts its criteria ask (CR 606.3's
-- once-per-turn limit reads its loyalty off the log), and the mana in its
-- activation cost.
data AbilityWasActivated = MkAbilityWasActivated
  { player :: PlayerId.PlayerId,
    -- | Nothing for an activation that put nothing on the stack: CR 605.3b's
    -- mana ability, or one activated inside a die roll's window.
    ability :: Maybe ObjectId.ObjectId,
    source :: ObjectId.ObjectId,
    facts :: AbilityFacts.AbilityFacts,
    -- | CR 602.2b: the mana in the ability's activation cost, as
    -- Game.activationCost reads it; Nothing for a cost with no mana.
    activationCost :: Maybe ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
