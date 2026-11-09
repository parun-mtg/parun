module Parun.Types.DeclarationTrigger where

import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TriggerFrequency as TriggerFrequency

-- | CR 508.3c / 508.3d: which player's declaration fires the ability, and the
-- creatures it has to have declared -- Nothing for CR 508.3d's "whenever [a
-- player] attacks", a count for Hermes, Overseer of Elpis' "whenever you
-- attack with one or more Birds". The frequency narrows the event to the
-- player's first such declaration of the turn (Desert Were-Worm's "for the
-- first time each turn"). The condition is a state that holds as the
-- player attacks (The Chief Warg's "while you control a creature with power 4
-- or greater"), never re-checked as TriggeredAbility.intervening would be (CR
-- 603.4).
data DeclarationTrigger = MkDeclarationTrigger
  { player :: PlayerRelation.PlayerRelation,
    attackers :: Maybe AttackCount.AttackCount,
    frequency :: TriggerFrequency.TriggerFrequency,
    while :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
