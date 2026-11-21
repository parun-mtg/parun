module Parun.Types.ArmDelayedTrigger where

import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.PlayerRef as PlayerRef

-- | CR 603.7: which delayed triggered ability to arm, and the temporal envelope
-- to arm it inside -- when it becomes armed, and how long it stays armed.
--
-- Parametric in `ability` for Parun.Types.CreateCopy's reason: the declaration
-- below is a whole ability, and Effect is what instantiates the variable.
data ArmDelayedTrigger ability = MkArmDelayedTrigger
  { name :: AbilityName.AbilityName,
    -- | CR 603.7a's floor, which is Immediately for everything but Meandering
    -- Towershell.
    onset :: Onset.Onset,
    -- | CR 603.7b's stated duration. Nothing is that rule's default -- once
    -- only, at the next trigger event -- spelled as an absence because the rule
    -- words it that way.
    duration :: Maybe Duration.Duration,
    -- | CR 603.7a / 613.1f: the delayed ability's own text, carried by the arm
    -- inside a quoted ability (Splinter Twin's "Exile that token at the
    -- beginning of the next end step") so it travels with every grant and copy
    -- of the quotation. Nothing for an arm whose `name` the source's own card
    -- declares (Face.delayedAbilities).
    ability :: Maybe ability,
    -- | A one-time boon for this player (Tenacious Pup's ruling), who controls
    -- it in place of CR 603.7d-e's controller of the arming resolution: "you
    -- get" (Dunbarrow Revivalist), or Loch Larent's "Target opponent gets",
    -- whose "you" is that player. Nothing is an ordinary delayed trigger.
    boon :: Maybe PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
