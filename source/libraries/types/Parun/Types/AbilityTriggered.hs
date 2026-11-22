module Parun.Types.AbilityTriggered where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parun.Types.Card as Card
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TriggeringMove as TriggeringMove

-- | CR 603.2: an ability triggered -- what it hangs on, its controller, and the
-- ability itself.
--
-- `source` and `ability` together are the DISCRIMINATOR
-- Parun.Types.TriggerEntry carries for the same reason (#61): one object can bear
-- two distinct triggered abilities keyed on the same event, so the source alone
-- does not say which of them triggered, and neither does the trigger condition
-- the ability holds. The ability VALUE rather than an index into the source's
-- abilities, TriggerEntry's haddock giving both reasons.
--
-- `source` is a Parun.Types.TriggerSource and not an object id, so a SOURCELESS
-- inherent ability the rulebook states without a card (CR 726.2's monarch pair,
-- CR 702.179d's speed increase, CR 729.1's rad counters) gets a record too.
-- `controller` is what tells two players' instances of one such ability apart,
-- there being no object to name.
data AbilityTriggered = MkAbilityTriggered
  { source :: TriggerSource.TriggerSource,
    controller :: PlayerId.PlayerId,
    ability :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card),
    -- | CR 603.3: the ability as an object on the stack, under the id
    -- Parun.Engine.Engine.reactions reserved as it triggered; Nothing for a
    -- record that is only a "triggers only once" key.
    stackObject :: Maybe ObjectId.ObjectId,
    -- | The objects whose move caused the ability to trigger, by move: CR
    -- 508.3's attackers and CR 603.6a's entrant; no other move is recorded,
    -- and a move that caused nothing has no key.
    causers :: Map.Map TriggeringMove.TriggeringMove (Set.Set ObjectId.ObjectId)
  }
  deriving (Eq, Ord, Show)
