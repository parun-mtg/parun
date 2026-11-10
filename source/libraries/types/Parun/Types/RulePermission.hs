module Parun.Types.RulePermission where

import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.RuleWaiver as RuleWaiver

-- | CR 702.3b / 302.6 / 613.11: one printed permission lifting a rule for the
-- creatures it names -- "this creature can attack as though it didn't have
-- defender" (Prison Barricade kicked), "can attack as though it had haste"
-- (Instill Energy), "you may activate abilities of creatures you control as
-- though those creatures had haste" (Thousand-Year Elixir).
--
-- A permission rather than an arm of Parun.Types.CombatRestriction, for
-- Parun.Types.BlockPermission's reason: a restriction forbids a declaration and
-- this lifts one, so no reader of the one may read the other. It lifts its rule
-- alone: the creature still HAS defender, and still lacks haste, so CR 302.6's
-- two halves are two waivers.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture every sibling carrier takes. Parun.Engine.RulePermission is the only
-- module that reads it, and it answers a set of ids.
data RulePermission = MkRulePermission
  { -- | Which creatures it covers: Prison Barricade's own text is
    -- Affected.Matching Filter.IsSource.
    affected :: Affected.Affected,
    -- | An "as long as" (CR 611.3a; Drowsing Tyrannodon),
    -- BlockPermission.while's gate: re-read on every look against the full
    -- projection, the "you" inside it the source's controller. Nothing grants
    -- unconditionally.
    while :: Maybe Condition.Condition,
    -- | The rule it lifts.
    waives :: RuleWaiver.RuleWaiver
  }
  deriving (Eq, Ord, Show)
