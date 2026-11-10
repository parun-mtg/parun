module Parun.Types.TriggerLimit where

import qualified Parun.Types.SpentOn as SpentOn

-- | The printed riders "This ability triggers only once each turn", "This
-- ability triggers only once" and "Do this only once each turn", as a property
-- of the ABILITY rather than of the trigger event.
--
-- There is NO comprehensive rule for the first two. CR 702.179d prints the
-- per-turn rider verbatim on the rulebook's own speed ability, which is as close
-- to a rule as the phrase gets, and this type says so rather than manufacturing
-- a citation. The per-game rider (Acrobatic Cheerleader) sits in the same
-- position with no rule at all. CR 603.2h is the third: the ability triggers
-- only while its instruction has not been taken that turn, which is the per-turn
-- limit spent by a different act (Parun.Types.SpentOn).
--
-- Deliberately NOT Parun.Types.TriggerFrequency, which the three arms below would
-- otherwise duplicate. That type narrows a trigger EVENT -- Aurelia, the
-- Warleader "attacks for the first time each turn" -- and the two are
-- distinguishable: an ability gained after the turn's first matching event has
-- already passed still triggers under this rider (Cursed Wombat's ruling of
-- 2024-06-07) and never does under that narrowing (Attended Healer's ruling of
-- 2020-09-25).
data TriggerLimit
  = -- | The ability triggers on every occurrence of its trigger event
    -- (CR 603.2c). Every printing but the riders' bearers.
    Unlimited
  | -- | One per turn, spent by what the payload names.
    OncePerTurn SpentOn.SpentOn
  | -- | At most one triggering per game. Spent on TRIGGERING like the arm above,
    -- and over a record that outlives the turn handoff
    -- (Parun.Types.GameState.triggeredThisGame).
    OncePerGame
  deriving (Eq, Ord, Show)
