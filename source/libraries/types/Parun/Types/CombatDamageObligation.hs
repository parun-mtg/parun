module Parun.Types.CombatDamageObligation where

-- | Whether a creature assigns its combat damage where a
-- Parun.Types.CombatDamageOption says at its controller's choice (Lone Wolf's
-- "you may") or regardless (Outmaneuver's "assign ... as though they weren't
-- blocked"). Parun.Types.CastObligation's shape, for CR 510.1b-c.
data CombatDamageObligation
  = Mandatory
  | Optional
  deriving (Bounded, Enum, Eq, Ord, Show)
