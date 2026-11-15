module Parun.Types.AttackWindow where

-- | Which declarations a count over EventShape.AttackerDeclared folds. Bounded
-- and Enum for Parun.Codec.AttackWindow's Arm.enum.
data AttackWindow
  = -- | CR 508.1a: every attacker declared this turn, the event log's own extent.
    ThisTurn
  | -- | CR 508.1a / 511.3: the attackers declared this combat phase, departed
    -- ones included (Fearless Swashbuckler).
    ThisCombat
  deriving (Bounded, Enum, Eq, Ord, Show)
