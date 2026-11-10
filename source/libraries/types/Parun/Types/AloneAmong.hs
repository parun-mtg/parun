module Parun.Types.AloneAmong where

-- | Which other attackers make a creature not alone: those declared, for an
-- attack trigger's "attacks alone", or those attacking now, for a filter's "is
-- attacking alone" (CR 506.5).
data AloneAmong
  = -- | CR 506.5: every other attacker.
    EveryAttacker
  | -- | CR 506.6: it attacks a player, and no other attacker attacks that
    -- player.
    AttackersOfItsPlayer
  deriving (Bounded, Enum, Eq, Ord, Show)
