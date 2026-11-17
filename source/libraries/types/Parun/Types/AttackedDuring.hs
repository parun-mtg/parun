module Parun.Types.AttackedDuring where

-- | CR 508.1a: which turn's attacker declarations PlayerScope.AttackedYou reads.
-- Bounded and Enum for Parun.Codec.AttackedDuring's Arm.enum.
data AttackedDuring
  = -- | CR 500.1: the attacking player's own most recent turn (Weathered Sentinels).
    TheirLastTurn
  | -- | CR 500.1: the turn in progress (Sandswirl Wanderglyph).
    ThisTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
