module Parun.Types.Holder where

-- | Which of a candidate's players a Parun.Types.Filter holder atom reads.
data Holder
  = -- | CR 108.4 / 110.2: its controller.
    Controller
  | -- | CR 108.3: its owner.
    Owner
  | -- | CR 400.7d / 601.2a: who cast the spell this permanent was.
    Caster
  | -- | CR 310.9e: the player who protects it, a battle.
    Protector
  deriving (Bounded, Enum, Eq, Ord, Show)
