module Parun.Types.LossCause where

-- | A state-based action that makes a player lose the game (CR 704.5, 704.6).
data LossCause
  = -- | CR 704.5a: 0 or less life.
    NoLife
  | -- | CR 704.5b: an attempted draw from an empty library.
    EmptyLibrary
  | -- | CR 704.5c / 704.6b: ten or more poison counters, or a Two-Headed Giant team's fifteen.
    Poison
  | -- | CR 704.6c / 903.10a: 21 or more combat damage from one commander.
    CommanderDamage
  deriving (Bounded, Enum, Eq, Ord, Show)
