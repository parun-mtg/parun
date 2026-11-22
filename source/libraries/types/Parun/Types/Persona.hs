module Parun.Types.Persona where

-- | What the search bot (Parun.Play.Search) values in a position, named for
-- a person choosing an opponent (Parun.Play.Opponent).
data Persona
  = -- | Every feature alike (Weights.neutral).
    MkBalanced
  | -- | Its creatures' power and its opponents' life lost above all.
    MkAggro
  | -- | Cards in hand, its own life, and its opponents' creatures gone.
    MkControl
  deriving (Bounded, Enum, Eq, Ord, Show)
