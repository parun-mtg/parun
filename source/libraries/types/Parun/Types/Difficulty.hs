module Parun.Types.Difficulty where

-- | How strongly the search bot (Parun.Play.Search) plays, named for a
-- person choosing an opponent (Parun.Play.Opponent), weakest first.
data Difficulty
  = -- | Few playouts, many mistakes.
    MkEasy
  | -- | Some mistakes.
    MkMedium
  | -- | No mistakes.
    MkHard
  deriving (Bounded, Enum, Eq, Ord, Show)
