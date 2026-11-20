module Parun.Types.KindStep where

-- | What a targeted Parun.Types.AnotherCounter.EachKind does to each kind of
-- counter on its targets (CR 122.1).
data KindStep
  = -- | Powerful Broker's "give that permanent or player another counter of that kind".
    Another
  | -- | Dramatist's Puppet's "put another counter of that kind on it or remove one from it", chosen kind by kind (CR 608.2d).
    AnotherOrRemoveOne
  deriving (Bounded, Enum, Eq, Ord, Show)
