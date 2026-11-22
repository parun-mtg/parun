module Parun.Types.SearchMethod where

-- | How the search bot (Parun.Play.Search) spends its playouts at a
-- decision.
data SearchMethod
  = -- | Each option played out alike, the best total taken
    -- (Parun.Play.Search.flat).
    Flat
  | -- | One tree over the decisions of every seat, shared by every sampled
    -- world (Parun.Play.Search.informationSet).
    InformationSet
  deriving (Bounded, Enum, Eq, Ord, Show)
