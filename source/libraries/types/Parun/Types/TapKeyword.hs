module Parun.Types.TapKeyword where

-- | A keyword ability whose cost taps creatures and counts their power, which
-- Parun.Types.Modification's TapsCounting names. Not
-- Parun.Types.KeywordFamily, which has no constructor for a nullary keyword
-- such as station.
data TapKeyword
  = -- | CR 702.122a: crew N.
    Crew
  | -- | CR 702.171a: saddle N.
    Saddle
  | -- | CR 702.184a: station.
    Station
  deriving (Bounded, Enum, Eq, Ord, Show)
