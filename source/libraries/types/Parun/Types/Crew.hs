module Parun.Types.Crew where

import Numeric.Natural (Natural)

-- | The payload of Parun.Types.Keyword's Crew arm: CR 702.122a's "Crew N".
--
-- timesEachTurn is CR 602.5b's rider printed after the keyword, Luxurious
-- Locomotive's "Activate only once each turn"; Nothing prints none.
-- Parun.Types.Equip's field, for its reason.
data Crew = MkCrew
  { power :: Natural,
    timesEachTurn :: Maybe Natural
  }
  deriving (Eq, Ord, Show)
