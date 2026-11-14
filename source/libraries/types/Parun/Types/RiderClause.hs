module Parun.Types.RiderClause where

import Numeric.Natural (Natural)

-- | Which clause of a keyword ability's printed rider (CR 602.5) a
-- Parun.Types.KeywordRiderGrant relaxes.
data RiderClause
  = -- | CR 602.5d read as CR 602.5e: activate any time the player could cast
    -- an instant (Leonin Shikari).
    InstantSpeed
  | -- | CR 602.5b's per-turn limit raised to this many (Birgi, God of
    -- Storytelling).
    TimesEachTurn Natural
  | -- | CR 602.5b's once-only limit raised by one (Wonder Man, Hollywood Hero).
    AdditionalTime
  | -- | CR 602.5b's once-only limit lifted, as though the ability hadn't been
    -- activated (Elvish Refueler).
    AsThoughNotActivated
  deriving (Eq, Ord, Show)
