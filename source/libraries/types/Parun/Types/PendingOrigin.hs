module Parun.Types.PendingOrigin where

-- | Which kind of replacement queued a Parun.Types.PendingReplacementEffect,
-- for what its drain does besides running the effects.
data PendingOrigin
  = -- | Every queuer the two below are not; the drain only runs the effects.
    Ordinary
  | -- | CR 400.7 / 614.6: a ZoneChangeR rider; the drain binds what the Binding.departedPermanent target arrived as under Binding.became (Ravenloft Adventurer).
    ZoneChangeRider
  | -- | CR 701.27f / 701.27i: an "as this transforms" row; the drain samples the transform's event again after it (Olag, Ludevic's Hubris).
    AsTransforms
  deriving (Bounded, Enum, Eq, Ord, Show)
