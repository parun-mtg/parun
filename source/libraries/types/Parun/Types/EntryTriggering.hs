module Parun.Types.EntryTriggering where

-- | How an effect changes the triggering of the abilities it reaches
-- (Parun.Types.EntriesTrigger, Parun.Types.BearersTrigger).
data EntryTriggering
  = -- | CR 603.6a / 613.11 / Hushwing Gryff, Nowhere to Run: they don't
    -- trigger.
    NoTrigger
  | -- | CR 603.2d / Yarok, the Desecrated, Teysa Karlov: each triggers an
    -- additional time.
    AnAdditionalTime
  deriving (Bounded, Enum, Eq, Ord, Show)
