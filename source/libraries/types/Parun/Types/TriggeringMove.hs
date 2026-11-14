module Parun.Types.TriggeringMove where

-- | Which zone change of a permanent an effect changes the triggering of
-- (Parun.Types.EntriesTrigger).
data TriggeringMove
  = -- | CR 603.6a: the permanent entering the battlefield.
    Entering
  | -- | CR 700.4: the permanent dying.
    Dying
  deriving (Bounded, Enum, Eq, Ord, Show)
