module Parun.Types.TriggeringMove where

-- | Which event an effect changes the triggering of
-- (Parun.Types.EntriesTrigger).
data TriggeringMove
  = -- | CR 603.6a: the permanent entering the battlefield.
    Entering
  | -- | CR 700.4: the permanent dying.
    Dying
  | -- | CR 508.3: the creature attacking (Parun.Engine.Event.Binding.attackCausers).
    --
    -- Not implemented: an attack that doesn't cause abilities to trigger
    -- (EntryTriggering.NoTrigger) (#7726).
    Attacking
  | -- | CR 119.9: the row's player gaining life; no permanent makes it, so
    -- EntriesTrigger.entrants is unread (Parun.CardSpec's lint keeps it empty).
    GainingLife
  deriving (Bounded, Enum, Eq, Ord, Show)
