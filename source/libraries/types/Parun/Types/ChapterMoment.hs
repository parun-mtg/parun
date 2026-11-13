module Parun.Types.ChapterMoment where

-- | Which moment of a Saga's final chapter ability a Parun.Types.WatchedAbility
-- watcher fires at.
data ChapterMoment
  = -- | CR 603.3b: it triggers -- Historian's Boon, against
    -- GameEvent.AbilityTriggered.
    Triggers
  | -- | CR 608.2n: it finishes resolving and leaves the stack -- Tom Bombadil,
    -- against GameEvent.TriggeredAbilityFinished.
    Resolves
  deriving (Bounded, Enum, Eq, Ord, Show)
