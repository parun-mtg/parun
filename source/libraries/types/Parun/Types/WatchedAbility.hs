module Parun.Types.WatchedAbility where

import qualified Parun.Types.ChapterMoment as ChapterMoment

-- | Which triggered abilities a Parun.Types.AbilityWatch watcher watches.
data WatchedAbility
  = -- | CR 715.2d: the final chapter ability of a Saga, as it triggers
    -- (Historian's Boon) or resolves (Tom Bombadil).
    SagaFinalChapter ChapterMoment.ChapterMoment
  | -- | CR 508.3: an ability of a creature, triggering because that creature
    -- attacked (Firebender Ascension).
    CausedByOwnAttack
  deriving (Eq, Ord, Show)
