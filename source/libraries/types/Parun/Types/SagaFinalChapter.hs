module Parun.Types.SagaFinalChapter where

import qualified Parun.Types.ChapterMoment as ChapterMoment
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 715.2d: "whenever the final chapter ability of a Saga [a related player]
-- controls" triggers (Historian's Boon) or resolves (Tom Bombadil).
data SagaFinalChapter = MkSagaFinalChapter
  { moment :: ChapterMoment.ChapterMoment,
    controller :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
