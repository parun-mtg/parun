module Parun.Types.SourcePlayer where

import qualified Numeric.Natural as Natural

-- | Which players an ability's SOURCE names, read off the source itself:
-- Parun.Engine.Game.sourcePlayersOf is the one reading. Carried as the NamedBySource
-- arm of Parun.Types.ControllerRelation, Parun.Types.PlayerScope,
-- Parun.Types.AttackedSubject and Parun.Types.TargetedSubject.
data SourcePlayer
  = -- | CR 303.4b: the player the source enchants.
    Enchanted
  | -- | CR 607.2d: the source's chosen player (PlayerChoice.ChosenPlayer).
    Chosen
  | -- | CR 607.2f: the players whose last choice for the source was its word at this 1-based position (Object.chosenNumberBy; Two Streams Facility).
    LastChoseWord Natural.Natural
  deriving (Eq, Ord, Show)
