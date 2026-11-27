module Parun.Types.SourcePlayer where

-- | Which player an ability's SOURCE names, read off the source itself:
-- Parun.Engine.Game.sourcePlayerOf is the one reading. Carried as the NamedBySource
-- arm of Parun.Types.ControllerRelation, Parun.Types.PlayerScope,
-- Parun.Types.AttackedSubject and Parun.Types.TargetedSubject.
--
-- Bounded and Enum for Parun.Codec.SourcePlayer's Arm.enum.
data SourcePlayer
  = -- | CR 303.4b: the player the source enchants.
    Enchanted
  | -- | CR 607.2d: the source's chosen player (PlayerChoice.ChosenPlayer).
    Chosen
  deriving (Bounded, Enum, Eq, Ord, Show)
