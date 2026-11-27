module Parun.Types.AttackedSubject where

import Data.Set (Set)
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SourcePlayer as SourcePlayer

-- | CR 508.3b / 508.3e: which attacked player or permanent fires the ability.
data AttackedSubject
  = -- | CR 506.3: a player in this relation to CR 109.5's "you", or such a
    -- player's planeswalker where the kinds admit one.
    --
    -- Unprinted: a battle among the kinds, or no kind at all.
    -- Parun.Codec.AttackedSubject refuses both.
    Related PlayerRelation.PlayerRelation (Set AttackTargetKind.AttackTargetKind)
  | -- | CR 303.4b / 607.2d: the player this ability's source names.
    NamedBySource SourcePlayer.SourcePlayer
  | -- | CR 508.3b: the ability's own source, a planeswalker or battle.
    Source
  deriving (Eq, Ord, Show)
