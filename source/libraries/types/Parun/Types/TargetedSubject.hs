module Parun.Types.TargetedSubject where

import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 115.1 / 601.2c: which targets a becomes-targeted condition watches --
-- permanents, players standing in a relation to CR 109.5's "you", or both.
data TargetedSubject
  = -- | A permanent the subject admits: ward's bearer, Venerated Rotpriest's
    -- "a creature you control" (CR 702.21a, 601.2c).
    Permanent ActingPermanent.ActingPermanent
  | -- | A player alone: Dormant Gomazoa's "whenever you become the target of a
    -- spell" (CR 109.5).
    Player PlayerRelation.PlayerRelation
  | -- | A player or a permanent: Leyline of Combustion's "you and/or at least
    -- one permanent you control", Loki, God of Mischief's "a player or
    -- permanent" (CR 115.1).
    PlayerOrPermanent PlayerRelation.PlayerRelation ActingPermanent.ActingPermanent
  deriving (Eq, Ord, Show)

-- | The permanent the subject admits, if any.
permanentOf :: TargetedSubject -> Maybe ActingPermanent.ActingPermanent
permanentOf x = case x of
  Permanent p -> Just p
  Player _ -> Nothing
  PlayerOrPermanent _ p -> Just p

-- | How a targeted player must stand to CR 109.5's "you" for the subject to
-- admit them, if a player can be admitted at all.
playerOf :: TargetedSubject -> Maybe PlayerRelation.PlayerRelation
playerOf x = case x of
  Permanent _ -> Nothing
  Player r -> Just r
  PlayerOrPermanent r _ -> Just r
