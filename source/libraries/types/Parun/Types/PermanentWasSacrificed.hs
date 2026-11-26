module Parun.Types.PermanentWasSacrificed where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SacrificeCause as SacrificeCause

-- | CR 701.21a: a permanent was sacrificed, by whom, and whether CR 702.82b
-- calls it devoured.

-- Named for the event rather than for the constructor, since
-- Parun.Types.PlayerActsOn is already the TRIGGER CONDITION's payload --
-- Parun.Types.SpellWasCast's reason, and the same pair of ends: that one says
-- which sacrifices an ability watches for, this one says which one happened.
data PermanentWasSacrificed = MkPermanentWasSacrificed
  { player :: PlayerId.PlayerId,
    permanent :: ObjectId.ObjectId,
    cause :: SacrificeCause.SacrificeCause
  }
  deriving (Eq, Ord, Show)
