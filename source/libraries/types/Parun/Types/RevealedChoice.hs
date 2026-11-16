module Parun.Types.RevealedChoice where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

-- | CR 702.106b: a player revealed the subtype they secretly chose for an
-- object (A Killer Among Us), shown to every player.
data RevealedChoice = MkRevealedChoice
  { player :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId,
    subtype :: Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
