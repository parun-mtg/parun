module Parun.Types.Selection where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Types.DifferentIn as DifferentIn
import qualified Parun.Types.DifferingValue as DifferingValue
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Threshold as Threshold

-- | What the objects one selection of a Parun.Types.Claim takes must be
-- TOGETHER, where not every group of them serves it: the claim then contends
-- for which objects, not how many.
data Selection
  = -- | Their amounts reach a total (CR 702.122a, 701.59a).
    Reaching Threshold.Threshold
  | -- | This many of them, no two sharing a Parun.Types.DifferentIn value (CR
    -- 201.2b): each pool object's values.
    KeptApart Natural (Map.Map ObjectId.ObjectId (Set.Set (DifferentIn.DifferentIn, DifferingValue.DifferingValue)))
  | -- | This many of them, all holding one creature type (CR 205.3m): each
    -- pool object's creature types.
    SharingOne Natural (Map.Map ObjectId.ObjectId (Set.Set Subtype.Subtype))
  deriving (Eq, Ord, Show)
