module Parun.Types.TransformR where

import qualified Data.Sequence as Seq
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | The payload of Parun.Types.ReplacementEffect's TransformR arm: which
-- permanents transforming are intercepted, and the effects performed as they
-- do (CR 701.27i).
data TransformR effect = MkTransformR
  { matching :: Filter.Filter Keyword.Keyword,
    effects :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
