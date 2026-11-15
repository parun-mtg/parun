module Parun.Types.TransformR where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.TransformRewrite as TransformRewrite

-- | The payload of Parun.Types.ReplacementEffect's TransformR arm: which
-- permanents transforming are intercepted, and what is done as they do (CR
-- 701.27i).
data TransformR effect = MkTransformR
  { matching :: Filter.Filter Keyword.Keyword,
    rewrite :: TransformRewrite.TransformRewrite effect
  }
  deriving (Eq, Ord, Show)
