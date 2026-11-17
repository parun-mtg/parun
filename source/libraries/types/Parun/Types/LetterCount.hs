module Parun.Types.LetterCount where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Comparison as Comparison

-- | CR 123.6d: how many letters a name sticker holds, compared thus to a
-- number -- Fight the _____ Fight's "with eight or more letters" is AtLeast 8.
data LetterCount = MkLetterCount
  { comparison :: Comparison.Comparison,
    letters :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
