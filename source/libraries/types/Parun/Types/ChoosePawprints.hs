module Parun.Types.ChoosePawprints where

import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural

-- | CR 700.2i: "Choose up to `most` {P} worth of modes", each mode worth its
-- entry in `pawprints`, printed order (CR 107.18).
--
-- The weights sit here rather than on Parun.Types.Mode so a selection alone
-- judges an answer: Prompt.ChooseModes carries it to every reader.
-- Parun.Codec.Modal rejects a list whose length is not the mode count, and
-- Parun.Codec.ChoosePawprints a weight of zero, which would let one mode repeat
-- without end.
data ChoosePawprints = MkChoosePawprints
  { most :: Natural.Natural,
    pawprints :: Seq.Seq Natural.Natural
  }
  deriving (Eq, Ord, Show)
