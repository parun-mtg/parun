module Parun.Extra.Word64 where

import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Parun.Extra.Integer as Integer

-- | Converts a 'Word.Word64' into a 'Natural.Natural'. This is total: every
-- 'Word.Word64' is a natural number.
toNatural :: Word.Word64 -> Natural.Natural
toNatural = Integer.toNaturalSaturating . toInteger

-- | Splits a 'Word.Word64' into its low and high 32 bits.
toWord32s :: Word.Word64 -> (Word.Word32, Word.Word32)
toWord32s w =
  let half :: Word.Word64 -> Word.Word32
      half = Maybe.fromMaybe 0 . Bits.toIntegralSized . (.&. 0xffffffff)
   in (half w, half (Bits.shiftR w 32))
