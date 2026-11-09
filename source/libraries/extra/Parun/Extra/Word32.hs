module Parun.Extra.Word32 where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word

-- | Converts a 'Word.Word32' into a 'Word.Word64'. This is total: every
-- 'Word.Word32' fits a 'Word.Word64'.
toWord64 :: Word.Word32 -> Word.Word64
toWord64 = Maybe.fromMaybe 0 . Bits.toIntegralSized
