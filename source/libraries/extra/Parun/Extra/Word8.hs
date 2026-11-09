module Parun.Extra.Word8 where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word

-- | Converts a 'Word.Word8' into an 'Int'. This is total: every 'Word.Word8'
-- fits an 'Int'.
toInt :: Word.Word8 -> Int
toInt = fromEnum

-- | Converts a 'Word.Word8' into a 'Word.Word32'. This is total: every
-- 'Word.Word8' fits a 'Word.Word32'.
toWord32 :: Word.Word8 -> Word.Word32
toWord32 = Maybe.fromMaybe 0 . Bits.toIntegralSized
