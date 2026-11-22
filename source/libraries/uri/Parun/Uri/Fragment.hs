module Parun.Uri.Fragment where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified Parun.Extra.Word8 as Word8
import qualified Parun.Uri.Percent as Percent

-- | Percent-encodes every octet RFC 3986 does not allow in a fragment.
encode :: Builder.Builder -> Builder.Builder
encode = Percent.encode isValid

isValid :: Word.Word8 -> Bool
isValid w =
  Percent.isUnreserved w
    || elem (Char.chr $ Word8.toInt w) "!$&'()*+,;=:@/?"
