-- | Percent-encoding (RFC 3986, section 2.1), shared by every URI component.
module Parun.Uri.Percent where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified Parun.Extra.Word8 as Word8

-- | Percent-encodes every octet the predicate does not allow.
encode :: (Word.Word8 -> Bool) -> Builder.Builder -> Builder.Builder
encode allowed =
  foldMap (encodeOctet allowed)
    . LazyByteString.unpack
    . Builder.toLazyByteString

encodeOctet :: (Word.Word8 -> Bool) -> Word.Word8 -> Builder.Builder
encodeOctet allowed w =
  if allowed w
    then Builder.word8 w
    else
      let (hi, lo) = divMod w 16
       in Builder.charUtf8 '%' <> Builder.word8Hex hi <> Builder.word8Hex lo

-- | An unreserved octet (RFC 3986, section 2.3), which no component encodes.
isUnreserved :: Word.Word8 -> Bool
isUnreserved w =
  let c = Char.chr $ Word8.toInt w
   in Char.isAsciiUpper c
        || Char.isAsciiLower c
        || Char.isDigit c
        || elem c "-._~"

-- | Decodes every percent-encoded octet; a percent sign not followed by two
-- hex digits stands for itself.
decode :: ByteString.ByteString -> ByteString.ByteString
decode = LazyByteString.toStrict . Builder.toLazyByteString . decodeFrom

decodeFrom :: ByteString.ByteString -> Builder.Builder
decodeFrom input =
  let (plain, rest) = ByteString.break (== 0x25) input
   in Builder.byteString plain <> case ByteString.unpack (ByteString.take 3 rest) of
        [] -> mempty
        [_, hi, lo]
          | Just h <- hexValue hi,
            Just l <- hexValue lo ->
              Builder.word8 (16 * h + l) <> decodeFrom (ByteString.drop 3 rest)
        _ -> Builder.word8 0x25 <> decodeFrom (ByteString.drop 1 rest)

-- | A hex digit's value.
hexValue :: Word.Word8 -> Maybe Word.Word8
hexValue w
  | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
  | w >= 0x41 && w <= 0x46 = Just (w - 0x37)
  | w >= 0x61 && w <= 0x66 = Just (w - 0x57)
  | otherwise = Nothing
