-- | A ChaCha20 key made from the operating system's entropy or from a seed
-- given as 64 hexadecimal digits, and shown as one.
module Parun.Play.Key where

import qualified Control.Exception as Exception
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Numeric
import qualified Parun.Entropy as Entropy
import qualified Parun.Exceptions.EntropyUnavailable as EntropyUnavailable
import qualified Parun.Extra.Word8 as Word8
import qualified Parun.Types.Key as Key

-- | A key from the operating system's entropy.
fresh :: IO Key.Key
fresh = do
  bytes <- Entropy.seed
  case fromBytes bytes of
    Just key -> pure key
    Nothing -> Exception.throwIO (EntropyUnavailable.MkEntropyUnavailable (Text.pack "not a key"))

-- | Thirty-two bytes as a key, each four a little-endian word (RFC 8439
-- section 2.3); Nothing for any other length.
fromBytes :: ByteString.ByteString -> Maybe Key.Key
fromBytes bytes =
  let word :: [Word.Word8] -> Word.Word32
      word = foldr (\b w -> Bits.shiftL w 8 .|. Word8.toWord32 b) 0
      chunked xs = case splitAt 4 xs of
        ([], _) -> Seq.empty
        (chunk, rest) -> word chunk Seq.<| chunked rest
   in if ByteString.length bytes == 32
        then Just (Key.UnsafeKey (chunked (ByteString.unpack bytes)))
        else Nothing

-- | Sixty-four hexadecimal digits as a key, two to a byte, in order.
fromHex :: Text.Text -> Maybe Key.Key
fromHex text =
  let byte pair = case Numeric.readHex (Text.unpack pair) of
        [(b, "")] | Text.length pair == 2 -> Just b
        _ -> Nothing
   in fromBytes . ByteString.pack =<< traverse byte (Text.chunksOf 2 text)

-- | The key as 'fromHex' reads it.
toHex :: Key.Key -> Text.Text
toHex key =
  let digits :: Word.Word32 -> String
      digits b = (if b < 0x10 then ('0' :) else id) (Numeric.showHex b "")
      word w = concatMap (\i -> digits (Bits.shiftR w (8 * i) .&. 0xff)) [0, 1, 2, 3]
   in Text.pack (concatMap word (Key.unwrap key))
