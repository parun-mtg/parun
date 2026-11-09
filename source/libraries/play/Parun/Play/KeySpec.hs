module Parun.Play.KeySpec where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Types.Key as Key.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Play.Key" $ do
  Spec.describe s "fromBytes" $ do
    Spec.it s "reads each four bytes as a little-endian word (RFC 8439 section 2.3.2)" $ do
      Spec.assertEq s (fmap (Foldable.toList . Key.Type.unwrap) (Key.fromBytes (ByteString.pack [0 .. 31]))) (Just [0x03020100, 0x07060504, 0x0b0a0908, 0x0f0e0d0c, 0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c])

    Spec.it s "refuses any length but 32" $ do
      Spec.assertEq s (Key.fromBytes (ByteString.pack [0 .. 30])) Nothing
      Spec.assertEq s (Key.fromBytes (ByteString.pack [0 .. 32])) Nothing

  Spec.describe s "fromHex" $ do
    Spec.it s "reads two digits to a byte, in order, either case" $ do
      Spec.assertEq s (Key.fromHex (Text.pack "000102030405060708090A0B0C0D0E0F101112131415161718191a1b1c1d1e1f")) (Key.fromBytes (ByteString.pack [0 .. 31]))

    Spec.it s "refuses too few or too many digits, or a non-digit" $ do
      Spec.assertEq s (Key.fromHex (Text.pack (replicate 63 '0'))) Nothing
      Spec.assertEq s (Key.fromHex (Text.pack (replicate 66 '0'))) Nothing
      Spec.assertEq s (Key.fromHex (Text.pack (replicate 62 '0' <> "0g"))) Nothing
      Spec.assertEq s (Key.fromHex (Text.pack "1")) Nothing

  Spec.describe s "toHex" $ do
    Spec.it s "writes what fromHex reads, in lower case" $ do
      let hex = Text.pack "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
      Spec.assertEq s (fmap Key.toHex (Key.fromHex hex)) (Just hex)
      Spec.assertEq s (fmap Key.toHex (Key.fromHex (Text.toUpper hex))) (Just hex)
