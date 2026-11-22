module Parun.Uri.PercentSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Spec as Spec
import qualified Parun.Uri.Percent as Percent

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Uri.Percent" $ do
  let decode = Builder.toString . Builder.byteString . Percent.decode . LazyByteString.toStrict . Builder.toLazyByteString . Builder.stringUtf8
      encode = Builder.toString . Percent.encode Percent.isUnreserved . Builder.stringUtf8

  Spec.describe s "encode" $ do
    Spec.it s "leaves an unreserved octet alone" $ do
      Spec.assertEq s (encode "aZ0-._~") "aZ0-._~"

    Spec.it s "percent encodes a reserved octet" $ do
      Spec.assertEq s (encode "a&b=c") "a%26b%3dc"

  Spec.describe s "decode" $ do
    Spec.it s "decodes a percent-encoded octet in either case" $ do
      Spec.assertEq s (decode "a%26b%3Dc") "a&b=c"

    Spec.it s "reverses encode" $ do
      Spec.assertEq s (decode (encode "a b/c%d")) "a b/c%d"

    Spec.it s "keeps a percent sign without two hex digits" $ do
      Spec.assertEq s (decode "100%-%4") "100%-%4"
