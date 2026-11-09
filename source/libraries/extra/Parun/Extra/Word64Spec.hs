module Parun.Extra.Word64Spec where

import qualified Parun.Extra.Word64 as Word64
import qualified Parun.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Extra.Word64" $ do
  Spec.describe s "toNatural" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Word64.toNatural 0) 0

    Spec.it s "works with the largest Word64" $ do
      Spec.assertEq s (Word64.toNatural 18446744073709551615) 18446744073709551615

  Spec.describe s "toWord32s" $ do
    Spec.it s "puts the low half first" $ do
      Spec.assertEq s (Word64.toWord32s 0x0123456789abcdef) (0x89abcdef, 0x01234567)

    Spec.it s "works with the largest Word64" $ do
      Spec.assertEq s (Word64.toWord32s 18446744073709551615) (4294967295, 4294967295)
