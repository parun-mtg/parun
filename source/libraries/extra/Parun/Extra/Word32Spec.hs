module Parun.Extra.Word32Spec where

import qualified Parun.Extra.Word32 as Word32
import qualified Parun.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Extra.Word32" $ do
  Spec.describe s "toWord64" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Word32.toWord64 0) 0

    Spec.it s "works with the largest Word32" $ do
      Spec.assertEq s (Word32.toWord64 4294967295) 4294967295
