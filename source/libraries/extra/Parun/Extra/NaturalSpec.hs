module Parun.Extra.NaturalSpec where

import qualified Numeric.Natural as Natural
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Extra.Natural" $ do
  Spec.describe s "length" $ do
    Spec.it s "works with no elements" $ do
      Spec.assertEq s (Natural.length "") 0

    Spec.it s "works with one element" $ do
      Spec.assertEq s (Natural.length "a") 1

    Spec.it s "works with more than one element" $ do
      Spec.assertEq s (Natural.length "bc") 2

  Spec.describe s "toDouble" $ do
    Spec.it s "converts exactly" $ do
      Spec.assertEq s (fmap Natural.toDouble [0, 1, 300, 2 ^ (53 :: Int)]) [0, 1, 300, 9007199254740992]

  Spec.describe s "toInt" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Natural.toInt 0) $ Just 0

    Spec.it s "succeeds with the max bound" $ do
      Spec.assertEq s (Natural.toInt maxInt) $ Just maxBound

    Spec.it s "fails beyond the max bound" $ do
      Spec.assertEq s (Natural.toInt $ maxInt + 1) Nothing

  Spec.describe s "toIntSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Natural.toIntSaturating 0) 0

    Spec.it s "works with the max bound" $ do
      Spec.assertEq s (Natural.toIntSaturating maxInt) maxBound

    Spec.it s "clamps to the max bound" $ do
      Spec.assertEq s (Natural.toIntSaturating $ maxInt + 1) maxBound

  Spec.describe s "minusSaturating" $ do
    Spec.it s "subtracts when the result is not negative" $ do
      Spec.assertEq s (Natural.minusSaturating 3 2) 1

    Spec.it s "works when the result is zero" $ do
      Spec.assertEq s (Natural.minusSaturating 2 2) 0

    Spec.it s "clamps to zero" $ do
      Spec.assertEq s (Natural.minusSaturating 2 3) 0

  Spec.describe s "toWord64" $ do
    Spec.it s "succeeds with the max bound" $ do
      Spec.assertEq s (Natural.toWord64 18446744073709551615) $ Just 18446744073709551615

    Spec.it s "fails beyond the max bound" $ do
      Spec.assertEq s (Natural.toWord64 18446744073709551616) Nothing

-- Int's width varies by target: 64 bits natively, 32 on wasm32.
maxInt :: Natural.Natural
maxInt = Integer.toNaturalSaturating $ toInteger (maxBound :: Int)
