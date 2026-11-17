module Parun.Extra.IntegerSpec where

import qualified Parun.Extra.Integer as Integer
import qualified Parun.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Extra.Integer" $ do
  Spec.describe s "toInt" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Integer.toInt 0) $ Just 0

    Spec.it s "succeeds with the max bound" $ do
      Spec.assertEq s (Integer.toInt maxInt) $ Just maxBound

    Spec.it s "fails beyond the max bound" $ do
      Spec.assertEq s (Integer.toInt $ maxInt + 1) Nothing

    Spec.it s "succeeds with the min bound" $ do
      Spec.assertEq s (Integer.toInt minInt) $ Just minBound

    Spec.it s "fails beyond the min bound" $ do
      Spec.assertEq s (Integer.toInt $ minInt - 1) Nothing

  Spec.describe s "toIntSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Integer.toIntSaturating 0) 0

    Spec.it s "works with the max bound" $ do
      Spec.assertEq s (Integer.toIntSaturating maxInt) maxBound

    Spec.it s "clamps to the max bound" $ do
      Spec.assertEq s (Integer.toIntSaturating $ maxInt + 1) maxBound

    Spec.it s "works with the min bound" $ do
      Spec.assertEq s (Integer.toIntSaturating minInt) minBound

    Spec.it s "clamps to the min bound" $ do
      Spec.assertEq s (Integer.toIntSaturating $ minInt - 1) minBound

  Spec.describe s "toNatural" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Integer.toNatural 0) $ Just 0

    Spec.it s "succeeds with a big positive number" $ do
      Spec.assertEq s (Integer.toNatural 1234567890) $ Just 1234567890

    Spec.it s "fails with a negative number" $ do
      Spec.assertEq s (Integer.toNatural (-1)) Nothing

  Spec.describe s "toNaturalSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Integer.toNaturalSaturating 0) 0

    Spec.it s "works with a big positive number" $ do
      Spec.assertEq s (Integer.toNaturalSaturating 1234567890) 1234567890

    Spec.it s "converts a negative number to zero" $ do
      Spec.assertEq s (Integer.toNaturalSaturating (-1)) 0

-- Int's width varies by target: 64 bits natively, 32 on wasm32.
maxInt :: Integer
maxInt = toInteger (maxBound :: Int)

minInt :: Integer
minInt = toInteger (minBound :: Int)
