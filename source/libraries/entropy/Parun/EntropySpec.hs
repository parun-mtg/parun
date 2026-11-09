module Parun.EntropySpec where

import qualified Data.ByteString as ByteString
import qualified Parun.Entropy as Entropy
import qualified Parun.Spec as Spec

spec :: Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Entropy" $ do
  Spec.describe s "seed" $ do
    Spec.it s "draws 32 bytes, different each time" $ do
      one <- Entropy.seed
      two <- Entropy.seed
      Spec.assertEq s (ByteString.length one) 32
      Spec.assertEq s (ByteString.length two) 32
      Spec.assertBool s (one /= two) "differ"
