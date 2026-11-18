module Parun.Codec.ShownDieSpec where

import qualified Numeric.Natural as Natural
import qualified Parun.Codec.ShownDie as ShownDie
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ShownDie as ShownDie

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ShownDie" $ do
  -- Distinct numbers, so a codec that swapped the fields would not round-trip.
  Spec.it s "MkShownDie" $
    Common.assertCodec
      s
      (ShownDie.codec Common.natural)
      (ShownDie.MkShownDie {ShownDie.sides = 6, ShownDie.result = 4 :: Natural.Natural})
      " {\"sides\":6,\"result\":4} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (ShownDie.codec Common.natural)
