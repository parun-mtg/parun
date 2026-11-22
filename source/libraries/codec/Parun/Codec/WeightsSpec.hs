module Parun.Codec.WeightsSpec where

import qualified Parun.Codec.Weights as Weights
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Features as Features
import qualified Parun.Types.Weights as Weights

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Weights" $ do
  Spec.it s "each side's weight of each feature" $
    Common.assertCodec s Weights.codec Weights.MkWeights {Weights.ours = Features.MkFeatures 1 2 3 4 5 6, Weights.theirs = Features.MkFeatures (-1) 0 7 8 9 10} " {\"ours\":{\"life\":1,\"hand\":2,\"power\":3,\"toughness\":4,\"lands\":5,\"permanents\":6},\"theirs\":{\"life\":-1,\"hand\":0,\"power\":7,\"toughness\":8,\"lands\":9,\"permanents\":10}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Weights.codec
