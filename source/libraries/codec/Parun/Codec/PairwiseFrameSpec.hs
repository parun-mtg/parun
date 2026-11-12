module Parun.Codec.PairwiseFrameSpec where

import qualified Parun.Codec.PairwiseFrame as PairwiseFrame
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PairwiseFrame as PairwiseFrame

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PairwiseFrame" $ do
  Spec.it s "Source" $
    Common.assertCodec
      s
      PairwiseFrame.codec
      PairwiseFrame.Source
      " \"Source\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PairwiseFrame.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PairwiseFrame.codec
