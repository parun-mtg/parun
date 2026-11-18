module Parun.Codec.RollEndSpec where

import qualified Parun.Codec.RollEnd as RollEnd
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RollEnd as RollEnd

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollEnd" $ do
  Spec.it s "Highest" $
    Common.assertCodec
      s
      RollEnd.codec
      RollEnd.Highest
      " \"Highest\" "
  Spec.it s "LowestBreakingTies" $
    Common.assertCodec
      s
      RollEnd.codec
      RollEnd.LowestBreakingTies
      " \"LowestBreakingTies\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RollEnd.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RollEnd.codec
