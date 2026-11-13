module Parun.Codec.SpentOnSpec where

import qualified Parun.Codec.SpentOn as SpentOn
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.SpentOn as SpentOn

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SpentOn" $ do
  Spec.it s "Triggering" $
    Common.assertCodec
      s
      SpentOn.codec
      SpentOn.Triggering
      " {\"type\":\"Triggering\"} "
  Spec.it s "TakingInstruction" $
    Common.assertCodec
      s
      SpentOn.codec
      (SpentOn.TakingInstruction (ClauseIndex.MkClauseIndex 1))
      " {\"type\":\"TakingInstruction\",\"value\":1} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s SpentOn.codec
