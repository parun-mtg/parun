module Parun.Codec.ParityOperandSpec where

import qualified Parun.Codec.ParityOperand as ParityOperand
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.ParityOperand as ParityOperand

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ParityOperand" $ do
  Spec.it s "Printed (Void Winnower)" $
    Common.assertCodec
      s
      ParityOperand.codec
      (ParityOperand.Printed Parity.Even)
      " {\"type\":\"Printed\",\"value\":\"Even\"} "
  Spec.it s "ChosenParity (Gollum, Riddle Master)" $
    Common.assertCodec
      s
      ParityOperand.codec
      ParityOperand.ChosenParity
      " {\"type\":\"ChosenParity\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ParityOperand.codec
