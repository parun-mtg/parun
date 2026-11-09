module Parun.Codec.ParityOperand where

import qualified Parun.Codec.Parity as Parity
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ParityOperand as ParityOperand

codec :: Codec.Codec ParityOperand.ParityOperand
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Printed" Parity.codec ParityOperand.Printed (\x -> case x of ParityOperand.Printed y -> Just y; _ -> Nothing),
      Arm.nullary "ChosenParity" ParityOperand.ChosenParity
    ]

tagOf :: ParityOperand.ParityOperand -> String
tagOf x = case x of
  ParityOperand.Printed {} -> "Printed"
  ParityOperand.ChosenParity -> "ChosenParity"
