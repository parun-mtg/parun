module Parun.Codec.PairwiseFrame where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PairwiseFrame as PairwiseFrame

codec :: Codec.Codec PairwiseFrame.PairwiseFrame
codec = Arm.enum
