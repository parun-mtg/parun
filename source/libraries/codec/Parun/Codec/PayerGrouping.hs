module Parun.Codec.PayerGrouping where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PayerGrouping as PayerGrouping

codec :: Codec.Codec PayerGrouping.PayerGrouping
codec = Arm.enum
