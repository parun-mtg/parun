module Parun.Codec.CommanderDeckRule where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule

codec :: Codec.Codec CommanderDeckRule.CommanderDeckRule
codec = Arm.enum
