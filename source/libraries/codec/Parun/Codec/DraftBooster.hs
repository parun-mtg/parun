module Parun.Codec.DraftBooster where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.DraftBooster as DraftBooster

codec :: Codec.Codec DraftBooster.DraftBooster
codec = Arm.enum
