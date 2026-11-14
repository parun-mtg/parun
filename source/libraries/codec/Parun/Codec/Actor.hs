module Parun.Codec.Actor where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Actor as Actor

codec :: Codec.Codec Actor.Actor
codec = Arm.enum
