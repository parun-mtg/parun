module Parun.Codec.TapKeyword where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.TapKeyword as TapKeyword

-- | Nullary tags spelled as Parun.Codec.KeywordFamily spells crew and saddle.
codec :: Codec.Codec TapKeyword.TapKeyword
codec = Arm.enum
