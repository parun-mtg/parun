module Parun.Codec.ManaSymbolKind where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ManaSymbolKind as ManaSymbolKind

codec :: Codec.Codec ManaSymbolKind.ManaSymbolKind
codec = Arm.enum
