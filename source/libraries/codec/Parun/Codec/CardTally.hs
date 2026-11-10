module Parun.Codec.CardTally where

import qualified Parun.Codec.Color as Color
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CardTally as CardTally

-- | Tagged rather than an enum, since one arm carries a colour.
codec :: Codec.Codec CardTally.CardTally
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "ManaValue" CardTally.ManaValue,
      Arm.payload "ManaSymbolsOf" Color.codec CardTally.ManaSymbolsOf (\x -> case x of CardTally.ManaSymbolsOf y -> Just y; _ -> Nothing)
    ]

tagOf :: CardTally.CardTally -> String
tagOf x = case x of
  CardTally.ManaValue {} -> "ManaValue"
  CardTally.ManaSymbolsOf {} -> "ManaSymbolsOf"
