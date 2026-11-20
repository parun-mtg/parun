module Parun.Codec.AnotherCounter where

import qualified Parun.Codec.EachKind as EachKind
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AnotherCounter as AnotherCounter

-- | Each arm tagged: EachKind's payload its record, ChosenKind's the slot the
-- targets are read from.
codec :: Codec.Codec AnotherCounter.AnotherCounter
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "EachKind" EachKind.codec AnotherCounter.EachKind (\x -> case x of AnotherCounter.EachKind y -> Just y; _ -> Nothing),
      Arm.payload "ChosenKind" SlotName.codec AnotherCounter.ChosenKind (\x -> case x of AnotherCounter.ChosenKind y -> Just y; _ -> Nothing)
    ]

tagOf :: AnotherCounter.AnotherCounter -> String
tagOf x = case x of
  AnotherCounter.EachKind {} -> "EachKind"
  AnotherCounter.ChosenKind {} -> "ChosenKind"
