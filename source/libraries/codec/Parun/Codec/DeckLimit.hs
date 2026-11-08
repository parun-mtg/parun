module Parun.Codec.DeckLimit where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.DeckLimit as DeckLimit

codec :: Codec.Codec DeckLimit.DeckLimit
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "UpTo" Common.natural DeckLimit.UpTo (\x -> case x of DeckLimit.UpTo y -> Just y; _ -> Nothing),
      Arm.nullary "AnyNumber" DeckLimit.AnyNumber
    ]

tagOf :: DeckLimit.DeckLimit -> String
tagOf x = case x of
  DeckLimit.UpTo {} -> "UpTo"
  DeckLimit.AnyNumber -> "AnyNumber"
