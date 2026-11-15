module Parun.Codec.OptionalitySpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Optionality as Optionality
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.MayPass as MayPass
import qualified Parun.Types.Optionality as Optionality
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Optionality" $ do
  Spec.it s "Mandatory" $
    Common.assertCodec
      s
      Optionality.codec
      Optionality.Mandatory
      " {\"type\":\"Mandatory\"} "
  -- CR 603.5's unmarked "you may": the asker is elided both ways, so every card
  -- written before the asker existed keeps its spelling.
  Spec.it s "Optional, asker elided" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.Relative PlayerRelation.You) MayPass.AllSeatsFirst)
      " {\"type\":\"Optional\"} "
  -- Jungle Wayfinder's "each player may", which is what the value is for.
  Spec.it s "Optional, asker written" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.Relative PlayerRelation.AnyPlayer) MayPass.AllSeatsFirst)
      " {\"type\":\"Optional\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}} "
  -- Any other reference round trips too -- the payload is a whole PlayerRef and
  -- not an enum of the two producers.
  Spec.it s "Optional, asker named by a slot" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) MayPass.AllSeatsFirst)
      " {\"type\":\"Optional\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Círdan the Shipwright's ruling, each seat deciding and acting in turn.
  Spec.it s "Optional, seat by seat" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.EachPlayerExcept (SlotName.MkSlotName (Text.pack "votes"))) MayPass.SeatBySeat)
      " {\"type\":\"OptionalSeatBySeat\",\"value\":{\"type\":\"EachPlayerExcept\",\"value\":\"votes\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Optionality.codec
