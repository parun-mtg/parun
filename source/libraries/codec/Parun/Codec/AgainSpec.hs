module Parun.Codec.AgainSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Again as Again
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Again as Again
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Again" $ do
  Spec.it s "AskPlayer" $
    Common.assertCodec s Again.codec (Again.AskPlayer (PlayerRef.Relative PlayerRelation.You)) " {\"type\":\"AskPlayer\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Another Round's "X more times".
  Spec.it s "MoreTimes" $
    Common.assertCodec s Again.codec (Again.MoreTimes (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")))) " {\"type\":\"MoreTimes\",\"value\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Again.codec
