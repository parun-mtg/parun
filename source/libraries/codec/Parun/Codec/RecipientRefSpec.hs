module Parun.Codec.RecipientRefSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.RecipientRef as RecipientRef
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.RecipientRef as RecipientRef
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RecipientRef" $ do
  -- Alluring Siren's "attacks you".
  Spec.it s "Players" $
    Common.assertCodec
      s
      RecipientRef.codec
      (RecipientRef.Players (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"Players\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Gideon Jura's "attack Gideon Jura".
  Spec.it s "Permanents" $
    Common.assertCodec
      s
      RecipientRef.codec
      (RecipientRef.Permanents (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Permanents\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RecipientRef.codec
