module Parun.Codec.LostPermanentSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.LostPermanent as LostPermanent
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.LostPermanent as LostPermanent
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LostPermanent" $ do
  Spec.it s "Self" $
    Common.assertCodec s LostPermanent.codec LostPermanent.Self " {\"type\":\"Self\"} "
  Spec.it s "Bound" $
    Common.assertCodec s LostPermanent.codec (LostPermanent.Bound (SlotName.MkSlotName (Text.pack "target"))) " {\"type\":\"Bound\",\"value\":\"target\"} "
  Spec.it s "Matching" $
    Common.assertCodec
      s
      LostPermanent.codec
      (LostPermanent.Matching (PlayerActsOn.MkPlayerActsOn PlayerRelation.Opponent (Filter.And [])))
      " {\"type\":\"Matching\",\"value\":{\"player\":\"Opponent\",\"filter\":{\"type\":\"And\",\"value\":[]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LostPermanent.codec
