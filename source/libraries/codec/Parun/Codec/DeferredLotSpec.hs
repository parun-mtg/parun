module Parun.Codec.DeferredLotSpec where

import qualified Parun.Codec.DeferredLot as DeferredLot
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackChoice as AttackChoice
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.BlockChoice as BlockChoice
import qualified Parun.Types.DeferredLot as DeferredLot
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DeferredLot" $ do
  Spec.it s "without riders" $
    Common.assertCodec s DeferredLot.codec (lot Nothing Nothing) " {\"tokens\":[7,8]} "
  Spec.it s "attacking Any" $
    Common.assertCodec s DeferredLot.codec (lot (Just AttackChoice.Any) Nothing) " {\"attacking\":{\"type\":\"Any\"},\"tokens\":[7,8]} "
  Spec.it s "attacking Specified" $
    Common.assertCodec s DeferredLot.codec (lot (Just (AttackChoice.Specified (AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 3)))) Nothing) " {\"attacking\":{\"type\":\"Specified\",\"value\":{\"type\":\"OfPlaneswalker\",\"value\":3}},\"tokens\":[7,8]} "
  Spec.it s "attacking Under" $
    Common.assertCodec s DeferredLot.codec (lot (Just (AttackChoice.Under (PlayerId.MkPlayerId 1))) Nothing) " {\"attacking\":{\"type\":\"Under\",\"value\":1},\"tokens\":[7,8]} "
  Spec.it s "blocking AnyAttacker" $
    Common.assertCodec s DeferredLot.codec (lot Nothing (Just BlockChoice.AnyAttacker)) " {\"blocking\":{\"type\":\"AnyAttacker\"},\"tokens\":[7,8]} "
  Spec.it s "blocking SpecifiedAttacker" $
    Common.assertCodec s DeferredLot.codec (lot Nothing (Just (BlockChoice.SpecifiedAttacker (ObjectId.MkObjectId 3)))) " {\"blocking\":{\"type\":\"SpecifiedAttacker\",\"value\":3},\"tokens\":[7,8]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DeferredLot.codec
  where
    lot = DeferredLot.MkDeferredLot [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
