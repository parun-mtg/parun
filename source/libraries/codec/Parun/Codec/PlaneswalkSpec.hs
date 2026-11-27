module Parun.Codec.PlaneswalkSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Planeswalk as Planeswalk
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Planeswalk as Planeswalk
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Planeswalk" $ do
  Spec.it s "the top card, away from every card" $
    Common.assertCodec s Planeswalk.codec (Planeswalk.MkPlaneswalk Nothing PlaneswalkAway.FromEveryCard) " {} "
  -- Norn's Seedcore's shape.
  Spec.it s "named cards, away from phenomena only" $
    Common.assertCodec
      s
      Planeswalk.codec
      (Planeswalk.MkPlaneswalk (Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "revealed")))) PlaneswalkAway.FromPhenomenaOnly)
      " {\"to\":{\"type\":\"InSlot\",\"value\":\"revealed\"},\"away\":\"FromPhenomenaOnly\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Planeswalk.codec
