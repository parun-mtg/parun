module Parun.Codec.TappedForManaSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Parun.Codec.TappedForMana as TappedForMana
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.TappedForMana as TappedForMana
import qualified Parun.Types.Zone as Zone

-- The permanent object 9 as it was tapped, and its encoding: the snapshot every
-- TappedForMana case here and in Parun.Codec.GameEventSpec carries.
testSnapshot :: ObjectSnapshot.ObjectSnapshot
testSnapshot =
  ObjectSnapshot.MkObjectSnapshot
    { ObjectSnapshot.object = ObjectId.MkObjectId 9,
      ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
      ObjectSnapshot.controller = Just (PlayerId.MkPlayerId 1),
      ObjectSnapshot.owner = PlayerId.MkPlayerId 1,
      ObjectSnapshot.zone = Zone.Battlefield,
      ObjectSnapshot.home = Zone.Battlefield,
      ObjectSnapshot.lapsedGrants = Set.empty
    }

testSnapshotJson :: String
testSnapshotJson = "{\"object\":9,\"characteristics\":" <> ProjectedCharacteristicsSpec.testCharacteristicsJson <> ",\"controller\":1,\"owner\":1,\"zone\":\"Battlefield\"}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TappedForMana" $ do
  Spec.it s "MkTappedForMana, one type" $
    Common.assertCodec
      s
      TappedForMana.codec
      (TappedForMana.MkTappedForMana {TappedForMana.permanent = ObjectId.MkObjectId 9, TappedForMana.mana = Set.singleton (ManaType.Colored Color.Green), TappedForMana.player = PlayerId.MkPlayerId 1, TappedForMana.snapshot = testSnapshot})
      (" {\"mana\":[{\"type\":\"Colored\",\"value\":\"Green\"}],\"permanent\":9,\"player\":1,\"snapshot\":" <> testSnapshotJson <> "} ")
  -- Two types off one activation, which is what a permanent adding "{B}{G}"
  -- writes and what CR 106.12a's narrowing then reads either of.
  Spec.it s "MkTappedForMana, two types" $
    Common.assertCodec
      s
      TappedForMana.codec
      (TappedForMana.MkTappedForMana {TappedForMana.permanent = ObjectId.MkObjectId 4, TappedForMana.mana = Set.fromList [ManaType.Colored Color.Black, ManaType.Colorless], TappedForMana.player = PlayerId.MkPlayerId 2, TappedForMana.snapshot = testSnapshot})
      (" {\"mana\":[{\"type\":\"Colored\",\"value\":\"Black\"},{\"type\":\"Colorless\"}],\"permanent\":4,\"player\":2,\"snapshot\":" <> testSnapshotJson <> "} ")
  Spec.it s "has a schema" $ Common.assertHasSchema s TappedForMana.codec
