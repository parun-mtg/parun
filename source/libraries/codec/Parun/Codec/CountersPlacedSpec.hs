module Parun.Codec.CountersPlacedSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.CountersPlaced as CountersPlaced
import qualified Parun.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CountersPlaced as CountersPlaced
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CountersPlaced" $ do
  -- The counts differ, so a codec swapping them would not round-trip.
  Spec.it s "MkCountersPlaced, every key" $
    Common.assertCodec
      s
      CountersPlaced.codec
      ( CountersPlaced.MkCountersPlaced
          { CountersPlaced.putter = PlayerId.MkPlayerId 2,
            CountersPlaced.object = ObjectId.MkObjectId 1,
            CountersPlaced.kind = CounterKind.Lore,
            CountersPlaced.before = 1,
            CountersPlaced.after = 3,
            CountersPlaced.snapshot =
              ObjectSnapshot.MkObjectSnapshot
                { ObjectSnapshot.object = ObjectId.MkObjectId 1,
                  ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
                  ObjectSnapshot.controller = Just (PlayerId.MkPlayerId 0),
                  ObjectSnapshot.owner = PlayerId.MkPlayerId 0,
                  ObjectSnapshot.zone = Zone.Battlefield,
                  ObjectSnapshot.home = Zone.Battlefield,
                  ObjectSnapshot.lapsedGrants = Set.empty
                }
          }
      )
      ( " {\"putter\":2,\"object\":1,\"kind\":{\"type\":\"Lore\"},\"before\":1,\"after\":3,\"snapshot\":{\"object\":1,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"controller\":0,\"owner\":0,\"zone\":\"Battlefield\"}} "
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s CountersPlaced.codec
