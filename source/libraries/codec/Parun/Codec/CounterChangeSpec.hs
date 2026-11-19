module Parun.Codec.CounterChangeSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.CounterChange as CounterChange
import qualified Parun.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterChange as CounterChange
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CounterChange" $ do
  -- CR 715.2b's threshold crossing: a Saga going from one lore counter to three
  -- crosses two at once, which neither count alone can say. BEFORE and AFTER are
  -- both a Natural and deliberately differ here -- a symmetric fixture would
  -- round-trip a codec that swapped them.
  Spec.it s "MkCounterChange, every key" $
    Common.assertCodec
      s
      CounterChange.codec
      ( CounterChange.MkCounterChange
          { CounterChange.object = ObjectId.MkObjectId 1,
            CounterChange.kind = CounterKind.Lore,
            CounterChange.before = 1,
            CounterChange.after = 3,
            CounterChange.snapshot = ObjectSnapshot.MkObjectSnapshot (ObjectId.MkObjectId 1) ProjectedCharacteristicsSpec.testCharacteristics (Just (PlayerId.MkPlayerId 0)) (PlayerId.MkPlayerId 0) Zone.Battlefield Zone.Battlefield Set.empty
          }
      )
      ( " {\"object\":1,\"kind\":{\"type\":\"Lore\"},\"before\":1,\"after\":3,\"snapshot\":{\"object\":1,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"controller\":0,\"owner\":0,\"zone\":\"Battlefield\"}} "
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterChange.codec
