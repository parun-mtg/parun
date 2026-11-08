module Parun.Codec.ObjectViewSpec where

import qualified Data.Map.Strict as Map
import qualified Parun.Codec.ObjectView as ObjectView
import qualified Parun.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ObjectView" $ do
  Spec.it s "a seen object" $
    Common.assertCodec
      s
      ObjectView.codec
      (hidden {ObjectView.characteristics = Just ProjectedCharacteristicsSpec.testCharacteristics})
      ( "{\"object\":7,\"owner\":0,\"controller\":1,\"tapped\":true,\"damage\":2,\"counters\":[],\"attachedTo\":null,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"face\":null,\"foretellCostReduction\":null,\"ability\":null}"
      )
  Spec.it s "an ability on the stack" $
    Common.assertCodec
      s
      ObjectView.codec
      (hidden {ObjectView.ability = Just (AbilityView.MkAbilityView (Just (ObjectId.MkObjectId 5)))})
      "{\"object\":7,\"owner\":0,\"controller\":1,\"tapped\":true,\"damage\":2,\"counters\":[],\"attachedTo\":null,\"characteristics\":null,\"face\":null,\"foretellCostReduction\":null,\"ability\":{\"source\":5}}"
  Spec.it s "a hidden object" $
    Common.assertCodec
      s
      ObjectView.codec
      hidden
      hiddenJson
  Spec.it s "a face-down object its seat may look at" $
    Common.assertCodec
      s
      ObjectView.codec
      (hidden {ObjectView.face = Just ProjectedCharacteristicsSpec.testCharacteristics})
      ( "{\"object\":7,\"owner\":0,\"controller\":1,\"tapped\":true,\"damage\":2,\"counters\":[],\"attachedTo\":null,\"characteristics\":null,\"face\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"foretellCostReduction\":null,\"ability\":null}"
      )
  Spec.it s "a foretold card given a foretell cost" $
    Common.assertCodec
      s
      ObjectView.codec
      (hidden {ObjectView.foretellCostReduction = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])})
      "{\"object\":7,\"owner\":0,\"controller\":1,\"tapped\":true,\"damage\":2,\"counters\":[],\"attachedTo\":null,\"characteristics\":null,\"face\":null,\"foretellCostReduction\":[{\"type\":\"Generic\",\"value\":2}],\"ability\":null}"
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectView.codec

-- Tapped and damaged, so neither status key encodes a default.
hidden :: ObjectView.ObjectView
hidden =
  ObjectView.MkObjectView
    { ObjectView.object = ObjectId.MkObjectId 7,
      ObjectView.owner = PlayerId.MkPlayerId 0,
      ObjectView.controller = Just (PlayerId.MkPlayerId 1),
      ObjectView.tapped = TapState.Tapped,
      ObjectView.damage = 2,
      ObjectView.counters = Map.empty,
      ObjectView.attachedTo = Nothing,
      ObjectView.characteristics = Nothing,
      ObjectView.face = Nothing,
      ObjectView.foretellCostReduction = Nothing,
      ObjectView.ability = Nothing
    }

hiddenJson :: String
hiddenJson = "{\"object\":7,\"owner\":0,\"controller\":1,\"tapped\":true,\"damage\":2,\"counters\":[],\"attachedTo\":null,\"characteristics\":null,\"face\":null,\"foretellCostReduction\":null,\"ability\":null}"
