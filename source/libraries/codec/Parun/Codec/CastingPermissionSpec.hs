module Parun.Codec.CastingPermissionSpec where

import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Codec.CastingPermission as CastingPermission
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.PermissionCost as PermissionCost
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CastingPermission" $ do
  Spec.it s "CastFromLibraryWhileSearching" $
    Common.assertCodec
      s
      CastingPermission.codec
      CastingPermission.CastFromLibraryWhileSearching
      " {\"type\":\"CastFromLibraryWhileSearching\"} "
  Spec.it s "CastFrom a graveyard" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard Nothing Nothing [] Nothing Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\"}} "
  Spec.it s "CastFrom a graveyard using a keyword" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard Nothing (Just KeywordFamily.Blitz) [] Nothing Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\",\"using\":\"Blitz\"}} "
  Spec.it s "CastFrom a graveyard paying an additional cost" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard Nothing Nothing [CostComponent.PayLife (CostAmount.Fixed 2)] Nothing Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\",\"additionalCosts\":[{\"type\":\"PayLife\",\"value\":{\"type\":\"Fixed\",\"value\":2}}]}} "
  Spec.it s "CastFrom a graveyard paying an alternative cost" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard Nothing Nothing [CostComponent.PayLife (CostAmount.Fixed 2)] Nothing (Just (PermissionCost.InsteadOfManaCost (ManaCost.MkManaCost [ManaSymbol.Generic 3]))))
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\",\"additionalCosts\":[{\"type\":\"PayLife\",\"value\":{\"type\":\"Fixed\",\"value\":2}}],\"alternativeCost\":{\"type\":\"InsteadOfManaCost\",\"value\":[{\"type\":\"Generic\",\"value\":3}]}}} "
  Spec.it s "rejects CastFrom exile paying an alternative cost" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CastingPermission.codec =<< Common.parse (Text.pack "{\"type\":\"CastFrom\",\"value\":{\"from\":\"Exile\",\"alternativeCost\":{\"type\":\"InsteadOfManaCost\",\"value\":[]}}}")))
      "expected a decode failure"
  Spec.it s "CastFrom a graveyard entering with counters" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard Nothing Nothing [] (Just (WithCounters.MkWithCounters (Map.singleton CounterKind.Finality (Quantity.Literal 1)))) Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\",\"entersWith\":[{\"count\":{\"type\":\"Literal\",\"value\":1},\"kind\":{\"type\":\"Finality\"}}]}} "
  Spec.it s "CastFrom a graveyard under a condition" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Graveyard (Just (Condition.Any [])) Nothing [] Nothing Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Graveyard\",\"condition\":{\"type\":\"Any\",\"value\":[]}}} "
  Spec.it s "CastFrom exile" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastFrom Zone.Exile Nothing Nothing [] Nothing Nothing)
      " {\"type\":\"CastFrom\",\"value\":{\"from\":\"Exile\"}} "
  Spec.it s "rejects CastFrom a hand" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CastingPermission.codec =<< Common.parse (Text.pack "{\"type\":\"CastFrom\",\"value\":{\"from\":\"Hand\"}}")))
      "expected a decode failure"
  Spec.it s "rejects CastFrom exile using a keyword" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CastingPermission.codec =<< Common.parse (Text.pack "{\"type\":\"CastFrom\",\"value\":{\"from\":\"Exile\",\"using\":\"Blitz\"}}")))
      "expected a decode failure"
  Spec.it s "PlayFromGraveyard" $
    Common.assertCodec
      s
      CastingPermission.codec
      CastingPermission.PlayFromGraveyard
      " {\"type\":\"PlayFromGraveyard\"} "
  Spec.it s "CastAsThoughItHadFlash unconditioned and free" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastAsThoughItHadFlash Nothing Nothing [] Nothing)
      " {\"type\":\"CastAsThoughItHadFlash\",\"value\":{}} "
  Spec.it s "CastAsThoughItHadFlash under a condition for an additional cost" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastAsThoughItHadFlash (Just (Condition.Any [])) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [] Nothing)
      " {\"type\":\"CastAsThoughItHadFlash\",\"value\":{\"condition\":{\"type\":\"Any\",\"value\":[]},\"additionalMana\":[{\"type\":\"Generic\",\"value\":2}]}} "
  Spec.it s "CastAsThoughItHadFlash creating a delayed ability outside sorcery timing" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastAsThoughItHadFlash Nothing Nothing [] (Just (AbilityName.MkAbilityName (Text.pack "sacrifice it"))))
      " {\"type\":\"CastAsThoughItHadFlash\",\"value\":{\"outsideSorceryTiming\":\"sacrifice it\"}} "
  Spec.it s "CastAsThoughItHadFlash for a non-mana additional cost" $
    Common.assertCodec
      s
      CastingPermission.codec
      (CastingPermission.CastAsThoughItHadFlash Nothing Nothing [CostComponent.PayLife (CostAmount.Fixed 2)] Nothing)
      " {\"type\":\"CastAsThoughItHadFlash\",\"value\":{\"additionalCosts\":[{\"type\":\"PayLife\",\"value\":{\"type\":\"Fixed\",\"value\":2}}]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastingPermission.codec
