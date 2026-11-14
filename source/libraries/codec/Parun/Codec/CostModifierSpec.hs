module Parun.Codec.CostModifierSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Parun.Codec.CostModifier as CostModifier
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.AppliedReduction as AppliedReduction
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostModifier as CostModifier.Type
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.CountedAgainst as CountedAgainst
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.PerTarget as PerTarget
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TurnOrdinal as TurnOrdinal
import qualified Parun.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CostModifier" $ do
  -- Every Maybe criterion absent: Thalia's shape, and no key written for any.
  Spec.it s "a spell increase" $
    Common.assertCodec
      s
      CostModifier.codec
      (CostModifier.Type.MkCostModifier CostSubject.Spells (Filter.Not (Filter.HasCardType CardType.Creature)) Nothing Nothing Nothing CountedAgainst.ChangeSource Nothing (CostChange.Increase (ManaCost.MkManaCost [ManaSymbol.Generic 1])))
      " {\"subject\":{\"type\":\"Spells\"},\"matching\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}},\"change\":{\"type\":\"Increase\",\"value\":[{\"type\":\"Generic\",\"value\":1}]}} "
  -- Every criterion present on the activation subject: Professor Hojo's first,
  -- Zirda's kind, Dwarven Mauler's target, Henzie's count, Heartstone's floor.
  Spec.it s "an activation reduction with every criterion" $
    Common.assertCodec
      s
      CostModifier.codec
      ( CostModifier.Type.MkCostModifier
          (CostSubject.Activations (ActivationCriteria.MkActivationCriteria Nothing (Just AbilityKind.NonManaAbility) Nothing Nothing))
          (Filter.HasCardType CardType.Creature)
          (Just Filter.IsSource)
          (Just (PerTarget.MkPerTarget (Filter.HasCardType CardType.Creature) Nothing))
          (Just (Quantity.CommanderCasts (PlayerRef.Relative PlayerRelation.You)))
          CountedAgainst.PaidFor
          (Just (TurnOrdinal.MkTurnOrdinal (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.You)) 1))
          (CostChange.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic 2]) 1 False))
      )
      " {\"subject\":{\"type\":\"Activations\",\"value\":{\"whichKind\":\"NonManaAbility\"}},\"matching\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"whichTargets\":{\"type\":\"IsSource\"},\"perTarget\":{\"matching\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}},\"perEach\":{\"type\":\"CommanderCasts\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}},\"countedAgainst\":\"PaidFor\",\"onlyNth\":{\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}},\"ordinal\":1},\"change\":{\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":2}],\"atLeast\":1}}} "
  -- Uthros Psionicist's second spell.
  Spec.it s "a spell reduction for the second each turn" $
    Common.assertCodec
      s
      CostModifier.codec
      (CostModifier.Type.MkCostModifier CostSubject.Spells (Filter.HasCardType CardType.Creature) Nothing Nothing Nothing CountedAgainst.ChangeSource (Just (TurnOrdinal.MkTurnOrdinal (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.AnyPlayer)) 2)) (CostChange.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic 2]) 0 False)))
      " {\"subject\":{\"type\":\"Spells\"},\"matching\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"onlyNth\":{\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}},\"ordinal\":2},\"change\":{\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":2}]}}} "
  Spec.it s "refuses countedAgainst beside no perEach" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CostModifier.codec =<< Common.parse (Text.pack "{\"subject\":{\"type\":\"Spells\"},\"matching\":{\"type\":\"IsSource\"},\"countedAgainst\":\"PaidFor\",\"change\":{\"type\":\"Increase\",\"value\":[{\"type\":\"Generic\",\"value\":1}]}}")))
      "expected a decode failure"
  -- Each refusal beside its accepted twin, so the refusal is the check's.
  Spec.it s "refuses onlyNth beside whichLoyalty" $
    Spec.assertBool
      s
      (Either.isLeft (decodeActivation "{\"whichLoyalty\":\"LoyaltyAbility\"}" 1) && Either.isRight (decodeActivation "{}" 1))
      "expected a decode failure only beside whichLoyalty"
  Spec.it s "refuses an activation past the first" $
    Spec.assertBool
      s
      (Either.isLeft (decodeActivation "{}" 2))
      "expected a decode failure"
  Spec.it s "refuses a target criterion on an addition to an activation" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CostModifier.codec =<< Common.parse (Text.pack "{\"subject\":{\"type\":\"Activations\",\"value\":{}},\"matching\":{\"type\":\"IsSource\"},\"whichTargets\":{\"type\":\"IsSource\"},\"change\":{\"type\":\"Add\",\"value\":{\"components\":[]}}}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s CostModifier.codec

-- An activation reduction limited to the nth each turn, its criteria as given.
decodeActivation :: String -> Int -> Either Text.Text CostModifier.Type.CostModifier
decodeActivation criteria n =
  Codec.decode CostModifier.codec
    =<< Common.parse (Text.pack ("{\"subject\":{\"type\":\"Activations\",\"value\":" <> criteria <> "},\"matching\":{\"type\":\"IsSource\"},\"onlyNth\":{\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}},\"ordinal\":" <> show n <> "},\"change\":{\"type\":\"Increase\",\"value\":[{\"type\":\"Generic\",\"value\":1}]}}"))
