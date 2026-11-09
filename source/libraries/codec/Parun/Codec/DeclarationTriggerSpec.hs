module Parun.Codec.DeclarationTriggerSpec where

import qualified Parun.Codec.DeclarationTrigger as DeclarationTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackMeasure as AttackMeasure
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.DeclarationTrigger as DeclarationTrigger
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.TriggerFrequency as TriggerFrequency

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DeclarationTrigger" $ do
  -- Boggart Prankster's "whenever you attack".
  Spec.it s "no count writes no key" $
    Common.assertCodec
      s
      DeclarationTrigger.codec
      (DeclarationTrigger.MkDeclarationTrigger PlayerRelation.You Nothing TriggerFrequency.EveryTime Nothing)
      " {\"player\":\"You\"} "
  -- Hermes, Overseer of Elpis' "whenever you attack with one or more Birds".
  Spec.it s "a count writes its key" $
    Common.assertCodec
      s
      DeclarationTrigger.codec
      (DeclarationTrigger.MkDeclarationTrigger PlayerRelation.Opponent (Just (AttackCount.MkAttackCount AttackMeasure.Creatures 1 (Filter.HasSubtype Subtype.Bird))) TriggerFrequency.EveryTime Nothing)
      " {\"player\":\"Opponent\",\"attackers\":{\"floor\":1,\"filter\":{\"type\":\"HasSubtype\",\"value\":\"Bird\"}}} "
  -- Desert Were-Worm's "for the first time each turn".
  Spec.it s "a first-time frequency writes its key" $
    Common.assertCodec
      s
      DeclarationTrigger.codec
      (DeclarationTrigger.MkDeclarationTrigger PlayerRelation.You (Just (AttackCount.MkAttackCount AttackMeasure.TotalPower 12 (Filter.And []))) TriggerFrequency.FirstTimeEachTurn Nothing)
      " {\"player\":\"You\",\"attackers\":{\"measure\":\"TotalPower\",\"floor\":12,\"filter\":{\"type\":\"And\",\"value\":[]}},\"frequency\":\"FirstTimeEachTurn\"} "
  -- The Chief Warg's "whenever you attack while you control ...".
  Spec.it s "a condition writes its key" $
    Common.assertCodec
      s
      DeclarationTrigger.codec
      (DeclarationTrigger.MkDeclarationTrigger PlayerRelation.You Nothing TriggerFrequency.EveryTime (Just (Condition.Compares (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 2)))))
      " {\"player\":\"You\",\"while\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":\"AtLeast\",\"threshold\":{\"type\":\"Literal\",\"value\":2}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DeclarationTrigger.codec
