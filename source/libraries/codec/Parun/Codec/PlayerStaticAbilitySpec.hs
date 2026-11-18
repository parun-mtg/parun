module Parun.Codec.PlayerStaticAbilitySpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerStaticAbility" $ do
  Spec.it s "MkPlayerStaticAbility" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      PlayerStaticAbility.MkPlayerStaticAbility
        { PlayerStaticAbility.scope = PlayerScope.Related PlayerRelation.AnyPlayer,
          PlayerStaticAbility.condition = Nothing,
          PlayerStaticAbility.functionsFrom = Set.empty,
          PlayerStaticAbility.name = Nothing,
          PlayerStaticAbility.effect = PlayerEffect.CantCastMoreThan (Filter.And []) 1
        }
      " {\"scope\":{\"type\":\"AnyPlayer\"},\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":{\"count\":1}}} "
  -- The "as long as" clause (CR 611.3a) round-trips as its own key, and the
  -- case above proves the absent key still decodes -- so a decoder that dropped
  -- the field would keep that one green and fail here.
  Spec.it s "MkPlayerStaticAbility with a condition" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      PlayerStaticAbility.MkPlayerStaticAbility
        { PlayerStaticAbility.scope = PlayerScope.Related PlayerRelation.AnyPlayer,
          PlayerStaticAbility.functionsFrom = Set.empty,
          PlayerStaticAbility.condition = Just (Condition.Compares (Compares.MkCompares (Quantity.IsActivePlayer (PlayerRef.Relative PlayerRelation.You)) Comparison.Exactly (Quantity.Literal 1))),
          PlayerStaticAbility.name = Nothing,
          PlayerStaticAbility.effect = PlayerEffect.CantCastMoreThan (Filter.And []) 1
        }
      " {\"scope\":{\"type\":\"AnyPlayer\"},\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"IsActivePlayer\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}},\"comparison\":\"Exactly\",\"threshold\":{\"type\":\"Literal\",\"value\":1}}},\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":{\"count\":1}}} "
  -- CR 113.6b: a stated zone round-trips as its own key (The Ur-Sphinx).
  Spec.it s "MkPlayerStaticAbility with zones it functions from" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      PlayerStaticAbility.MkPlayerStaticAbility
        { PlayerStaticAbility.scope = PlayerScope.Related PlayerRelation.AnyPlayer,
          PlayerStaticAbility.condition = Nothing,
          PlayerStaticAbility.functionsFrom = Set.fromList [Zone.Battlefield, Zone.Command],
          PlayerStaticAbility.name = Nothing,
          PlayerStaticAbility.effect = PlayerEffect.CantCastMoreThan (Filter.And []) 1
        }
      " {\"scope\":{\"type\":\"AnyPlayer\"},\"functionsFrom\":[\"Battlefield\",\"Command\"],\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":{\"count\":1}}} "
  Spec.it s "rejects a zone" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode PlayerStaticAbility.codec =<< Common.parse (Text.pack "{\"scope\":{\"type\":\"AnyPlayer\"},\"functionsFrom\":[\"Graveyard\"],\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":{\"count\":1}}}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerStaticAbility.codec
