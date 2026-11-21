module Parun.Codec.PermanentsDealDamageToPlayerSpec where

import qualified Parun.Codec.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PermanentsDealDamageToPlayer" $ do
  -- Norn's Decree's payload: the damagers an opponent controls, combat damage,
  -- and CR 109.5's "you" as the damaged player. The two relation keys name
  -- different relations, so a codec that crossed them would not round-trip.
  Spec.it s "MkPermanentsDealDamageToPlayer, combat damage to you" $
    Common.assertCodec
      s
      PermanentsDealDamageToPlayer.codec
      ( PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
          { PermanentsDealDamageToPlayer.filter = Filter.HeldBy Holder.Controller (ControllerRelation.Related PlayerRelation.Opponent),
            PermanentsDealDamageToPlayer.kind = Just DamageKind.Combat,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.You,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = False,
            PermanentsDealDamageToPlayer.anyController = False,
            PermanentsDealDamageToPlayer.orBattle = False,
            PermanentsDealDamageToPlayer.while = Nothing
          }
      )
      " {\"filter\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"Opponent\"}},\"kind\":\"Combat\",\"recipient\":\"You\"} "
  -- "To one or more players" of damage of any kind: of the two defaulted keys,
  -- only the one set appears.
  Spec.it s "MkPermanentsDealDamageToPlayer, one or more players" $
    Common.assertCodec
      s
      PermanentsDealDamageToPlayer.codec
      ( PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
          { PermanentsDealDamageToPlayer.filter = Filter.HeldBy Holder.Controller (ControllerRelation.Related PlayerRelation.You),
            PermanentsDealDamageToPlayer.kind = Nothing,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.AnyPlayer,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = True,
            PermanentsDealDamageToPlayer.anyController = False,
            PermanentsDealDamageToPlayer.orBattle = False,
            PermanentsDealDamageToPlayer.while = Nothing
          }
      )
      " {\"filter\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"You\"}},\"oneOrMorePlayers\":true,\"recipient\":\"AnyPlayer\"} "
  -- Witch-king of Angmar's payload: damagers naming no controller.
  Spec.it s "MkPermanentsDealDamageToPlayer, any controller" $
    Common.assertCodec
      s
      PermanentsDealDamageToPlayer.codec
      ( PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
          { PermanentsDealDamageToPlayer.filter = Filter.And [],
            PermanentsDealDamageToPlayer.kind = Just DamageKind.Combat,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.You,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = False,
            PermanentsDealDamageToPlayer.anyController = True,
            PermanentsDealDamageToPlayer.orBattle = False,
            PermanentsDealDamageToPlayer.while = Nothing
          }
      )
      " {\"anyController\":true,\"filter\":{\"type\":\"And\",\"value\":[]},\"kind\":\"Combat\",\"recipient\":\"You\"} "
  -- Zurgo and Ojutai's "to a player or battle".
  Spec.it s "MkPermanentsDealDamageToPlayer, or battle" $
    Common.assertCodec
      s
      PermanentsDealDamageToPlayer.codec
      ( PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
          { PermanentsDealDamageToPlayer.filter = Filter.And [],
            PermanentsDealDamageToPlayer.kind = Just DamageKind.Combat,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.AnyPlayer,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = False,
            PermanentsDealDamageToPlayer.anyController = False,
            PermanentsDealDamageToPlayer.orBattle = True,
            PermanentsDealDamageToPlayer.while = Nothing
          }
      )
      " {\"filter\":{\"type\":\"And\",\"value\":[]},\"kind\":\"Combat\",\"orBattle\":true,\"recipient\":\"AnyPlayer\"} "
  -- Tamiyo, Upriser Crowned's "while you're the monarch".
  Spec.it s "MkPermanentsDealDamageToPlayer, while" $
    Common.assertCodec
      s
      PermanentsDealDamageToPlayer.codec
      ( PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
          { PermanentsDealDamageToPlayer.filter = Filter.And [],
            PermanentsDealDamageToPlayer.kind = Just DamageKind.Combat,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.You,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = False,
            PermanentsDealDamageToPlayer.anyController = True,
            PermanentsDealDamageToPlayer.orBattle = False,
            PermanentsDealDamageToPlayer.while = Just (Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))
          }
      )
      " {\"anyController\":true,\"filter\":{\"type\":\"And\",\"value\":[]},\"kind\":\"Combat\",\"recipient\":\"You\",\"while\":{\"type\":\"Compares\",\"value\":{\"comparison\":\"Exactly\",\"measured\":{\"type\":\"Literal\",\"value\":0},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentsDealDamageToPlayer.codec
