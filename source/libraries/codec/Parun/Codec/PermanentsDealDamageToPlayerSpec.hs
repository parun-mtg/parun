module Parun.Codec.PermanentsDealDamageToPlayerSpec where

import qualified Parun.Codec.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer
import qualified Parun.Types.PlayerRelation as PlayerRelation

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
          { PermanentsDealDamageToPlayer.filter = Filter.HeldBy Holder.Controller PlayerRelation.Opponent,
            PermanentsDealDamageToPlayer.kind = Just DamageKind.Combat,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.You,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = False,
            PermanentsDealDamageToPlayer.anyController = False
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
          { PermanentsDealDamageToPlayer.filter = Filter.HeldBy Holder.Controller PlayerRelation.You,
            PermanentsDealDamageToPlayer.kind = Nothing,
            PermanentsDealDamageToPlayer.recipient = PlayerRelation.AnyPlayer,
            PermanentsDealDamageToPlayer.oneOrMorePlayers = True,
            PermanentsDealDamageToPlayer.anyController = False
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
            PermanentsDealDamageToPlayer.anyController = True
          }
      )
      " {\"anyController\":true,\"filter\":{\"type\":\"And\",\"value\":[]},\"kind\":\"Combat\",\"recipient\":\"You\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentsDealDamageToPlayer.codec
