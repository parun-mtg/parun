{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PermanentsDealDamageToPlayer where

import qualified Parun.Codec.DamageKind as DamageKind
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer

-- | A bare object keyed by the record's field names. `filter` and `recipient`
-- are required: a card that qualifies only the damagers writes AnyPlayer for
-- the recipient, which is what "deal damage to a player" says. `kind` defaults
-- to any damage and `oneOrMorePlayers` to that same "to a player", DamageTrigger's
-- defaults.
codec :: Codec.Codec PermanentsDealDamageToPlayer.PermanentsDealDamageToPlayer
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentsDealDamageToPlayer.filter
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) PermanentsDealDamageToPlayer.kind
  recipient <- Fields.required "recipient" PlayerRelation.codec PermanentsDealDamageToPlayer.recipient
  oneOrMorePlayers <- Fields.defaulted "oneOrMorePlayers" False Common.boolean PermanentsDealDamageToPlayer.oneOrMorePlayers
  pure
    PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer
      { PermanentsDealDamageToPlayer.filter = filter_,
        PermanentsDealDamageToPlayer.kind = kind,
        PermanentsDealDamageToPlayer.recipient = recipient,
        PermanentsDealDamageToPlayer.oneOrMorePlayers = oneOrMorePlayers
      }
