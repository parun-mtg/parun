{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.GrantLookAtExiled where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.Types.PlayerRef as PlayerRef.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | The cards are required and CR 702.75a's rider defaults to absent: the
-- keyword is what asks for it, and a card that spells the grant out (Extract
-- Power) gives CR 406.3's permission alone. `player` defaults to CR 109.5's
-- "you", Parun.Codec.GrantPlayFromExile's default for its own.
codec :: Codec.Codec GrantLookAtExiled.GrantLookAtExiled
codec = Fields.object $ do
  cards <- Fields.required "cards" ObjectRef.codec GrantLookAtExiled.cards
  player <- Fields.defaulted "player" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec GrantLookAtExiled.player
  followsExiler <- Fields.defaulted "followsExiler" False Common.boolean GrantLookAtExiled.followsExiler
  pure
    GrantLookAtExiled.MkGrantLookAtExiled
      { GrantLookAtExiled.cards = cards,
        GrantLookAtExiled.player = player,
        GrantLookAtExiled.followsExiler = followsExiler
      }
