{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SacrificeTrigger where

import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Batching as BatchingType
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.SacrificeTrigger as SacrificeTrigger

-- | A bare object: Parun.Codec.PlayerActsOn's two keys, flattened, beside
-- "batching", which EachOccurrence leaves out.
codec :: Codec.Codec SacrificeTrigger.SacrificeTrigger
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec (PlayerActsOn.player . SacrificeTrigger.acts)
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) (PlayerActsOn.filter . SacrificeTrigger.acts)
  batching <- Fields.defaulted "batching" BatchingType.EachOccurrence Batching.codec SacrificeTrigger.batching
  pure
    SacrificeTrigger.MkSacrificeTrigger
      { SacrificeTrigger.acts = PlayerActsOn.MkPlayerActsOn {PlayerActsOn.player = player, PlayerActsOn.filter = filter_},
        SacrificeTrigger.batching = batching
      }
