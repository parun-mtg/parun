{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PastRoll where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Source as Source
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PastRoll as PastRoll

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PastRoll.PastRoll
codec = Fields.object $ do
  ability <- Fields.required "ability" Source.codec PastRoll.ability
  resolution <- Fields.required "resolution" ObjectId.codec PastRoll.resolution
  roller <- Fields.required "roller" PlayerId.codec PastRoll.roller
  total <- Fields.required "total" Common.integer PastRoll.total
  pure PastRoll.MkPastRoll {PastRoll.ability = ability, PastRoll.resolution = resolution, PastRoll.roller = roller, PastRoll.total = total}
