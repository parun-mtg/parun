{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DamageSources where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.SourceDealtDamage as SourceDealtDamage
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DamageSources as DamageSources
import qualified Parun.Types.Filter as Filter.Type

-- | A bare object keyed by the record's field names; an absent `by` admits any
-- source.
codec :: Codec.Codec DamageSources.DamageSources
codec = Fields.object $ do
  dealt <- Fields.required "dealt" (SourceDealtDamage.codec Keyword.codec) DamageSources.dealt
  by <- Fields.defaulted "by" (Filter.Type.And []) (Filter.codec Keyword.codec) DamageSources.by
  pure DamageSources.MkDamageSources {DamageSources.dealt = dealt, DamageSources.by = by}
