{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ObjectDamage where

import qualified Parun.Codec.DamageDirection as DamageDirection
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ObjectDamage as ObjectDamage

codec :: Codec.Codec ObjectDamage.ObjectDamage
codec = Fields.object $ do
  direction <- Fields.required "direction" DamageDirection.codec ObjectDamage.direction
  by <- Fields.defaulted "by" Nothing (Common.maybe (Filter.codec Keyword.codec)) ObjectDamage.by
  pure ObjectDamage.MkObjectDamage {ObjectDamage.direction = direction, ObjectDamage.by = by}
