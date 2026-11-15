{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ObjectDamage where

import qualified Parun.Codec.DamageDirection as DamageDirection
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ObjectDamage as ObjectDamage

codec :: Codec.Codec ObjectDamage.ObjectDamage
codec = Fields.object $ do
  direction <- Fields.required "direction" DamageDirection.codec ObjectDamage.direction
  controlledBy <- Fields.defaulted "controlledBy" Nothing (Common.maybe PlayerRef.codec) ObjectDamage.controlledBy
  pure ObjectDamage.MkObjectDamage {ObjectDamage.direction = direction, ObjectDamage.controlledBy = controlledBy}
