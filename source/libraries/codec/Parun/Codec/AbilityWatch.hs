{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AbilityWatch where

import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.WatchedAbility as WatchedAbility
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AbilityWatch as AbilityWatch

-- | A bare object keyed by the record's field names, both required: neither
-- half of the printed wording has a default.
codec :: Codec.Codec AbilityWatch.AbilityWatch
codec = Fields.object $ do
  ability <- Fields.required "ability" WatchedAbility.codec AbilityWatch.ability
  controller <- Fields.required "controller" PlayerRelation.codec AbilityWatch.controller
  pure AbilityWatch.MkAbilityWatch {AbilityWatch.ability = ability, AbilityWatch.controller = controller}
