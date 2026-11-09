{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AbilityView where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.StackedAbility as StackedAbility
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AbilityView as AbilityView

-- | A bare object keyed by the record's field names; `source` is null when the
-- seat may not know where the ability came from.
codec :: Codec.Codec AbilityView.AbilityView
codec = Fields.object $ do
  source <- Fields.required "source" (Common.maybe ObjectId.codec) AbilityView.source
  ability <- Fields.required "ability" StackedAbility.codec AbilityView.ability
  pure AbilityView.MkAbilityView {AbilityView.source = source, AbilityView.ability = ability}
