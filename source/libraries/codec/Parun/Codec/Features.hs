{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Features where

import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Features as Features

codec :: Codec.Codec Features.Features
codec = Fields.object $ do
  life <- Fields.required "life" Common.integer Features.life
  hand <- Fields.required "hand" Common.integer Features.hand
  power <- Fields.required "power" Common.integer Features.power
  toughness <- Fields.required "toughness" Common.integer Features.toughness
  lands <- Fields.required "lands" Common.integer Features.lands
  permanents <- Fields.required "permanents" Common.integer Features.permanents
  pure Features.MkFeatures {Features.life = life, Features.hand = hand, Features.power = power, Features.toughness = toughness, Features.lands = lands, Features.permanents = permanents}
