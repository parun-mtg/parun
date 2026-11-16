{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Crew where

import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Crew as Crew

-- | A bare object keyed by the record's field names, Parun.Codec.Equip's shape;
-- @timesEachTurn@ is defaulted, so plain crew writes only @power@.
codec :: Codec.Codec Crew.Crew
codec = Fields.object $ do
  power <- Fields.required "power" Common.natural Crew.power
  timesEachTurn <- Fields.defaulted "timesEachTurn" Nothing (Common.maybe Common.natural) Crew.timesEachTurn
  pure Crew.MkCrew {Crew.power = power, Crew.timesEachTurn = timesEachTurn}
