{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Planeswalk where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlaneswalkAway as PlaneswalkAway
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Planeswalk as Planeswalk
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway.Type

-- | A bare object keyed by the record's field names, each elided at its
-- default: the top card, walking away from every face-up card. The tag that
-- picks it is written by Parun.Codec.Effect's Planeswalk arm.
codec :: Codec.Codec Planeswalk.Planeswalk
codec = Fields.object $ do
  to <- Fields.defaulted "to" Nothing (Common.maybe ObjectRef.codec) Planeswalk.to
  away <- Fields.defaulted "away" PlaneswalkAway.Type.FromEveryCard PlaneswalkAway.codec Planeswalk.away
  pure Planeswalk.MkPlaneswalk {Planeswalk.to = to, Planeswalk.away = away}
