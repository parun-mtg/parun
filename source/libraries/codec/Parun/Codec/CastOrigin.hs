module Parun.Codec.CastOrigin where

import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CastOrigin as CastOrigin

codec :: Codec.Codec CastOrigin.CastOrigin
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "FromZone" Zone.codec CastOrigin.FromZone (\x -> case x of CastOrigin.FromZone y -> Just y; _ -> Nothing),
      Arm.nullary "OutsideTheGame" CastOrigin.OutsideTheGame
    ]

-- | Total, so a new constructor is an error here rather than a silent @{}@.
tagOf :: CastOrigin.CastOrigin -> String
tagOf x = case x of
  CastOrigin.FromZone _ -> "FromZone"
  CastOrigin.OutsideTheGame -> "OutsideTheGame"
