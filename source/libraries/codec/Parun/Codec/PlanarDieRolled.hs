{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlanarDieRolled where

import qualified Parun.Codec.PlanarDieFace as PlanarDieFace
import qualified Parun.Codec.PlanarRollCause as PlanarRollCause
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlanarDieRolled as PlanarDieRolled
import qualified Parun.Types.PlanarRollCause as PlanarRollCause

codec :: Codec.Codec PlanarDieRolled.PlanarDieRolled
codec = Fields.object $ do
  roller <- Fields.required "roller" PlayerId.codec PlanarDieRolled.roller
  face <- Fields.optional "face" PlanarDieFace.codec PlanarDieRolled.face
  cause <- Fields.defaulted "cause" PlanarRollCause.SpecialAction PlanarRollCause.codec PlanarDieRolled.cause
  pure PlanarDieRolled.MkPlanarDieRolled {PlanarDieRolled.roller = roller, PlanarDieRolled.face = face, PlanarDieRolled.cause = cause}
