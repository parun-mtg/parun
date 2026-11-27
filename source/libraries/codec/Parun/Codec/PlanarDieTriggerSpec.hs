module Parun.Codec.PlanarDieTriggerSpec where

import qualified Parun.Codec.PlanarDieTrigger as PlanarDieTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlanarDieTrigger as PlanarDieTrigger
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlanarDieTrigger" $ do
  -- Pompeii's "whenever you roll a blank on the planar die".
  Spec.it s "round-trips a face" $
    Common.assertCodec
      s
      PlanarDieTrigger.codec
      (PlanarDieTrigger.MkPlanarDieTrigger PlayerRelation.You (Just PlanarDieFace.Blank))
      " {\"player\":\"You\",\"face\":\"Blank\"} "
  -- Stairs to Infinity's "whenever you roll the planar die".
  Spec.it s "round-trips any face" $
    Common.assertCodec
      s
      PlanarDieTrigger.codec
      (PlanarDieTrigger.MkPlanarDieTrigger PlayerRelation.You Nothing)
      " {\"player\":\"You\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlanarDieTrigger.codec
