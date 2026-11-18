module Parun.Codec.RollExtremeSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.RollExtreme as RollExtreme
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RollEnd as RollEnd
import qualified Parun.Types.RollExtreme as RollExtreme
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollExtreme" $ do
  -- Ricochet's "the player with the lowest result".
  Spec.it s "the lowest, ties rerolled" $
    Common.assertCodec
      s
      RollExtreme.codec
      RollExtreme.MkRollExtreme {RollExtreme.end = RollEnd.LowestBreakingTies, RollExtreme.slot = SlotName.MkSlotName (Text.pack "lowest")}
      " {\"end\":\"LowestBreakingTies\",\"slot\":\"lowest\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s RollExtreme.codec
