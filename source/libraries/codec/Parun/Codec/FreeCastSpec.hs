module Parun.Codec.FreeCastSpec where

import qualified Parun.Codec.FreeCast as FreeCast
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FreeCast as FreeCast
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.FreeCast" $ do
  -- CR 118.9 / World War Hulk: no computed number and no zone, so both keys
  -- are omitted.
  Spec.it s "MkFreeCast, amount elided" $
    Common.assertCodec
      s
      FreeCast.codec
      (FreeCast.MkFreeCast {FreeCast.spells = Filter.And [], FreeCast.amount = Nothing, FreeCast.from = Nothing})
      " {\"spells\":{\"type\":\"And\",\"value\":[]}} "
  -- Omniscience: "from your hand".
  Spec.it s "MkFreeCast, from written" $
    Common.assertCodec
      s
      FreeCast.codec
      (FreeCast.MkFreeCast {FreeCast.spells = Filter.And [], FreeCast.amount = Nothing, FreeCast.from = Just Zone.Hand})
      " {\"spells\":{\"type\":\"And\",\"value\":[]},\"from\":\"Hand\"} "
  -- Omnipresence: the number the spells' mana value compares against.
  Spec.it s "MkFreeCast, amount written" $
    Common.assertCodec
      s
      FreeCast.codec
      ( FreeCast.MkFreeCast
          { FreeCast.spells = Filter.Measures (Measures.MkMeasures Measure.ManaValue Comparison.AtMost Operand.EnclosingAmount),
            FreeCast.amount = Just (Quantity.Literal 2),
            FreeCast.from = Nothing
          }
      )
      " {\"spells\":{\"type\":\"Measures\",\"value\":{\"measure\":\"ManaValue\",\"comparison\":\"AtMost\",\"operand\":{\"type\":\"EnclosingAmount\"}}},\"amount\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FreeCast.codec
