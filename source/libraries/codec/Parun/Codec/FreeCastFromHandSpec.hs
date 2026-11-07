module Parun.Codec.FreeCastFromHandSpec where

import qualified Parun.Codec.FreeCastFromHand as FreeCastFromHand
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FreeCastFromHand as FreeCastFromHand
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.FreeCastFromHand" $ do
  -- CR 118.9 / Omniscience: no computed number, so the key is omitted.
  Spec.it s "MkFreeCastFromHand, amount elided" $
    Common.assertCodec
      s
      FreeCastFromHand.codec
      (FreeCastFromHand.MkFreeCastFromHand {FreeCastFromHand.spells = Filter.And [], FreeCastFromHand.amount = Nothing})
      " {\"spells\":{\"type\":\"And\",\"value\":[]}} "
  -- Omnipresence: the number the spells' mana value compares against.
  Spec.it s "MkFreeCastFromHand, amount written" $
    Common.assertCodec
      s
      FreeCastFromHand.codec
      ( FreeCastFromHand.MkFreeCastFromHand
          { FreeCastFromHand.spells = Filter.Measures (Measures.MkMeasures Measure.ManaValue Comparison.AtMost Operand.EnclosingAmount),
            FreeCastFromHand.amount = Just (Quantity.Literal 2)
          }
      )
      " {\"spells\":{\"type\":\"Measures\",\"value\":{\"measure\":\"ManaValue\",\"comparison\":\"AtMost\",\"operand\":{\"type\":\"EnclosingAmount\"}}},\"amount\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FreeCastFromHand.codec
