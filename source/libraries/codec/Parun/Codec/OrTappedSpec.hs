module Parun.Codec.OrTappedSpec where

import qualified Parun.Codec.OrTapped as OrTapped
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.OrTapped as OrTapped
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OrTapped" $ do
  Spec.it s "MkOrTapped, unless elided (Razorgrass Field)" $
    Common.assertCodec
      s
      OrTapped.codec
      (OrTapped.MkOrTapped {OrTapped.price = EntryPrice.PayLife 3, OrTapped.unless = Nothing, OrTapped.counters = Nothing})
      " {\"price\":{\"type\":\"PayLife\",\"value\":3}} "
  Spec.it s "MkOrTapped, unless written (Fortified Beachhead)" $
    Common.assertCodec
      s
      OrTapped.codec
      ( OrTapped.MkOrTapped
          { OrTapped.price = EntryPrice.PayLife 3,
            OrTapped.unless =
              Just $
                Condition.Compares
                  (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 1)),
            OrTapped.counters = Nothing
          }
      )
      " {\"price\":{\"type\":\"PayLife\",\"value\":3},\"unless\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":\"AtLeast\",\"threshold\":{\"type\":\"Literal\",\"value\":1}}}} "
  Spec.it s "MkOrTapped, counters written (Dragon's Disciple)" $
    Common.assertCodec
      s
      OrTapped.codec
      ( OrTapped.MkOrTapped
          { OrTapped.price = EntryPrice.Reveal (Filter.HasSubtype Subtype.Dragon),
            OrTapped.unless = Nothing,
            OrTapped.counters = Just (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 1))
          }
      )
      " {\"price\":{\"type\":\"Reveal\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Dragon\"}},\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OrTapped.codec
