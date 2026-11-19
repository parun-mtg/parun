module Parun.Codec.OrTappedSpec where

import qualified Parun.Codec.OrTapped as OrTapped
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.OrTapped as OrTapped
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OrTapped" $ do
  Spec.it s "MkOrTapped, unless elided (Razorgrass Field)" $
    Common.assertCodec
      s
      OrTapped.codec
      (OrTapped.MkOrTapped {OrTapped.price = EntryPrice.PayLife 3, OrTapped.unless = Nothing})
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
                  (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 1))
          }
      )
      " {\"price\":{\"type\":\"PayLife\",\"value\":3},\"unless\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":\"AtLeast\",\"threshold\":{\"type\":\"Literal\",\"value\":1}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OrTapped.codec
