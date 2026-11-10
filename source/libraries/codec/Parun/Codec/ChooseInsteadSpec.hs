module Parun.Codec.ChooseInsteadSpec where

import qualified Parun.Codec.ChooseInstead as ChooseInstead
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChooseInstead as ChooseInstead
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChooseInstead" $ do
  -- The two selections differ, so a codec that swapped them fails.
  Spec.it s "MkChooseInstead, all three keys" $
    Common.assertCodec
      s
      (ChooseInstead.codec Common.natural)
      (ChooseInstead.MkChooseInstead (Condition.Compares (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 0))) 1 2)
      " {\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":\"AtLeast\",\"threshold\":{\"type\":\"Literal\",\"value\":0}}},\"ordinary\":1,\"instead\":2} "

  Spec.it s "has a schema" $ Common.assertHasSchema s (ChooseInstead.codec Common.natural)
