module Parun.Codec.CostPortionSpec where

import qualified Parun.Codec.CostPortion as CostPortion
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CostPortion as CostPortion

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CostPortion" $ do
  Spec.it s "TotalCost" $
    Common.assertCodec s CostPortion.codec CostPortion.TotalCost " \"TotalCost\" "
  Spec.it s "OnX" $
    Common.assertCodec s CostPortion.codec CostPortion.OnX " \"OnX\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CostPortion.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s CostPortion.codec
