module Parun.Codec.PayerGroupingSpec where

import qualified Parun.Codec.PayerGrouping as PayerGrouping
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PayerGrouping as PayerGrouping

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PayerGrouping" $ do
  Spec.it s "Severally" $
    Common.assertCodec
      s
      PayerGrouping.codec
      PayerGrouping.Severally
      " \"Severally\" "
  Spec.it s "Jointly" $
    Common.assertCodec
      s
      PayerGrouping.codec
      PayerGrouping.Jointly
      " \"Jointly\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PayerGrouping.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PayerGrouping.codec
