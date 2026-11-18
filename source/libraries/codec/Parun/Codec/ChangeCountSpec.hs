module Parun.Codec.ChangeCountSpec where

import qualified Parun.Codec.ChangeCount as ChangeCount
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChangeCount as ChangeCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChangeCount" $ do
  Spec.it s "Every" $
    Common.assertCodec
      s
      ChangeCount.codec
      ChangeCount.Every
      " \"Every\" "
  Spec.it s "One" $
    Common.assertCodec
      s
      ChangeCount.codec
      ChangeCount.One
      " \"One\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ChangeCount.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ChangeCount.codec
