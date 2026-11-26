module Parun.Codec.CopyChoiceSpec where

import qualified Parun.Codec.CopyChoice as CopyChoice
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CopyChoice as CopyChoice

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CopyChoice" $ do
  Spec.it s "ChooseThenTokenOrCast" $
    Common.assertCodec
      s
      CopyChoice.codec
      CopyChoice.ChooseThenTokenOrCast
      " \"ChooseThenTokenOrCast\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CopyChoice.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CopyChoice.codec
