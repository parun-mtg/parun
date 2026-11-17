module Parun.Codec.ChoiceDistinctSpec where

import qualified Parun.Codec.ChoiceDistinct as ChoiceDistinct
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChoiceDistinct as ChoiceDistinct

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChoiceDistinct" $ do
  Spec.it s "MayRepeat" $
    Common.assertCodec
      s
      ChoiceDistinct.codec
      ChoiceDistinct.MayRepeat
      " \"MayRepeat\" "
  Spec.it s "Distinct" $
    Common.assertCodec
      s
      ChoiceDistinct.codec
      ChoiceDistinct.Distinct
      " \"Distinct\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ChoiceDistinct.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ChoiceDistinct.codec
