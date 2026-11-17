module Parun.Codec.ChoiceOrdinalSpec where

import qualified Parun.Codec.ChoiceOrdinal as ChoiceOrdinal
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChoiceOrdinal as ChoiceOrdinal

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChoiceOrdinal" $ do
  Spec.it s "First" $
    Common.assertCodec
      s
      ChoiceOrdinal.codec
      ChoiceOrdinal.First
      " \"First\" "
  Spec.it s "Second" $
    Common.assertCodec
      s
      ChoiceOrdinal.codec
      ChoiceOrdinal.Second
      " \"Second\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ChoiceOrdinal.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ChoiceOrdinal.codec
