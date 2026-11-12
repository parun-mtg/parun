module Parun.Codec.ChapterMomentSpec where

import qualified Parun.Codec.ChapterMoment as ChapterMoment
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChapterMoment as ChapterMoment

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChapterMoment" $ do
  Spec.it s "Resolves" $
    Common.assertCodec
      s
      ChapterMoment.codec
      ChapterMoment.Resolves
      " \"Resolves\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ChapterMoment.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ChapterMoment.codec
