module Parun.Codec.SourcePlayerSpec where

import qualified Parun.Codec.SourcePlayer as SourcePlayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.SourcePlayer as SourcePlayer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SourcePlayer" $ do
  Spec.it s "Enchanted" $
    Common.assertCodec
      s
      SourcePlayer.codec
      SourcePlayer.Enchanted
      " \"Enchanted\" "
  Spec.it s "Chosen" $
    Common.assertCodec
      s
      SourcePlayer.codec
      SourcePlayer.Chosen
      " \"Chosen\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s SourcePlayer.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s SourcePlayer.codec
