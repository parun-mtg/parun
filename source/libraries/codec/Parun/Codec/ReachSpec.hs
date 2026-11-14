module Parun.Codec.ReachSpec where

import qualified Parun.Codec.Reach as Reach
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Reach as Reach

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Reach" $ do
  Spec.it s "PublicZone" $
    Common.assertCodec
      s
      Reach.codec
      Reach.PublicZone
      " \"PublicZone\" "
  Spec.it s "AnyZone" $
    Common.assertCodec
      s
      Reach.codec
      Reach.AnyZone
      " \"AnyZone\" "
