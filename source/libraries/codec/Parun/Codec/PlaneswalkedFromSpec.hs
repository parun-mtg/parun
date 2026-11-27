module Parun.Codec.PlaneswalkedFromSpec where

import qualified Parun.Codec.PlaneswalkedFrom as PlaneswalkedFrom
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlaneswalkedFrom as PlaneswalkedFrom

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlaneswalkedFrom" $ do
  Spec.it s "APlane" $
    Common.assertCodec
      s
      PlaneswalkedFrom.codec
      PlaneswalkedFrom.APlane
      " \"APlane\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlaneswalkedFrom.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlaneswalkedFrom.codec
