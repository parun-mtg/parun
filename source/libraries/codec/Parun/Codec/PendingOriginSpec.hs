module Parun.Codec.PendingOriginSpec where

import qualified Parun.Codec.PendingOrigin as PendingOrigin
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PendingOrigin as PendingOrigin

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PendingOrigin" $ do
  Spec.it s "AsTransforms" $
    Common.assertCodec s PendingOrigin.codec PendingOrigin.AsTransforms " \"AsTransforms\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PendingOrigin.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PendingOrigin.codec
