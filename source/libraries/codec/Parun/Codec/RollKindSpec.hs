module Parun.Codec.RollKindSpec where

import qualified Parun.Codec.RollKind as RollKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RollKind as RollKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollKind" $ do
  Spec.it s "ToVisit" $
    Common.assertCodec
      s
      RollKind.codec
      RollKind.ToVisit
      " \"ToVisit\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RollKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s RollKind.codec
