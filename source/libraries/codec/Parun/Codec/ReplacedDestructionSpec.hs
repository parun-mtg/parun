module Parun.Codec.ReplacedDestructionSpec where

import qualified Parun.Codec.ReplacedDestruction as ReplacedDestruction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DestructionCause as DestructionCause
import qualified Parun.Types.ReplacedDestruction as ReplacedDestruction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ReplacedDestruction" $ do
  Spec.it s "a board and a cause" $
    Common.assertCodec
      s
      (ReplacedDestruction.codec Common.natural)
      ReplacedDestruction.MkReplacedDestruction {ReplacedDestruction.board = 3, ReplacedDestruction.cause = DestructionCause.ByEffect}
      " { \"board\": 3, \"cause\": \"ByEffect\" } "
  Spec.it s "has a schema" $ Common.assertHasSchema s (ReplacedDestruction.codec Common.natural)
