module Parun.Codec.ExpendedSpec where

import qualified Parun.Codec.Expended as Expended
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Expended as Expended
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Expended" $ do
  Spec.it s "MkExpended" $
    Common.assertCodec
      s
      Expended.codec
      ( Expended.MkExpended
          { Expended.player = PlayerId.MkPlayerId 0,
            Expended.before = 2,
            Expended.after = 5
          }
      )
      " {\"player\":0,\"before\":2,\"after\":5} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Expended.codec
