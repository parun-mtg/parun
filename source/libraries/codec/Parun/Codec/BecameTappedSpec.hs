module Parun.Codec.BecameTappedSpec where

import qualified Parun.Codec.BecameTapped as BecameTapped
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BecameTapped as BecameTapped
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BecameTapped" $ do
  Spec.it s "MkBecameTapped, both keys" $
    Common.assertCodec
      s
      BecameTapped.codec
      (BecameTapped.MkBecameTapped (ObjectId.MkObjectId 7) (PlayerId.MkPlayerId 1))
      " {\"permanent\":7,\"tapper\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameTapped.codec
