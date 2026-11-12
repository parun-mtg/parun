module Parun.Codec.CommittedCrimeSpec where

import qualified Parun.Codec.CommittedCrime as CommittedCrime
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CommittedCrime as CommittedCrime
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CommittedCrime" $ do
  Spec.it s "MkCommittedCrime" $
    Common.assertCodec
      s
      CommittedCrime.codec
      ( CommittedCrime.MkCommittedCrime
          { CommittedCrime.player = PlayerId.MkPlayerId 0,
            CommittedCrime.object = ObjectId.MkObjectId 7
          }
      )
      " {\"player\":0,\"object\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CommittedCrime.codec
