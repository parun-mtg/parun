module Parun.Codec.EnlistedSpec where

import qualified Parun.Codec.Enlisted as Enlisted
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Enlisted as Enlisted
import qualified Parun.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Enlisted" $ do
  -- CR 702.154c. Distinct ids: only an asymmetric case catches a codec that
  -- crossed the enlister and the creature it tapped.
  Spec.it s "MkEnlisted, both keys" $
    Common.assertCodec
      s
      Enlisted.codec
      ( Enlisted.MkEnlisted
          { Enlisted.enlister = ObjectId.MkObjectId 1,
            Enlisted.enlisted = ObjectId.MkObjectId 2
          }
      )
      " {\"enlister\":1,\"enlisted\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Enlisted.codec
