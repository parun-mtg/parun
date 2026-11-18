module Parun.Codec.PastRollSpec where

import qualified Parun.Codec.PastRoll as PastRoll
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PastRoll as PastRoll
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Source as Source

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PastRoll" $ do
  Spec.it s "MkPastRoll" $
    Common.assertCodec
      s
      PastRoll.codec
      ( PastRoll.MkPastRoll
          { PastRoll.ability = Source.OfCard (PrintingId.MkPrintingId 2),
            PastRoll.resolution = ObjectId.MkObjectId 5,
            PastRoll.roller = PlayerId.MkPlayerId 1,
            PastRoll.total = 7
          }
      )
      " {\"ability\":{\"type\":\"OfCard\",\"value\":2},\"resolution\":5,\"roller\":1,\"total\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PastRoll.codec
