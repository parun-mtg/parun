module Parun.Codec.LandPlayedSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.LandPlayed as LandPlayed
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.CastOrigin as CastOrigin
import qualified Parun.Types.ExileLink as ExileLink
import qualified Parun.Types.LandPlayed as LandPlayed
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LandPlayed" $ do
  -- CR 305.1: a land played out of exile.
  Spec.it s "MkLandPlayed, every key" $
    Common.assertCodec
      s
      LandPlayed.codec
      ( LandPlayed.MkLandPlayed
          { LandPlayed.player = PlayerId.MkPlayerId 1,
            LandPlayed.land = ObjectId.MkObjectId 7,
            LandPlayed.from = CastOrigin.FromZone Zone.Exile,
            LandPlayed.exiledWith = Just ExileLink.MkExileLink {ExileLink.source = ObjectId.MkObjectId 3, ExileLink.ability = Nothing, ExileLink.acquisition = Acquisition.Own, ExileLink.chosen = False, ExileLink.controller = Nothing, ExileLink.tokens = Set.empty}
          }
      )
      " {\"player\":1,\"land\":7,\"from\":{\"type\":\"FromZone\",\"value\":\"Exile\"},\"exiledWith\":{\"source\":3}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LandPlayed.codec
