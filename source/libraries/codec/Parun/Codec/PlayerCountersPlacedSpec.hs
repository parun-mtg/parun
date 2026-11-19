module Parun.Codec.PlayerCountersPlacedSpec where

import qualified Parun.Codec.PlayerCountersPlaced as PlayerCountersPlaced
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerCountersPlaced as PlayerCountersPlaced
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerCountersPlaced" $ do
  -- The players differ and so do the counts, so a codec swapping either pair
  -- would not round-trip.
  Spec.it s "MkPlayerCountersPlaced, every key" $
    Common.assertCodec
      s
      PlayerCountersPlaced.codec
      ( PlayerCountersPlaced.MkPlayerCountersPlaced
          { PlayerCountersPlaced.putter = PlayerId.MkPlayerId 2,
            PlayerCountersPlaced.player = PlayerId.MkPlayerId 1,
            PlayerCountersPlaced.kind = PlayerCounterKind.Poison,
            PlayerCountersPlaced.before = 1,
            PlayerCountersPlaced.after = 3
          }
      )
      " {\"putter\":2,\"player\":1,\"kind\":\"Poison\",\"before\":1,\"after\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerCountersPlaced.codec
