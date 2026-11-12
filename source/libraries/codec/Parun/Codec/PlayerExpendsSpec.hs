module Parun.Codec.PlayerExpendsSpec where

import qualified Parun.Codec.PlayerExpends as PlayerExpends
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerExpends as PlayerExpends
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerExpends" $ do
  Spec.it s "MkPlayerExpends" $
    Common.assertCodec
      s
      PlayerExpends.codec
      ( PlayerExpends.MkPlayerExpends
          { PlayerExpends.player = PlayerRelation.You,
            PlayerExpends.amount = 4
          }
      )
      " {\"player\":\"You\",\"amount\":4} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerExpends.codec
