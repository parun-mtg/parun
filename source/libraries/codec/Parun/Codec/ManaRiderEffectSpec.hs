module Parun.Codec.ManaRiderEffectSpec where

import qualified Parun.Codec.ManaRiderEffect as ManaRiderEffect
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaRiderEffect as ManaRiderEffect

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaRiderEffect" $ do
  Spec.it s "CantBeCountered" $
    Common.assertCodec
      s
      ManaRiderEffect.codec
      ManaRiderEffect.CantBeCountered
      " { \"type\": \"CantBeCountered\" } "
  Spec.it s "GainsKeyword" $
    Common.assertCodec
      s
      ManaRiderEffect.codec
      (ManaRiderEffect.GainsKeyword Keyword.Riot Duration.Indefinite)
      " { \"type\": \"GainsKeyword\", \"value\": { \"duration\": { \"type\": \"Indefinite\" }, \"keyword\": { \"type\": \"Riot\" } } } "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaRiderEffect.codec
