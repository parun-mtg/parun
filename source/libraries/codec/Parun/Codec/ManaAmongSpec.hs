module Parun.Codec.ManaAmongSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.ManaAmong as ManaAmong
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.ManaAmong as ManaAmong
import qualified Parun.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaAmong" $ do
  Spec.it s "MkManaAmong, one color" $
    Common.assertCodec
      s
      ManaAmong.codec
      (ManaAmong.MkManaAmong (Set.fromList [ManaType.Colored Color.Red, ManaType.Colored Color.Green]) ColorSpread.OneColor)
      " {\"types\":[{\"type\":\"Colored\",\"value\":\"Red\"},{\"type\":\"Colored\",\"value\":\"Green\"}]} "
  Spec.it s "MkManaAmong, any combination" $
    Common.assertCodec
      s
      ManaAmong.codec
      (ManaAmong.MkManaAmong (Set.fromList [ManaType.Colored Color.Blue, ManaType.Colored Color.Red]) ColorSpread.AnyCombination)
      " {\"spread\":\"AnyCombination\",\"types\":[{\"type\":\"Colored\",\"value\":\"Blue\"},{\"type\":\"Colored\",\"value\":\"Red\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaAmong.codec
