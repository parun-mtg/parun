module Parun.Codec.CharacteristicColorsSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.CharacteristicColors as CharacteristicColors
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CharacteristicColors as CharacteristicColors
import qualified Parun.Types.Color as Color

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CharacteristicColors" $ do
  Spec.it s "named colours" $
    Common.assertCodec
      s
      CharacteristicColors.codec
      (CharacteristicColors.MkCharacteristicColors {CharacteristicColors.colors = Set.fromList [Color.White, Color.Green], CharacteristicColors.chosen = False})
      " {\"colors\":[\"White\",\"Green\"]} "
  Spec.it s "the chosen colour" $
    Common.assertCodec
      s
      CharacteristicColors.codec
      (CharacteristicColors.MkCharacteristicColors {CharacteristicColors.colors = Set.empty, CharacteristicColors.chosen = True})
      " {\"chosen\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CharacteristicColors.codec
