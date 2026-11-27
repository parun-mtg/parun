module Parun.Codec.SourcePlayerSpec where

import qualified Parun.Codec.SourcePlayer as SourcePlayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.SourcePlayer as SourcePlayer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SourcePlayer" $ do
  Spec.it s "Enchanted" $
    Common.assertCodec
      s
      SourcePlayer.codec
      SourcePlayer.Enchanted
      " {\"type\":\"Enchanted\"} "
  Spec.it s "Chosen" $
    Common.assertCodec
      s
      SourcePlayer.codec
      SourcePlayer.Chosen
      " {\"type\":\"Chosen\"} "
  Spec.it s "LastChoseWord" $
    Common.assertCodec
      s
      SourcePlayer.codec
      (SourcePlayer.LastChoseWord 2)
      " {\"type\":\"LastChoseWord\",\"value\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s SourcePlayer.codec
