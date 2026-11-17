module Parun.Codec.RerollReachSpec where

import qualified Parun.Codec.RerollReach as RerollReach
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RerollReach as RerollReach

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RerollReach" $ do
  Spec.it s "ThatDie" $
    Common.assertCodec
      s
      RerollReach.codec
      RerollReach.ThatDie
      " \"ThatDie\" "
  Spec.it s "AnyOfTheDice" $
    Common.assertCodec
      s
      RerollReach.codec
      RerollReach.AnyOfTheDice
      " \"AnyOfTheDice\" "
  Spec.it s "has a schema" $ Common.assertHasSchema s RerollReach.codec
