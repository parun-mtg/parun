module Parun.Codec.LifeGainCauseSpec where

import qualified Parun.Codec.LifeGainCause as LifeGainCause
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.LifeGainCause as LifeGainCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LifeGainCause" $ do
  -- CR 119.10.
  Spec.it s "AnyCause" $
    Common.assertCodec s LifeGainCause.codec LifeGainCause.AnyCause " \"AnyCause\" "
  -- CR 609.1: Rain of Gore.
  Spec.it s "ByOwnSpellOrAbility" $
    Common.assertCodec s LifeGainCause.codec LifeGainCause.ByOwnSpellOrAbility " \"ByOwnSpellOrAbility\" "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeGainCause.codec
