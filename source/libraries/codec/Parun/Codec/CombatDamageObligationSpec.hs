module Parun.Codec.CombatDamageObligationSpec where

import qualified Parun.Codec.CombatDamageObligation as CombatDamageObligation
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CombatDamageObligation as CombatDamageObligation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CombatDamageObligation" $ do
  Spec.it s "Mandatory" $
    Common.assertCodec
      s
      CombatDamageObligation.codec
      CombatDamageObligation.Mandatory
      " \"Mandatory\" "
  Spec.it s "Optional" $
    Common.assertCodec
      s
      CombatDamageObligation.codec
      CombatDamageObligation.Optional
      " \"Optional\" "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CombatDamageObligation.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CombatDamageObligation.codec
