module Parun.Codec.DeclaredRoleSpec where

import qualified Parun.Codec.DeclaredRole as DeclaredRole
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.DeclaredRole as DeclaredRole

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DeclaredRole" $ do
  Spec.it s "Attacked anything" $
    Common.assertCodec
      s
      DeclaredRole.codec
      (DeclaredRole.Attacked Nothing)
      " {\"type\":\"Attacked\"} "
  Spec.it s "Attacked a battle" $
    Common.assertCodec
      s
      DeclaredRole.codec
      (DeclaredRole.Attacked (Just AttackTargetKind.OfBattle))
      " {\"type\":\"Attacked\",\"value\":\"OfBattle\"} "
  Spec.it s "Blocked" $
    Common.assertCodec
      s
      DeclaredRole.codec
      DeclaredRole.Blocked
      " {\"type\":\"Blocked\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DeclaredRole.codec
