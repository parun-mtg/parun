module Parun.Codec.ObjectDamageSpec where

import qualified Parun.Codec.ObjectDamage as ObjectDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.ObjectDamage as ObjectDamage.Type
import qualified Parun.Types.PlayerRef as PlayerRef

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ObjectDamage" $ do
  Spec.it s "any source omits the controller" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.controlledBy = Nothing})
      " {\"direction\":\"DealtTo\"} "
  Spec.it s "sources a player controlled" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.controlledBy = Just PlayerRef.Candidate})
      " {\"direction\":\"DealtTo\",\"controlledBy\":{\"type\":\"Candidate\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectDamage.codec
