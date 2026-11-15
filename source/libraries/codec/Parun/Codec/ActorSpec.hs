module Parun.Codec.ActorSpec where

import qualified Parun.Codec.Actor as Actor
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Actor as Actor

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Actor" $ do
  Spec.it s "Source" $
    Common.assertCodec
      s
      Actor.codec
      Actor.Source
      " \"Source\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Actor.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s Actor.codec
