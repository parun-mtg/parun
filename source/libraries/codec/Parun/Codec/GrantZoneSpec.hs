module Parun.Codec.GrantZoneSpec where

import qualified Parun.Codec.GrantZone as GrantZone
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.GrantZone as GrantZone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.GrantZone" $ do
  Spec.it s "Exile" $
    Common.assertCodec
      s
      GrantZone.codec
      GrantZone.Exile
      " \"Exile\" "
  Spec.it s "Graveyard" $
    Common.assertCodec
      s
      GrantZone.codec
      GrantZone.Graveyard
      " \"Graveyard\" "
  Spec.it s "AnyPublic" $
    Common.assertCodec
      s
      GrantZone.codec
      GrantZone.AnyPublic
      " \"AnyPublic\" "
