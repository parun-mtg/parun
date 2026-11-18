module Parun.Codec.TapKeywordSpec where

import qualified Parun.Codec.TapKeyword as TapKeyword
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.TapKeyword as TapKeyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TapKeyword" $ do
  -- Sulu, Heroic Helmsman's "stations Spacecraft".
  Spec.it s "Station" $
    Common.assertCodec
      s
      TapKeyword.codec
      TapKeyword.Station
      " \"Station\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TapKeyword.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s TapKeyword.codec
