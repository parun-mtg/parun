module Parun.Codec.CounterLimitSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.CounterLimit as CounterLimit
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLimit as CounterLimit
import qualified Parun.Types.CounterName as CounterName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CounterLimit" $ do
  -- Rasputin Dreamweaver's last sentence (CR 122.4).
  Spec.it s "MkCounterLimit" $
    Common.assertCodec
      s
      CounterLimit.codec
      (CounterLimit.MkCounterLimit (CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "dream"))) 7)
      " {\"atMost\":7,\"kind\":{\"type\":\"Named\",\"value\":\"dream\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterLimit.codec
