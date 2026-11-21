module Parun.Json.BooleanSpec where

import qualified Data.Text as Text
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Json.Boolean as Boolean
import qualified Parun.Spec as Spec
import qualified Parun.Zepto as Zepto

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Json.Boolean" $ do
  Spec.describe s "decode" $ do
    Spec.it s "fails with no match" $ do
      Spec.assertEq s (Zepto.parseMaybe Boolean.decode (Text.pack "invalid")) Nothing

    Spec.it s "succeeds with false" $ do
      Spec.assertEq s (Zepto.parseMaybe Boolean.decode (Text.pack "false")) . Just $ Boolean.MkBoolean False

    Spec.it s "succeeds with true" $ do
      Spec.assertEq s (Zepto.parseMaybe Boolean.decode (Text.pack "true")) . Just $ Boolean.MkBoolean True

  Spec.describe s "encode" $ do
    Spec.it s "encodes false" $ do
      Spec.assertEq s (Builder.toString . Boolean.encode $ Boolean.MkBoolean False) "false"

    Spec.it s "encodes true" $ do
      Spec.assertEq s (Builder.toString . Boolean.encode $ Boolean.MkBoolean True) "true"
