module Parun.Json.NullSpec where

import qualified Data.Text as Text
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Json.Null as Null
import qualified Parun.Spec as Spec
import qualified Parun.Zepto as Zepto

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Json.Null" $ do
  Spec.describe s "decode" $ do
    Spec.it s "fails with no match" $ do
      Spec.assertEq s (Zepto.parseMaybe Null.decode (Text.pack "invalid")) Nothing

    Spec.it s "succeeds with a match" $ do
      Spec.assertEq s (Zepto.parseMaybe Null.decode (Text.pack "null")) . Just $ Null.MkNull ()

  Spec.describe s "encode" $ do
    Spec.it s "encodes null" $ do
      Spec.assertEq s (Builder.toString . Null.encode $ Null.MkNull ()) "null"
