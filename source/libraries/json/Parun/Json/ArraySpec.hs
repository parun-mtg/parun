module Parun.Json.ArraySpec where

import qualified Control.Applicative as Applicative
import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Json.Array as Array
import qualified Parun.Spec as Spec
import qualified Parun.Zepto as Zepto

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Json.Array" $ do
  Spec.describe s "decode" $ do
    let p :: Zepto.Parser String
        p = Applicative.some $ Zepto.satisfy Char.isDigit

    Spec.it s "succeeds with an empty array" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[]")) . Just $ Array.MkArray []

    Spec.it s "succeeds with an empty array with blank space" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[ ]")) . Just $ Array.MkArray []

    Spec.it s "succeeds with a single element" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[1]")) . Just $ Array.MkArray ["1"]

    Spec.it s "succeeds with a single element with blank space" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[ 1 ]")) . Just $ Array.MkArray ["1"]

    Spec.it s "succeeds with multiple elements" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[1,2]")) . Just $ Array.MkArray ["1", "2"]

    Spec.it s "succeeds with multiple elements with blank space" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[ 1 , 2 ]")) . Just $ Array.MkArray ["1", "2"]

    Spec.it s "fails with leading comma" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[,1]")) Nothing

    Spec.it s "fails with trailing comma" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[1,]")) Nothing

    Spec.it s "fails with extra comma" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[1,,2]")) Nothing

    Spec.it s "fails with only comma" $ do
      Spec.assertEq s (Zepto.parseMaybe (Array.decode p) (Text.pack "[,]")) Nothing

  Spec.describe s "encode" $ do
    let b = Builder.integerDec

    Spec.it s "encodes empty array" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray []) "[]"

    Spec.it s "encodes single element" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray [1]) "[1]"

    Spec.it s "encodes multiple elements" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray [1, 2]) "[1,2]"
