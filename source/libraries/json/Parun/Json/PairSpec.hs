module Parun.Json.PairSpec where

import qualified Control.Applicative as Applicative
import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Json.Pair as Pair
import qualified Parun.Json.String as String
import qualified Parun.Spec as Spec
import qualified Parun.Zepto as Zepto

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Json.Pair" $ do
  let pair :: String -> a -> Pair.Pair a
      pair = Pair.MkPair . String.MkString . Text.pack

  Spec.describe s "decode" $ do
    let p :: Zepto.Parser String
        p = Applicative.some $ Zepto.satisfy Char.isDigit

    Spec.it s "succeeds with simple pair" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack "\"a\":1")) . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after name" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack "\"a\" :1")) . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after separator" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack "\"a\": 1")) . Just $ pair "a" "1"

    Spec.it s "fails with missing name" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack ":1")) Nothing

    Spec.it s "fails with missing separator" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack "\"a\" 1")) Nothing

    Spec.it s "fails with extra separator" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack "\"a\"::1")) Nothing

    Spec.it s "fails with missing value" $ do
      Spec.assertEq s (Zepto.parseMaybe (Pair.decode p) (Text.pack ":1")) Nothing

  Spec.describe s "encode" $ do
    let b = Builder.integerDec

    Spec.it s "encodes simple pair" $ do
      Spec.assertEq s (Builder.toString . Pair.encode b $ pair "a" 1) "\"a\":1"
