module Parun.ZeptoSpec where

import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Zepto as Zepto

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Zepto" $ do
  let run :: Zepto.Parser a -> String -> Maybe a
      run p = Zepto.parseMaybe p . Text.pack

  Spec.describe s "char" $ do
    Spec.it s "succeeds with a match" $ do
      Spec.assertEq s (run (Zepto.char 'a') "ab") $ Just 'a'

    Spec.it s "fails with no match" $ do
      Spec.assertEq s (run (Zepto.char 'a') "b") Nothing

    Spec.it s "fails with no input" $ do
      Spec.assertEq s (run (Zepto.char 'a') "") Nothing

  Spec.describe s "string" $ do
    Spec.it s "succeeds with a match" $ do
      Spec.assertEq s (run (Zepto.string $ Text.pack "ab") "abc") . Just $ Text.pack "ab"

    Spec.it s "fails with a partial match" $ do
      Spec.assertEq s (run (Zepto.string $ Text.pack "ab") "ac") Nothing

  Spec.describe s "takeWhile" $ do
    Spec.it s "takes the matching prefix" $ do
      Spec.assertEq s (run ((,) <$> Zepto.takeWhile Char.isDigit <*> Zepto.anyChar) "12x") $ Just (Text.pack "12", 'x')

    Spec.it s "succeeds with no match" $ do
      Spec.assertEq s (run (Zepto.takeWhile Char.isDigit) "x") . Just $ Text.pack ""

  Spec.describe s "peekChar" $ do
    Spec.it s "consumes nothing" $ do
      Spec.assertEq s (run ((,) <$> Zepto.peekChar <*> Zepto.anyChar) "a") $ Just (Just 'a', 'a')

    Spec.it s "succeeds with no input" $ do
      Spec.assertEq s (run Zepto.peekChar "") $ Just Nothing

  Spec.describe s "endOfInput" $ do
    Spec.it s "succeeds at the end" $ do
      Spec.assertEq s (run (Zepto.char 'a' *> Zepto.endOfInput) "a") $ Just ()

    Spec.it s "fails before the end" $ do
      Spec.assertEq s (run Zepto.endOfInput "a") Nothing

  Spec.describe s "<|>" $ do
    Spec.it s "backtracks after the left parser consumed" $ do
      Spec.assertEq s (run ((Zepto.char 'a' *> Zepto.char 'b') Applicative.<|> Zepto.char 'a') "ac") $ Just 'a'

    Spec.it s "keeps the failure that read further" $ do
      Spec.assertEq s (Zepto.parse (Zepto.char 'x' Applicative.<|> (Zepto.char 'a' *> Zepto.char 'b')) $ Text.pack "ac") $ Left "(line 1, column 2):\nunexpected 'c', expecting 'b'"

    Spec.it s "keeps the first failure on a tie" $ do
      Spec.assertEq s (Zepto.parse (Zepto.char 'a' Applicative.<|> Zepto.char 'b') $ Text.pack "c") $ Left "(line 1, column 1):\nunexpected 'c', expecting 'a'"

  Spec.describe s "many" $ do
    Spec.it s "collects every match in order" $ do
      Spec.assertEq s (run (Applicative.many $ Zepto.satisfy Char.isDigit) "12x") $ Just "12"

    Spec.it s "succeeds with no match" $ do
      Spec.assertEq s (run (Applicative.many $ Zepto.satisfy Char.isDigit) "x") $ Just ""

  Spec.describe s "parse" $ do
    Spec.it s "allows unconsumed input" $ do
      Spec.assertEq s (Zepto.parse (Zepto.char 'a') $ Text.pack "ab") $ Right 'a'

    Spec.it s "reports a failure's line and column" $ do
      Spec.assertEq s (Zepto.parse (Zepto.takeWhile (/= 'x') *> Zepto.char 'y') $ Text.pack "a\nbc x") $ Left "(line 2, column 4):\nunexpected 'x', expecting 'y'"

    Spec.it s "advances a tab to the next multiple of eight" $ do
      Spec.assertEq s (Zepto.parse (Zepto.takeWhile (/= 'x') *> Zepto.char 'y') $ Text.pack "a\tx") $ Left "(line 1, column 9):\nunexpected 'x', expecting 'y'"

    Spec.it s "reports the end of input" $ do
      Spec.assertEq s (Zepto.parse (Zepto.char 'a') Text.empty) $ Left "(line 1, column 1):\nunexpected end of input, expecting 'a'"

    Spec.it s "reports a failure's own message" $ do
      Spec.assertEq s (Zepto.parse (Zepto.anyChar *> (fail "bad" :: Zepto.Parser ())) $ Text.pack "ab") $ Left "(line 1, column 2):\nbad"
