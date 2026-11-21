module Parun.Json.Number where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Decimal as Decimal
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Ord as Ord
import qualified Parun.Zepto as Zepto

newtype Number = MkNumber
  { unwrap :: Decimal.Decimal
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser Number
decode = do
  sign <- ifNext (== '-') id $ negate <$ Zepto.anyChar
  int <- integerPart
  frac <- ifNext (== '.') Text.empty fractionPart
  ex <- ifNext (\c -> c == 'e' || c == 'E') 0 exponentPart
  let mantissa = sign . fromDigits $ int <> frac
      scale = ex - toInteger (Text.length frac)
  pure . MkNumber $ Decimal.mkDecimal mantissa scale

-- | Runs the parser when the next character satisfies the predicate, committed
-- to it, and otherwise returns the default without consuming anything.
ifNext :: (Char -> Bool) -> a -> Zepto.Parser a -> Zepto.Parser a
ifNext f x p = do
  c <- Zepto.peekChar
  case c of
    Just d | f d -> p
    _ -> pure x

integerPart :: Zepto.Parser Text.Text
integerPart = do
  c <- Zepto.peekChar
  if c == Just '0'
    then do
      _ <- Zepto.anyChar
      d <- Zepto.peekChar
      case d of
        Just e | Char.isDigit e -> Zepto.anyChar *> fail ("unexpected " <> show e <> " after a leading zero")
        _ -> pure $ Text.singleton '0'
    else Text.cons <$> Zepto.satisfy (Ord.between '1' '9') <*> Zepto.takeWhile Char.isDigit

fractionPart :: Zepto.Parser Text.Text
fractionPart = Zepto.char '.' *> digits

exponentPart :: Zepto.Parser Integer
exponentPart = do
  _ <- Zepto.anyChar
  sign <- ifNext (\c -> c == '-' || c == '+') id $ do
    c <- Zepto.anyChar
    pure $ if c == '-' then negate else id
  ds <- digits
  pure . sign $ fromDigits ds

-- | One or more digits.
digits :: Zepto.Parser Text.Text
digits = Text.cons <$> Zepto.satisfy Char.isDigit <*> Zepto.takeWhile Char.isDigit

fromDigits :: Text.Text -> Integer
fromDigits = Text.foldl' (\n c -> (10 * n) + toInteger (Char.digitToInt c)) 0

-- | An integer is written out in full, as a person writes one (50, not 5e1),
-- up to six trailing zeros, which reaches the largest number a card prints
-- (Gleemax's {1000000}); past that, and for a fraction, the exponent is
-- written.
encode :: Number -> Builder.Builder
encode n =
  let d = unwrap n
      e = Decimal.exponent d
   in Builder.integerDec (Decimal.mantissa d)
        <> if e == 0
          then mempty
          else
            if e > 0 && e <= 6
              then Builder.string7 (replicate (Integer.toIntSaturating e) '0')
              else Builder.charUtf8 'e' <> Builder.integerDec e
