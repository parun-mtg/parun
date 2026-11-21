module Parun.Json.Array where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Builder
import qualified Parun.Extra.Monoid as Monoid
import qualified Parun.Extra.Semigroup as Semigroup
import qualified Parun.Zepto as Zepto

newtype Array a = MkArray
  { unwrap :: [a]
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser a -> Zepto.Parser (Array a)
decode p = MkArray <$> (Zepto.char '[' *> decodeBlanks *> decodeElements ']' p)

-- | Comma-separated elements, each followed by blanks, then the closing
-- character. The next character picks the branch, so a failure is reported
-- where it happened rather than at the last comma.
decodeElements :: Char -> Zepto.Parser a -> Zepto.Parser [a]
decodeElements close p = do
  c <- Zepto.peekChar
  if c == Just close
    then [] <$ Zepto.anyChar
    else decodeRest close p []

-- | The elements after the ones read so far, newest first.
decodeRest :: Char -> Zepto.Parser a -> [a] -> Zepto.Parser [a]
decodeRest close p acc = do
  x <- p <* decodeBlanks
  c <- Zepto.peekChar
  if c == Just ','
    then Zepto.anyChar *> decodeBlanks *> decodeRest close p (x : acc)
    else reverse (x : acc) <$ Zepto.char close

decodeBlanks :: Zepto.Parser ()
decodeBlanks = Monad.void . Zepto.takeWhile $ \c -> c == ' ' || c == '\t' || c == '\n' || c == '\r'

encode :: (a -> Builder.Builder) -> Array a -> Builder.Builder
encode b =
  Semigroup.around (Builder.charUtf8 '[') (Builder.charUtf8 ']')
    . Monoid.sepBy (Builder.charUtf8 ',')
    . fmap b
    . unwrap
