module Parun.Json.Pair where

import qualified Data.ByteString.Builder as Builder
import qualified Parun.Json.Array as Array
import qualified Parun.Json.String as String
import qualified Parun.Zepto as Zepto

data Pair a = MkPair
  { name :: String.String,
    value :: a
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser a -> Zepto.Parser (Pair a)
decode p =
  MkPair
    <$> (String.decode <* Array.decodeBlanks)
    <*> (Zepto.char ':' *> Array.decodeBlanks *> p)

encode :: (a -> Builder.Builder) -> Pair a -> Builder.Builder
encode b p = String.encode (name p) <> Builder.charUtf8 ':' <> b (value p)
