module Parun.Json.Object where

import qualified Data.ByteString.Builder as Builder
import qualified Parun.Extra.Monoid as Monoid
import qualified Parun.Extra.Semigroup as Semigroup
import qualified Parun.Json.Array as Array
import qualified Parun.Json.Pair as Pair
import qualified Parun.Zepto as Zepto

newtype Object a = MkObject
  { unwrap :: [Pair.Pair a]
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser a -> Zepto.Parser (Object a)
decode p =
  MkObject <$> (Zepto.char '{' *> Array.decodeBlanks *> Array.decodeElements '}' (Pair.decode p))

encode :: (a -> Builder.Builder) -> Object a -> Builder.Builder
encode b =
  Semigroup.around (Builder.charUtf8 '{') (Builder.charUtf8 '}')
    . Monoid.sepBy (Builder.charUtf8 ',')
    . fmap (Pair.encode b)
    . unwrap
