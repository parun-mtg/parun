module Parun.JsonPointer.Pointer where

import qualified Control.Applicative as Applicative
import qualified Data.ByteString.Builder as Builder
import qualified Parun.JsonPointer.Token as Token
import qualified Parun.Zepto as Zepto

-- | A JSON Pointer as defined by RFC 6901. A JSON Pointer is a sequence of
-- zero or more reference tokens. An empty list represents the root of the
-- document.
newtype Pointer = MkPointer
  { unwrap :: [Token.Token]
  }
  deriving (Eq, Ord, Show)

-- | Decodes a JSON Pointer from a string. Per RFC 6901, a JSON Pointer is
-- either an empty string (root) or a sequence of reference tokens each
-- prefixed by @/@.
decode :: Zepto.Parser Pointer
decode = MkPointer <$> Applicative.many (Zepto.char '/' *> Token.decode)

-- | Encodes a JSON Pointer to a string. Each token is prefixed with @/@.
encode :: Pointer -> Builder.Builder
encode = foldMap encodeToken . unwrap

encodeToken :: Token.Token -> Builder.Builder
encodeToken t = Builder.charUtf8 '/' <> Token.encode t
