module Parun.Json.Null where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Parun.Zepto as Zepto

newtype Null = MkNull
  { unwrap :: ()
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser Null
decode = MkNull () <$ Zepto.string (Text.pack "null")

encode :: Null -> Builder.Builder
encode = const $ Builder.stringUtf8 "null"
