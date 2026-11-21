module Parun.Json.Boolean where

import qualified Control.Applicative as Applicative
import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Parun.Zepto as Zepto

newtype Boolean = MkBoolean
  { unwrap :: Bool
  }
  deriving (Eq, Ord, Show)

decode :: Zepto.Parser Boolean
decode =
  MkBoolean
    <$> Applicative.asum
      [ False <$ Zepto.string (Text.pack "false"),
        True <$ Zepto.string (Text.pack "true")
      ]

encode :: Boolean -> Builder.Builder
encode = Builder.stringUtf8 . Bool.bool "false" "true" . unwrap
