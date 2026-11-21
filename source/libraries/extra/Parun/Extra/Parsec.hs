module Parun.Extra.Parsec where

import qualified Data.Text as Text
import qualified Parun.Extra.Either as Either
import qualified Text.Parsec as Parsec

-- | Attempts to parse the string using the given parser.
parseString :: Parsec.Parsec String () a -> String -> Maybe a
parseString p = Either.hush . Parsec.parse p ""

-- | Attempts to parse the string, as text, using the given parser.
parseText :: Parsec.Parsec Text.Text () a -> String -> Maybe a
parseText p = Either.hush . Parsec.parse p "" . Text.pack
