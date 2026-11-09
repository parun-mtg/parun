module Parun.Exceptions.EntropyUnavailable where

import qualified Control.Exception as Exception
import qualified Data.Text as Text

-- | The operating system gave no random bytes to seed a game with.
newtype EntropyUnavailable = MkEntropyUnavailable
  { reason :: Text.Text
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception EntropyUnavailable
