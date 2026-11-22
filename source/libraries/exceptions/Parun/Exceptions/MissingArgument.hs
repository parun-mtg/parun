module Parun.Exceptions.MissingArgument where

import qualified Control.Exception as Exception

-- | A command line lacked a positional argument the command requires: its
-- name in the usage.
newtype MissingArgument = MkMissingArgument
  { argument :: String
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception MissingArgument where
  displayException e = "missing argument: " <> argument e
