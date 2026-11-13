module Parun.Exceptions.UnexpectedArgument where

import qualified Control.Exception as Exception

-- | A command line held an argument where it takes none.
newtype UnexpectedArgument = MkUnexpectedArgument
  { argument :: String
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception UnexpectedArgument where
  displayException e = "unexpected argument: " <> argument e
