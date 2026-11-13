module Parun.Exceptions.UnknownOption where

import qualified Control.Exception as Exception

-- | A command line named an option the command does not take.
newtype UnknownOption = MkUnknownOption
  { option :: String
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception UnknownOption where
  displayException e = "unknown option: " <> option e
