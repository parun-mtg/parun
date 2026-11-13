module Parun.Exceptions.InvalidOption where

import qualified Control.Exception as Exception

-- | A command line gave an option it takes a bad argument, or none where one
-- is required: why, as a sentence.
newtype InvalidOption = MkInvalidOption
  { problem :: String
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception InvalidOption where
  displayException = problem
