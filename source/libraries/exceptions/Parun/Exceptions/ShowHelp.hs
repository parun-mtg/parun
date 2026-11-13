module Parun.Exceptions.ShowHelp where

import qualified Control.Exception as Exception

-- | A command line asked for the command's usage instead of a run of it.
data ShowHelp = MkShowHelp
  deriving (Eq, Ord, Show)

instance Exception.Exception ShowHelp where
  displayException _ = "help requested"
