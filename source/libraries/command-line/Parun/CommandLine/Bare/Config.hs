-- | What the command line of a command taking no option but the usage's asks
-- for.
module Parun.CommandLine.Bare.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import Numeric.Natural (Natural)
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Bare.Flag as Flag

-- | The positional arguments, in order, and whether to print the usage
-- instead.
data Config = MkConfig
  { arguments :: [String],
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no argument.
initial :: Config
initial = MkConfig {arguments = [], help = False}

-- | The config the flags ask for, each in turn over 'initial', with the
-- positional arguments checked as 'CommandLine.positionals' checks them
-- against the names required and the most allowed.
fromFlags :: (Catch.MonadThrow m) => [String] -> Maybe Natural -> [Flag.Flag] -> [String] -> m Config
fromFlags required most fs positional = do
  config <- Monad.foldM applyFlag initial fs
  Monad.unless (help config) $ CommandLine.positionals required most positional
  pure config {arguments = positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
