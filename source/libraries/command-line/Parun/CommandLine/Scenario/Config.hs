-- | What `parun scenario`'s command line asks for.
module Parun.CommandLine.Scenario.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.CommandLine.Scenario.Flag as Flag

-- | Whether to perturb each scenario that runs clean, the scenario files, in
-- order (none only when the usage is asked for), and whether to print the
-- usage instead.
data Config = MkConfig
  { perturb :: Bool,
    files :: [FilePath],
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no file.
initial :: Config
initial = MkConfig {perturb = False, files = [], help = False}

-- | The config the flags ask for, each in turn over 'initial', reading the
-- files from the positional arguments; throws when there is none.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> [String] -> m Config
fromFlags fs positional = do
  config <- Monad.foldM applyFlag initial fs
  Monad.unless (help config) $ CommandLine.positionals ["FILE"] Nothing positional
  pure config {files = positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
  Flag.Perturb b -> pure config {perturb = b}

-- | `parun scenario` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "scenario" Flag.flags) {Command.rest = Just Value.MkFiles}
