-- | What `parun bench`'s command line asks for.
module Parun.Benchmark.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Parun.Benchmark.Flag as Flag
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Value as Value

-- | The substrings selecting benchmarks (any of them; every benchmark when
-- none), where to write the results, and whether to print the usage
-- instead.
data Config = MkConfig
  { patterns :: [Text.Text],
    json :: Maybe FilePath,
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags.
initial :: Config
initial = MkConfig {patterns = [], json = Nothing, help = False}

-- | The config the flags ask for, each in turn over 'initial'.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
  Flag.Json file -> pure config {json = Just file}
  Flag.Pattern p -> pure config {patterns = patterns config <> [Text.pack p]}

-- | `parun bench` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "bench" Flag.flags) {Command.values = [("json", Value.MkFiles)]}
