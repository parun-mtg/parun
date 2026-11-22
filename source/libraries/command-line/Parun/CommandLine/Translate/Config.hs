-- | What `parun translate`'s command line asks for.
module Parun.CommandLine.Translate.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.CommandLine.Translate.Flag as Flag

-- | The mtgish AST file and the candidates file (absent only when the usage
-- is asked for), where to write the cards accepted, the set whose cards to
-- translate, the names and @FILEs naming the others, and whether to print
-- the usage instead.
data Config = MkConfig
  { sources :: Maybe (FilePath, FilePath),
    out :: Maybe FilePath,
    set :: Maybe Text.Text,
    names :: [Text.Text],
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no argument.
initial :: Config
initial = MkConfig {sources = Nothing, out = Nothing, set = Nothing, names = [], help = False}

-- | The config the flags ask for, each in turn over 'initial', reading the
-- two files and then the names from the positional arguments; throws when
-- a file is missing.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> [String] -> m Config
fromFlags fs positional = do
  config <- Monad.foldM applyFlag initial fs
  Monad.unless (help config) $ CommandLine.positionals ["MTGISH", "CANDIDATES"] Nothing positional
  pure $ case positional of
    mtgish : candidates : rest -> config {sources = Just (mtgish, candidates), names = fmap Text.pack rest}
    _ -> config

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help b -> pure config {help = b}
  Flag.Out d -> pure config {out = Just d}
  Flag.Set c -> pure config {set = Just $ Text.pack c}

-- | `parun translate` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "translate" Flag.flags) {Command.values = [("out", Value.MkFiles)], Command.positionals = [Value.MkFiles, Value.MkFiles], Command.rest = Just (Value.MkPrefixed "@" Value.MkFiles)}
