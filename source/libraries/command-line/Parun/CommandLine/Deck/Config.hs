-- | What `parun deck`'s command line asks for, each flag's argument checked.
module Parun.CommandLine.Deck.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Deck.Flag as Flag

-- | The format to check the deck against, the file holding its card pool,
-- the deck list's file (absent only when the usage is asked for), and
-- whether to print the usage instead.
data Config = MkConfig
  { format :: Maybe Text.Text,
    pool :: Maybe FilePath,
    file :: Maybe FilePath,
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no file.
initial :: Config
initial = MkConfig {format = Nothing, pool = Nothing, file = Nothing, help = False}

-- | The config the flags ask for, each in turn over 'initial', reading the
-- deck list's file from the positional arguments; throws at the first flag
-- whose argument is no good, a missing file or an extra argument.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> [String] -> m Config
fromFlags fs positional = do
  config <- Monad.foldM applyFlag initial fs
  Monad.unless (help config) $ CommandLine.positionals ["FILE"] (Just 1) positional
  pure config {file = Maybe.listToMaybe positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Format f -> pure config {format = Just $ Text.pack f}
  Flag.Help b -> pure config {help = b}
  Flag.Pool p -> pure config {pool = Just p}
