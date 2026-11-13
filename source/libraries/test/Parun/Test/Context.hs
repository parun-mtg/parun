-- | What a `parun test` run works with: its config, and what the config
-- leaves to the run's surroundings.
module Parun.Test.Context where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Parun.Exceptions.ShowHelp as ShowHelp
import qualified Parun.Play.Key as Key
import qualified Parun.Test.Config as Config
import qualified Parun.Test.Display as Display
import qualified Parun.Types.Key as Key.Type
import qualified System.Environment as Environment
import qualified System.IO as IO

-- | The config, the shuffle's key, drawn when the config names none, and how
-- the run shows its progress.
data Context = MkContext
  { config :: Config.Config,
    key :: Key.Type.Key,
    display :: Display.Display
  }
  deriving (Eq, Show)

-- | The context a run under this config works in: its seed's key, else a fresh
-- one; a progress line when standard output is a terminal; and annotations
-- under GitHub Actions. Throws 'ShowHelp.ShowHelp' when the config asks for
-- the usage instead.
fromConfig :: Config.Config -> IO Context
fromConfig c = do
  Monad.when (Config.help c) $ Exception.throwIO ShowHelp.MkShowHelp
  k <- maybe Key.fresh pure $ Config.seed c
  terminal <- IO.hIsTerminalDevice IO.stdout
  actions <- Environment.lookupEnv "GITHUB_ACTIONS"
  pure MkContext {config = c, key = k, display = Display.MkDisplay {Display.live = terminal, Display.annotated = actions == Just "true"}}
