-- | What `parun test`'s command line asks for, each flag's argument checked.
module Parun.Test.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Char as Char
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Exceptions.InvalidOption as InvalidOption
import qualified Parun.Play.Key as Key
import qualified Parun.Test.Flag as Flag
import qualified Parun.Types.Key as Key.Type
import qualified Text.Read as Read

-- | The substrings selecting cases (any of them; every case when none), the
-- per-case budget in microseconds, the shuffle's seed, where to write the
-- run's report, whether to list the selected cases instead of running them,
-- whether to stop at the first failure, and whether to print the usage
-- instead.
data Config = MkConfig
  { patterns :: [Text.Text],
    timeout :: Maybe Natural,
    seed :: Maybe Key.Type.Key,
    json :: Maybe FilePath,
    list :: Bool,
    failFast :: Bool,
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags.
initial :: Config
initial = MkConfig {patterns = [], timeout = Nothing, seed = Nothing, json = Nothing, list = False, failFast = False, help = False}

-- | The config the flags ask for, each in turn over 'initial'; throws at the
-- first whose argument is no good.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.FailFast b -> pure config {failFast = b}
  Flag.Help b -> pure config {help = b}
  Flag.Json file -> pure config {json = Just file}
  Flag.List b -> pure config {list = b}
  Flag.Pattern p -> pure config {patterns = patterns config <> [Text.pack p]}
  Flag.Seed k -> maybe (invalid $ "not a seed: " <> k) (\x -> pure config {seed = Just x}) . Key.fromHex $ Text.pack k
  Flag.Timeout d -> maybe (invalid $ "not a duration: " <> d) (\n -> pure config {timeout = Just n}) $ duration d
  where
    invalid problem = Catch.throwM $ InvalidOption.MkInvalidOption problem

-- | A duration in microseconds: a count of seconds, or of the unit it ends in.
duration :: String -> Maybe Natural
duration s = do
  let (digits, unit) = span Char.isDigit s
  n <- Read.readMaybe digits
  scale <- case unit of
    "" -> Just 1000000
    "ms" -> Just 1000
    "s" -> Just 1000000
    "m" -> Just 60000000
    "h" -> Just 3600000000
    _ -> Nothing
  pure $ n * scale
