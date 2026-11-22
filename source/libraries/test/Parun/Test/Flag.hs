-- | `parun test`'s command line, loosely parsed: each flag with its argument
-- as given, checked only as 'Parun.Test.Config.fromFlags' reads it.
module Parun.Test.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = FailFast Bool
  | Help Bool
  | Json FilePath
  | List Bool
  | Pattern String
  | Seed String
  | Timeout String
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option "p" ["pattern"] (GetOpt.ReqArg Pattern "SUBSTRING") "run the cases whose path holds this; repeated, those matching any",
    GetOpt.Option [] ["timeout"] (GetOpt.ReqArg Timeout "DURATION") "fail a case still running after this long: seconds, or a number ending in ms, s, m or h",
    GetOpt.Option [] ["seed"] (GetOpt.ReqArg Seed "KEY") "the shuffle's key, 64 hexadecimal digits; drawn from the operating system when absent",
    GetOpt.Option [] ["json"] (GetOpt.ReqArg Json "FILE") "write every case's name, failure, its site and nanoseconds to this file as JSON",
    GetOpt.Option [] ["list"] (GetOpt.NoArg $ List True) "print the names of the cases a run would select, in the order they are registered, without running them",
    GetOpt.Option [] ["no-list"] (GetOpt.NoArg $ List False) "",
    GetOpt.Option [] ["fail-fast"] (GetOpt.NoArg $ FailFast True) "start no case after the first failure; those already running finish",
    GetOpt.Option [] ["no-fail-fast"] (GetOpt.NoArg $ FailFast False) ""
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun test [OPTION...]" flags

-- | The flags on a command line; throws at its first argument, unknown option
-- or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  (fs, positional) <- CommandLine.options flags arguments
  fs <$ CommandLine.positionals [] (Just 0) positional
