-- | `parun bench`'s command line, loosely parsed: each flag with its argument
-- as given, checked only as 'Parun.Benchmark.Config.fromFlags' reads it.
module Parun.Benchmark.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = Help Bool
  | Json FilePath
  | Pattern String
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option "p" ["pattern"] (GetOpt.ReqArg Pattern "SUBSTRING") "run the benchmarks whose name holds this; repeated, those matching any",
    GetOpt.Option [] ["json"] (GetOpt.ReqArg Json "FILE") "write every benchmark's result to this file as JSON"
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun bench [OPTION...]" flags

-- | The flags on a command line; throws at its first argument, unknown option
-- or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  (fs, positional) <- CommandLine.options flags arguments
  fs <$ CommandLine.positionals [] (Just 0) positional
