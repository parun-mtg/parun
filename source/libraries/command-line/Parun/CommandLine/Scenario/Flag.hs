-- | `parun scenario`'s command line, loosely parsed: each flag as given, read
-- by 'Parun.CommandLine.Scenario.Config.fromFlags'.
module Parun.CommandLine.Scenario.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line.
data Flag
  = Help Bool
  | Perturb Bool
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["perturb"] (GetOpt.NoArg $ Perturb True) "rerun each scenario that ran clean with one check's expected value perturbed at a time, failing it for every perturbation it still passes",
    GetOpt.Option [] ["no-perturb"] (GetOpt.NoArg $ Perturb False) ""
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun scenario [OPTION...] FILE..." flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
