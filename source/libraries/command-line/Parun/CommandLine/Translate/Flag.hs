-- | `parun translate`'s command line, loosely parsed: each flag with its
-- argument as given, read by 'Parun.CommandLine.Translate.Config.fromFlags'.
module Parun.CommandLine.Translate.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = Help Bool
  | Out FilePath
  | Set String
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["out"] (GetOpt.ReqArg Out "DIR") "write every card accepted to this directory, the pool's included; into the pool, never overwriting, when absent",
    GetOpt.Option [] ["set"] (GetOpt.ReqArg Set "CODE") "translate every card CANDIDATES says this set printed, beside those named"
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun translate [OPTION...] MTGISH CANDIDATES [--] [NAME | @FILE...]\nA name beginning with - goes after the --." flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
