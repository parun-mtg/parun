-- | The command line of a command that takes no option but the usage's, such
-- as `parun oracle`, loosely parsed.
module Parun.CommandLine.Bare.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line.
newtype Flag
  = Help Bool
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) ""
  ]

-- | The usage of the command of this name taking these positional arguments.
usage :: String -> String -> String
usage name positional = GetOpt.usageInfo (unwords $ ["usage: parun", name, "[OPTION...]"] <> filter (not . null) [positional]) flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
