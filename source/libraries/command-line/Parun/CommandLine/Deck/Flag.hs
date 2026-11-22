-- | `parun deck`'s command line, loosely parsed: each flag with its argument
-- as given, checked only as 'Parun.CommandLine.Deck.Config.fromFlags' reads
-- it.
module Parun.CommandLine.Deck.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = Format String
  | Help Bool
  | Pool FilePath
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["format"] (GetOpt.ReqArg Format "FORMAT") "check the deck against this format's rules and pool; the list's header's when absent",
    GetOpt.Option [] ["pool"] (GetOpt.ReqArg Pool "FILE") "the deck list of the card pool a limited deck is built from"
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun deck [OPTION...] FILE" flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
