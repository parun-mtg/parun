-- | `parun play`'s command line, loosely parsed: each flag with its argument
-- as given, checked only as 'Parun.Play.Command.Config.fromFlags' reads it.
module Parun.Play.Command.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified Parun.Types.Limits as Limits
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = ArrangeGraveyard String
  | Bot String
  | First String
  | Format String
  | Help Bool
  | Json Bool
  | Level String
  | Meta FilePath
  | ObjectLimit String
  | Persona String
  | Pool String
  | Save FilePath
  | Seed String
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["seed"] (GetOpt.ReqArg Seed "KEY") "the generator's key, 64 hexadecimal digits, said when the game ends; drawn from the operating system when absent",
    GetOpt.Option [] ["first"] (GetOpt.ReqArg First "SEAT") "which seat plays first; a seat picked at random chooses when absent",
    GetOpt.Option [] ["bot"] (GetOpt.ReqArg Bot "SEAT") "a seat the search bot plays",
    GetOpt.Option [] ["json"] (GetOpt.NoArg $ Json True) "speak the line protocol on standard input and output",
    GetOpt.Option [] ["no-json"] (GetOpt.NoArg $ Json False) "",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg Save "FILE") "save the game as a scenario, even one cut short",
    GetOpt.Option [] ["arrange-graveyard"] (GetOpt.ReqArg ArrangeGraveyard "SEAT") "a seat asked to order cards put into its graveyard at the same time",
    GetOpt.Option [] ["format"] (GetOpt.ReqArg Format "FORMAT") "play under this format, refusing a deck not legal in it: constructed, limited, or one with a pool, such as vintage",
    GetOpt.Option [] ["level"] (GetOpt.ReqArg Level "[SEAT=]LEVEL") "how strongly the search bot plays, easy, medium or hard (Parun.Play.Opponent), in every seat it plays or in SEAT alone; hard when absent",
    GetOpt.Option [] ["persona"] (GetOpt.ReqArg Persona "[SEAT=]PERSONA") "what the search bot values in a position, balanced, aggro or control (Parun.Play.Opponent), in every seat it plays or in SEAT alone; balanced when absent",
    GetOpt.Option [] ["meta"] (GetOpt.ReqArg Meta "META") "the search bot infers each opponent's deck list from this meta file's decks",
    GetOpt.Option [] ["pool"] (GetOpt.ReqArg Pool "FORMAT") "no meta named, the search bot deals each opponent's unseen cards from this format's pool, from every card where it has none; the game's format's when absent",
    GetOpt.Option [] ["object-limit"] (GetOpt.ReqArg ObjectLimit "N") ("the most objects the game may hold before a player with priority may only pass; " <> show (Limits.objects Limits.standard) <> " when absent")
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun play [OPTION...] DECK DECK [DECK...]" flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
