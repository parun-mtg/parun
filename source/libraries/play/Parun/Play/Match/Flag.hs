-- | `parun match`'s command line, loosely parsed: each flag with its
-- argument as given, checked only as 'Parun.Play.Match.Config.fromFlags'
-- reads it.
module Parun.Play.Match.Flag where

import qualified Control.Monad.Catch as Catch
import qualified Parun.CommandLine as CommandLine
import qualified System.Console.GetOpt as GetOpt

-- | One flag on the command line, its argument as given.
data Flag
  = Candidates String
  | Cutoff String
  | Games String
  | Help Bool
  | Level String
  | Meta FilePath
  | Method String
  | Mistakes String
  | Persona String
  | Playout String
  | Pool String
  | Positions FilePath
  | Samples String
  | Save FilePath
  | Search String
  | Seed String
  | Turns String
  | Weights String
  deriving (Eq, Show)

flags :: [GetOpt.OptDescr Flag]
flags =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["seed"] (GetOpt.ReqArg Seed "KEY") "the key each game's is derived from, 64 hexadecimal digits, said when the match ends; drawn from the operating system when absent",
    GetOpt.Option [] ["games"] (GetOpt.ReqArg Games "N") "how many games to play; 100 when absent",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg Save "DIR") "save each game in this directory as a scenario",
    GetOpt.Option [] ["positions"] (GetOpt.ReqArg Positions "FILE") "write the position of each decision at priority in a game a seat won to this file, for parun fit",
    GetOpt.Option [] ["search"] (GetOpt.ReqArg Search "SEAT") "the search bot plays this seat, 1 or 2; the random bot plays every other",
    GetOpt.Option [] ["samples"] (GetOpt.ReqArg Samples "[SEAT=]N") "the search bot's playouts of each option, in every seat or in SEAT alone; 2 when absent",
    GetOpt.Option [] ["turns"] (GetOpt.ReqArg Turns "[SEAT=]N") "the turns a playout may begin after the decision's before it is cut off and its position scored, in every seat or in SEAT alone; 0 when absent, ending a playout as the next turn begins",
    GetOpt.Option [] ["cutoff"] (GetOpt.ReqArg Cutoff "[SEAT=]N") "the prompts a playout is answered before it is cut off and its position scored, in every seat or in SEAT alone; 300 when absent",
    GetOpt.Option [] ["playout"] (GetOpt.ReqArg Playout "[SEAT=]POLICY") "who answers every seat in the search bot's playouts, random (the random bot), greedy (playing a land, else casting a permanent spell, where it can), polar (greedy, choosing targets by whether their effect harms or helps them) or classified (polar, choosing modes and payments that way too), in every seat or in SEAT alone; random when absent",
    GetOpt.Option [] ["method"] (GetOpt.ReqArg Method "[SEAT=]METHOD") "how the search bot spends its playouts, flat (each option played out alike) or tree (one information-set tree over every seat's decisions, the playouts of every option shared by it), in every seat or in SEAT alone; flat when absent",
    GetOpt.Option [] ["candidates"] (GetOpt.ReqArg Candidates "[SEAT=]N") "the worlds the search bot deals once at a decision and weighs by how likely each made the plays its opponents made at priority, every playout's world drawn among them, in every seat or in SEAT alone; 0 when absent, each playout's world dealt fresh",
    GetOpt.Option [] ["mistakes"] (GetOpt.ReqArg Mistakes "[SEAT=]PERCENT") "the percent of the search bot's decisions at which it takes the option scored next below the best, in every seat or in SEAT alone; 0 when absent",
    GetOpt.Option [] ["level"] (GetOpt.ReqArg Level "[SEAT=]LEVEL") "how strongly the search bot plays, easy, medium or hard, setting its samples and mistakes (Parun.Play.Opponent), in every seat or in SEAT alone; hard when absent",
    GetOpt.Option [] ["weights"] (GetOpt.ReqArg Weights "[SEAT=]FILE") "the file of the weights the search bot scores a cut-off playout's position by (Parun.Codec.Weights), in every seat or in SEAT alone; every feature weighted 1 on each side when absent",
    GetOpt.Option [] ["persona"] (GetOpt.ReqArg Persona "[SEAT=]PERSONA") "the weights the search bot scores a cut-off playout's position by named, balanced, aggro or control (Parun.Play.Opponent), in every seat or in SEAT alone; balanced when absent",
    GetOpt.Option [] ["meta"] (GetOpt.ReqArg Meta "META") "the deck lists are not known: the search bot infers each opponent's from this meta file's decks; each list known when absent",
    GetOpt.Option [] ["pool"] (GetOpt.ReqArg Pool "FORMAT") "the deck lists are not known and no meta is named: the search bot deals each opponent's unseen cards from this format's pool, from every card where it has none"
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun match [OPTION...] DECK DECK" flags

-- | The flags on a command line, and its positional arguments; throws at its
-- first unknown option or option missing its argument.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [String])
fromArguments = CommandLine.options flags
