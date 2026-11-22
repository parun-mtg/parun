-- | A subcommand as tab completion sees it.
module Parun.CommandLine.Completion.Command where

import qualified Control.Monad as Monad
import qualified Parun.CommandLine.Completion.Value as Value
import qualified System.Console.GetOpt as GetOpt

-- | The subcommand's name, its options, the value each option taking an
-- argument completes to by any of its long names ('Value.MkOpaque' when
-- unnamed), the values of its positional arguments in order, and the value
-- of every one after those, Nothing when it takes no more.
data Command = MkCommand
  { name :: String,
    options :: [GetOpt.OptDescr ()],
    values :: [(String, Value.Value)],
    positionals :: [Value.Value],
    rest :: Maybe Value.Value
  }

-- | The command of this name taking these options and no argument but those
-- named.
new :: String -> [GetOpt.OptDescr f] -> Command
new n os = MkCommand {name = n, options = fmap Monad.void os, values = [], positionals = [], rest = Nothing}
