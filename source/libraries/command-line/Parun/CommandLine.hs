-- | What every subcommand's command line shares: reading its options,
-- checking its positional arguments, and refusing a bad one.
module Parun.CommandLine where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Char as Char
import qualified Data.List as List
import Numeric.Natural (Natural)
import qualified Parun.Exceptions.InvalidOption as InvalidOption
import qualified Parun.Exceptions.MissingArgument as MissingArgument
import qualified Parun.Exceptions.ShowHelp as ShowHelp
import qualified Parun.Exceptions.UnexpectedArgument as UnexpectedArgument
import qualified Parun.Exceptions.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | The options on a command line, in order, and its positional arguments;
-- throws at its first unknown option or option missing its argument.
options :: (Catch.MonadThrow m) => [GetOpt.OptDescr f] -> [String] -> m ([f], [String])
options descriptions arguments = do
  let (fs, positional, unknown, problems) = GetOpt.getOpt' GetOpt.Permute descriptions arguments
  mapM_ (Catch.throwM . UnknownOption.MkUnknownOption) unknown
  mapM_ (Catch.throwM . InvalidOption.MkInvalidOption . List.dropWhileEnd Char.isSpace) problems
  pure (fs, positional)

-- | Checks positional arguments against the names of those required, in
-- order, and the most allowed, any number when 'Nothing': throws at the
-- first required one missing, else at the first extra one.
positionals :: (Catch.MonadThrow m) => [String] -> Maybe Natural -> [String] -> m ()
positionals required most given = do
  mapM_ (Catch.throwM . MissingArgument.MkMissingArgument) . take 1 $ List.genericDrop (List.genericLength given :: Natural) required
  mapM_ (Catch.throwM . UnexpectedArgument.MkUnexpectedArgument) . take 1 $ foldMap (`List.genericDrop` given) most

-- | The value an option's argument parses as, or 'InvalidOption.InvalidOption'
-- saying `problem` where it parses as none.
orInvalid :: (Catch.MonadThrow m) => String -> Maybe a -> m a
orInvalid problem = maybe (Catch.throwM $ InvalidOption.MkInvalidOption problem) pure

-- | The config a command line asks for, or the exit 'refusals' makes of it
-- under this usage: its parse, and 'ShowHelp.ShowHelp' when the config asks
-- for the usage instead.
configured :: String -> (c -> Bool) -> IO c -> IO c
configured usage help parse = Exception.catches (parse >>= \c -> c <$ Monad.when (help c) (Exception.throwIO ShowHelp.MkShowHelp)) (refusals usage)

-- | What a command line that asks for no run comes to: the usage on standard
-- output for 'ShowHelp.ShowHelp', and otherwise the problem and the usage on
-- standard error and a failed exit.
refusals :: String -> [Exception.Handler a]
refusals usage =
  [ Exception.Handler $ \ShowHelp.MkShowHelp -> putStr usage >> Exit.exitSuccess,
    Exception.Handler $ \e -> refuse (e :: MissingArgument.MissingArgument),
    Exception.Handler $ \e -> refuse (e :: UnexpectedArgument.UnexpectedArgument),
    Exception.Handler $ \e -> refuse (e :: UnknownOption.UnknownOption),
    Exception.Handler $ \e -> refuse (e :: InvalidOption.InvalidOption)
  ]
  where
    refuse :: (Exception.Exception e) => e -> IO a
    refuse e = do
      IO.hPutStrLn IO.stderr $ Exception.displayException e
      IO.hPutStr IO.stderr usage
      Exit.exitFailure
