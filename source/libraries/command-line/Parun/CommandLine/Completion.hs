-- | Tab completion of a `parun` command line, answered by the binary itself
-- so it cannot drift from the commands it completes. A shell's shim
-- (Parun.CommandLine.Completion.Shim) asks and the binary answers, on one wire
-- for every shell:
--
-- * The shim runs `parun __complete INDEX WORD...`: the words typed after the
--   program's name, and the index, from 0, of the one the cursor is in. A word
--   absent at INDEX is the empty word.
-- * The answer is one candidate a line, each a whole word beginning with the
--   word at INDEX; the shell narrows or replaces them as it does. No
--   candidate begins with `:`.
-- * Or it is the one line `:files` and a prefix of the word at INDEX: the
--   shell completes a file name after the prefix and keeps the prefix in
--   front of it.
module Parun.CommandLine.Completion where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Reply as Reply
import qualified Parun.CommandLine.Completion.Value as Value
import qualified System.Console.GetOpt as GetOpt

-- | The reply for the word at this index of these, among these commands,
-- these the shipped formats: a command's name, then its options, their
-- arguments and its positional arguments as GetOpt reads them.
complete :: [String] -> [Command.Command] -> Natural -> [String] -> Reply.Reply
complete formats commands index typed =
  let current = Maybe.fromMaybe "" (Maybe.listToMaybe (List.genericDrop index typed))
   in case List.genericTake index typed of
        [] -> candidates current (fmap Command.name commands)
        named : before -> case List.find ((== named) . Command.name) commands of
          Nothing -> Reply.MkCandidates []
          Just command -> within formats command before current

-- The reply for the word at the cursor after these words of the command's,
-- read as GetOpt reads them: an option's argument, a positional argument,
-- or after `--` only positional arguments.
within :: [String] -> Command.Command -> [String] -> String -> Reply.Reply
within formats command = go 0
  where
    go :: Natural -> [String] -> String -> Reply.Reply
    go given before current = case before of
      [] -> atCursor given current
      "--" : after -> positional False (given + List.genericLength after) current
      word : after -> case awaiting word of
        Nothing -> go (given + 1) after current
        Just Nothing -> go given after current
        Just (Just value) -> case after of
          [] -> answer formats value "" current
          _ : more -> go given more current
    atCursor given current
      | Just long <- List.stripPrefix "--" current,
        (named, '=' : argument) <- break (== '=') long =
          maybe (Reply.MkCandidates []) (\option -> answer formats (valueOf option) ("--" <> named <> "=") argument) (longOption named)
      | List.isPrefixOf "-" current = candidates current optionWords
      | otherwise = positional True given current
    -- The value of the positional argument after this many, offering the
    -- options in place of one the command does not take unless after `--`.
    positional optional given current = case List.genericDrop given (Command.positionals command) of
      value : _ -> answer formats value "" current
      [] -> case Command.rest command of
        Just value -> answer formats value "" current
        Nothing
          | optional -> candidates current optionWords
          | otherwise -> Reply.MkCandidates []
    -- Whether a word is an option: Nothing when not, else the value of the
    -- argument it awaits in the next word, if any.
    awaiting word = case word of
      '-' : '-' : long
        | '=' `elem` long -> Just Nothing
        | otherwise -> Just (fmap valueOf (Monad.mfilter requires (longOption long)))
      '-' : shorts@(_ : _) -> Just (clustered shorts)
      _ -> Nothing
    -- A cluster of short options awaits an argument only when its last
    -- takes one; one before the last takes the rest of the cluster.
    clustered shorts = case shorts of
      [] -> Nothing
      c : more -> case List.find (\(GetOpt.Option cs _ _ _) -> elem c cs) (Command.options command) of
        Just option | requires option -> if null more then Just (valueOf option) else Nothing
        _ -> clustered more
    requires (GetOpt.Option _ _ argument _) = case argument of
      GetOpt.ReqArg _ _ -> True
      _ -> False
    -- The option a long name means, as GetOpt reads one: the name exactly,
    -- else the one option it begins the name of.
    longOption named = case [option | option@(GetOpt.Option _ ls _ _) <- Command.options command, elem named ls] of
      option : _ -> Just option
      [] -> case [option | option@(GetOpt.Option _ ls _ _) <- Command.options command, any (List.isPrefixOf named) ls] of
        [option] -> Just option
        _ -> Nothing
    valueOf (GetOpt.Option _ ls _ _) = Maybe.fromMaybe Value.MkOpaque (Maybe.listToMaybe (Maybe.mapMaybe (`lookup` Command.values command) ls))
    optionWords = [w | GetOpt.Option cs ls _ _ <- Command.options command, w <- fmap (\c -> ['-', c]) cs <> fmap ("--" <>) ls]

-- | The reply for a word, after this prefix of it, completing to a value,
-- these the shipped formats.
answer :: [String] -> Value.Value -> String -> String -> Reply.Reply
answer formats v prefix word = case v of
  Value.MkFiles -> Reply.MkFiles prefix
  Value.MkChoices choices -> Reply.MkCandidates [prefix <> choice | choice <- choices, List.isPrefixOf word choice]
  Value.MkFormats -> answer formats (Value.MkChoices formats) prefix word
  Value.MkSeated seats inner -> case break (== '=') word of
    (seat, '=' : after) | elem seat seats -> answer formats inner (prefix <> seat <> "=") after
    _ -> answer formats inner prefix word
  Value.MkPrefixed start inner -> maybe (Reply.MkCandidates []) (answer formats inner (prefix <> start)) (List.stripPrefix start word)
  Value.MkOpaque -> Reply.MkCandidates []

-- The words beginning with this one.
candidates :: String -> [String] -> Reply.Reply
candidates word = Reply.MkCandidates . filter (List.isPrefixOf word)
