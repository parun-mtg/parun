-- | What `parun play`'s command line asks for, each flag's argument checked.
module Parun.Play.Command.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.Exceptions.InvalidOption as InvalidOption
import qualified Parun.Play.Command.Flag as Flag
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Text.Read as Read

-- | What the game is played under, the deck lists' files, a seat each (none
-- only when the usage is asked for), and whether to print the usage
-- instead.
data Config = MkConfig
  { options :: PlayOptions.PlayOptions,
    decks :: [FilePath],
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no deck.
initial :: Config
initial = MkConfig {options = PlayOptions.none, decks = [], help = False}

-- | The config the flags ask for, each in turn over 'initial', reading the
-- decks from the positional arguments; throws at the first flag whose
-- argument is no good, fewer than two decks, a seat past the last, or a
-- seat nobody plays.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> [String] -> m Config
fromFlags fs positional = do
  config <- Monad.foldM applyFlag initial fs
  let o = options config
      count = List.genericLength positional :: Natural
  Monad.unless (help config) $ do
    CommandLine.positionals ["DECK", "DECK"] Nothing positional
    Monad.forM_ (List.find (> count) (Maybe.maybeToList (PlayOptions.first o) <> Set.toList (PlayOptions.bots o <> PlayOptions.arrangesGraveyard o))) $ \n ->
      Catch.throwM (InvalidOption.MkInvalidOption ("not a seat: " <> show n))
    Monad.forM_ (unplayed o count) $ \n ->
      Catch.throwM (InvalidOption.MkInvalidOption ("seat " <> show n <> " has no player: name it with --bot, or speak the line protocol with --json"))
  pure config {decks = positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.ArrangeGraveyard s -> seat s >>= \n -> set o {PlayOptions.arrangesGraveyard = Set.insert n (PlayOptions.arrangesGraveyard o)}
  Flag.Bot s -> seat s >>= \n -> set o {PlayOptions.bots = Set.insert n (PlayOptions.bots o)}
  Flag.First s -> seat s >>= \n -> set o {PlayOptions.first = Just n}
  Flag.Format s -> set o {PlayOptions.format = Just (Text.pack s)}
  Flag.Help b -> pure config {help = b}
  Flag.Json b -> set o {PlayOptions.json = b}
  Flag.Level s -> CommandLine.orInvalid ("not a level, easy, medium or hard: " <> s) (Opponent.named Opponent.difficultyName s) >>= \d -> set o {PlayOptions.budget = Opponent.budgetAt d (PlayOptions.budget o)}
  Flag.Meta s -> set o {PlayOptions.meta = Just s}
  Flag.ObjectLimit s -> CommandLine.orInvalid ("not a number of objects: " <> s) (Read.readMaybe s) >>= \n -> set o {PlayOptions.limits = (PlayOptions.limits o) {Limits.objects = n}}
  Flag.Persona s -> CommandLine.orInvalid ("not a persona, balanced, aggro or control: " <> s) (Opponent.named Opponent.personaName s) >>= \p -> set o {PlayOptions.weights = Opponent.weightsOf p}
  Flag.Pool s -> set o {PlayOptions.pool = Just (Text.pack s)}
  Flag.Save s -> set o {PlayOptions.save = Just s}
  Flag.Seed s -> CommandLine.orInvalid ("not a seed: " <> s) (Key.fromHex (Text.pack s)) >>= \k -> set o {PlayOptions.seed = Just k}
  where
    o = options config
    set x = pure config {options = x}
    seat s = CommandLine.orInvalid ("not a seat: " <> s) (Monad.mfilter (>= 1) (Read.readMaybe s))

-- | The first seat of so many that nobody plays: one not the bot's, in a game
-- not over the line protocol.
unplayed :: PlayOptions.PlayOptions -> Natural -> Maybe Natural
unplayed o count
  | PlayOptions.json o = Nothing
  | otherwise = List.find (`Set.notMember` PlayOptions.bots o) [1 .. count]

-- | `parun play` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "play" Flag.flags)
    { Command.values =
        [ ("save", Value.MkFiles),
          ("format", Value.MkFormats),
          ("level", Value.MkChoices (Opponent.names Opponent.difficultyName)),
          ("persona", Value.MkChoices (Opponent.names Opponent.personaName)),
          ("meta", Value.MkFiles),
          ("pool", Value.MkFormats)
        ],
      Command.rest = Just Value.MkFiles
    }
