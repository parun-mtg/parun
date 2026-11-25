-- | What `parun play`'s command line asks for, each flag's argument checked.
module Parun.Play.Command.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
-- seat nobody plays. Each `--level` and `--persona` is read once every
-- `--bot` is, so it may come before the seat it names.
fromFlags :: (Catch.MonadThrow m) => [Flag.Flag] -> [String] -> m Config
fromFlags fs positional = do
  let (botFlags, otherFlags) = List.partition isBotFlag fs
  config <- Monad.foldM applyFlag initial (otherFlags <> botFlags)
  let o = options config
      count = List.genericLength positional :: Natural
  Monad.unless (help config) $ do
    CommandLine.positionals ["DECK", "DECK"] Nothing positional
    Monad.forM_ (List.find (> count) (Maybe.maybeToList (PlayOptions.first o) <> Set.toList (Map.keysSet (PlayOptions.bots o) <> PlayOptions.arrangesGraveyard o))) $ \n ->
      Catch.throwM (InvalidOption.MkInvalidOption ("not a seat: " <> show n))
    Monad.forM_ (unplayed o count) $ \n ->
      Catch.throwM (InvalidOption.MkInvalidOption ("seat " <> show n <> " has no player: name it with --bot, or speak the line protocol with --json"))
  pure config {decks = positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.ArrangeGraveyard s -> seatNamed s >>= \n -> set o {PlayOptions.arrangesGraveyard = Set.insert n (PlayOptions.arrangesGraveyard o)}
  Flag.Bot s -> seatNamed s >>= \n -> set o {PlayOptions.bots = Map.insert n PlayOptions.bot (PlayOptions.bots o)}
  Flag.First s -> seatNamed s >>= \n -> set o {PlayOptions.first = Just n}
  Flag.Format s -> set o {PlayOptions.format = Just (Text.pack s)}
  Flag.Help b -> pure config {help = b}
  Flag.Json b -> set o {PlayOptions.json = b}
  Flag.Level s -> perBot s $ \value -> CommandLine.orInvalid ("not a level, easy, medium or hard: " <> value) (Opponent.named Opponent.difficultyName value) >>= \d -> pure (Bifunctor.first (Opponent.budgetAt d))
  Flag.Meta s -> set o {PlayOptions.meta = Just s}
  Flag.ObjectLimit s -> CommandLine.orInvalid ("not a number of objects: " <> s) (Read.readMaybe s) >>= \n -> set o {PlayOptions.limits = (PlayOptions.limits o) {Limits.objects = n}}
  Flag.Persona s -> perBot s $ \value -> CommandLine.orInvalid ("not a persona, balanced, aggro or control: " <> value) (Opponent.named Opponent.personaName value) >>= \p -> pure (Bifunctor.second (const (Opponent.weightsOf p)))
  Flag.Pool s -> set o {PlayOptions.pool = Just (Text.pack s)}
  Flag.Save s -> set o {PlayOptions.save = Just s}
  Flag.Seed s -> CommandLine.orInvalid ("not a seed: " <> s) (Key.fromHex (Text.pack s)) >>= \k -> set o {PlayOptions.seed = Just k}
  Flag.ThinkLimit s -> CommandLine.orInvalid ("not a number of milliseconds: " <> s) (Read.readMaybe s) >>= \n -> set o {PlayOptions.thinkLimit = n}
  where
    o = options config
    set x = pure config {options = x}
    -- A flag's value in every bot's seat seated so far, or with a `SEAT=`
    -- prefix in that seat's alone, which must be a bot's.
    perBot s parse = case break (== '=') s of
      (prefix, '=' : value) -> do
        n <- seatNamed prefix
        Monad.unless (Map.member n (PlayOptions.bots o)) $ Catch.throwM (InvalidOption.MkInvalidOption ("seat " <> show n <> " is not the bot's: name it with --bot"))
        update <- parse value
        set o {PlayOptions.bots = Map.adjust update n (PlayOptions.bots o)}
      _ -> parse s >>= \update -> set o {PlayOptions.bots = Map.map update (PlayOptions.bots o)}

-- | Whether a flag sets how the search bot plays in its seats, and so is
-- read after every other.
isBotFlag :: Flag.Flag -> Bool
isBotFlag flag = case flag of
  Flag.Level _ -> True
  Flag.Persona _ -> True
  _ -> False

-- | The seat a flag's argument names, from 1.
seatNamed :: (Catch.MonadThrow m) => String -> m Natural
seatNamed s = CommandLine.orInvalid ("not a seat: " <> s) (Monad.mfilter (>= 1) (Read.readMaybe s))

-- | The first seat of so many that nobody plays: one not the bot's, in a game
-- not over the line protocol.
unplayed :: PlayOptions.PlayOptions -> Natural -> Maybe Natural
unplayed o count
  | PlayOptions.json o = Nothing
  | otherwise = List.find (`Map.notMember` PlayOptions.bots o) [1 .. count]

-- | `parun play` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "play" Flag.flags)
    { Command.values =
        [ ("save", Value.MkFiles),
          ("format", Value.MkFormats),
          ("level", Value.MkSeated seats (Value.MkChoices (Opponent.names Opponent.difficultyName))),
          ("persona", Value.MkSeated seats (Value.MkChoices (Opponent.names Opponent.personaName))),
          ("meta", Value.MkFiles),
          ("pool", Value.MkFormats)
        ],
      Command.rest = Just Value.MkFiles
    }
  where
    -- The seats a `SEAT=` prefix completes to: a table of four's.
    seats = fmap show [1 .. 4 :: Natural]
