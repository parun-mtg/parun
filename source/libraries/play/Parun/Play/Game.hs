{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A whole game: each seat answered by its own answerer (a program over
-- lines or the random bot), chance by the generator.
module Parun.Play.Game where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.Script as Script
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Bot as Bot
import qualified Parun.Play.Chance as Chance
import qualified Parun.Play.Generator as Generator.Play
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Registry as Registry
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Result as Result

-- | The generator over the base monad, play stopping with why (stop).
type Play m = State.StateT Generator.Generator (Except.ExceptT Text.Text m)

-- | The random bot, answering each prompt from its seat's view of the game
-- (Driver.seated) and naming a card from the reference's names
-- (Interpreter.cardNames); chance answers the rest.
bot :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
bot registry = Answerer.MkAnswerer $ \asked -> do
  names <- case Asked.prompt asked of
    Prompt.ChooseCardName {} -> Interpreter.fetched halted (Interpreter.cardNames (reference registry))
    _ -> pure []
  Answerer.unwrap (Driver.seated (Bot.bot names) Chance.chance) asked

-- | The goldfish: passes priority every time and takes the least eventful
-- answer to every other prompt its seat decides (Script.passing), naming a
-- card as the random bot does ('bot'); chance answers the rest.
goldfish :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
goldfish registry = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseCardName {} -> Answerer.unwrap (bot registry) asked
  _ -> Answerer.unwrap (Driver.seated (Answerer.MkAnswerer (pure . Script.passing . Asked.prompt)) Chance.chance) asked

-- | A program speaking the protocol (Protocol.Exchange): each request out,
-- its reply back, Nothing at the end of input.
wired :: (Monad m) => Registry.Registry m -> Protocol.Exchange m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
wired registry exchange = Protocol.over (judgingNames registry) (\request -> underneath (exchange request) >>= maybe (stop (Text.pack "end of input")) pure) Chance.chance

-- | `live` with its answers to each prompt a seat answers (Protocol.requestFor)
-- given to `kept` as replies, and taken from `stored` instead while it has
-- one: a game picked up from its seed (Protocol.resumed) asks `live` nothing it
-- answered before. Either way the answer draws apart from the game's generator
-- (Generator.Play.apart), so the game draws alike. RemoteSeatSpec's resumed
-- game against the search bot is the proof.
remembered :: (Monad m) => m (Maybe R.Reply) -> (R.Reply -> m ()) -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m) -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
remembered stored kept live = Answerer.MkAnswerer $ \asked -> case Protocol.requestFor asked of
  Nothing -> Answerer.unwrap live asked
  Just _ -> do
    recalled <- underneath stored
    Generator.Play.apart $ case recalled of
      Just reply -> either stop pure (Protocol.decodeAnswer (Asked.prompt asked) reply)
      Nothing -> do
        answer <- Answerer.unwrap live asked
        underneath (kept (Protocol.encodeAnswer (Asked.prompt asked) answer))
        pure answer

-- | A game over the card reference `registry` ('consulting'), under
-- `settings`' options and format (CR 800.2, 100.2), from setup to its result,
-- its prompts answered through `fold`; the head of the matchup plays first,
-- and each seat starts at its graveyard order in `orders`, under the host's
-- `limits` (Driver.match). Left is why play stopped before the end.
-- RecordSpec's Yorion, Sky Nomad case proves the format reaches the game,
-- and its ante case the options.
run :: (Monad m) => Driver.Fold (Play m) -> Registry.Registry m -> GameSettings.GameSettings -> Limits.Limits -> [Observer.Observer (Play m)] -> Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)) -> Generator.Generator -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> m (Either Text.Text (Result.Result, GameState.GameState))
run fold registry settings limits observers seats generator orders matchup =
  playing generator (Driver.match fold observers (consulting registry seats) settings limits orders matchup)

-- | Play from the generator, in the base monad: Left is why it stopped first.
playing :: (Monad m) => Generator.Generator -> Play m a -> m (Either Text.Text a)
playing generator played = Except.runExceptT (State.evalStateT played generator)

-- | Each seat, then chance, over the card reference `registry`: a name a seat
-- chooses is asked again until CR 201.4 allows it
-- (Interpreter.policingCardNames; wired seats are told why, judgingNames),
-- and CR 108.1's questions about the reference are answered from it
-- (Interpreter.lookingUpCards). PlaySpec's Null Chamber case is the proof.
consulting :: (Monad m) => Registry.Registry m -> Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)) -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
consulting registry seats =
  Driver.bySeat
    (fmap (policed registry) seats)
    (Answerer.MkAnswerer (Interpreter.lookingUpCards (reference registry) halted (Answerer.unwrap Chance.chance)))

-- | A seat whose card names CR 201.4 refuses are asked again.
policed :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m) -> Answerer.Answerer (Asked.Asked GameState.GameState) (Play m)
policed registry (Answerer.MkAnswerer seat) = Answerer.MkAnswerer (Interpreter.policingCardNames (reference registry) halted seat)

-- | A chosen name the reference `registry` refuses, with why: CR 201.4's
-- name of no card, or CR 201.4a's card without the characteristics asked for.
-- Every other answer stands. PlaySpec's Null Chamber refusal is the proof.
judgingNames :: (Monad m) => Registry.Registry m -> Asked.Asked GameState.GameState r -> r -> Play m (Either Text.Text r)
judgingNames registry asked answer = case Asked.prompt asked of
  Prompt.ChooseCardName _ chooser _ restriction _ -> do
    found <- Interpreter.fetched halted (Interpreter.chosenFace (reference registry) answer)
    pure $ case found of
      Nothing -> Left (CardName.unwrap answer <> Text.pack " is not the name of a card")
      Just _
        | Interpreter.admitsName (Asked.game asked) chooser restriction found -> Right answer
        | otherwise -> Left (CardName.unwrap answer <> Text.pack " is not a card name this choice allows")
  _ -> pure (Right answer)

-- | The registry, its lookups made from inside a game.
reference :: (Monad m) => Registry.Registry m -> Registry.Registry (Play m)
reference = Registry.hoist underneath

-- | Card data that cannot be read stops play, saying why.
halted :: (Monad m) => InvalidCorpus.InvalidCorpus -> Play m r
halted = stop . Text.pack . Exception.displayException

-- | Play stops, saying why.
stop :: (Monad m) => Text.Text -> Play m a
stop = Trans.lift . Except.throwE

-- | Something of the base monad's, from inside a game.
underneath :: (Monad m) => m a -> Play m a
underneath = Trans.lift . Trans.lift

-- | CR 103.1: the seat that plays first, of `seats` in turn order. `--first`
-- names it; otherwise a seat drawn from the generator chooses which seat
-- takes the first turn, the bot through 'Bot.starting' and any other seat by
-- `ask`, which is Nothing at the end of input. Each step is told through
-- `tell`.
starter :: (Monad m) => (Text.Text -> m ()) -> (Natural -> NonEmpty.NonEmpty Natural -> m (Maybe Natural)) -> NonEmpty.NonEmpty Natural -> PlayOptions.PlayOptions -> State.StateT Generator.Generator m (Maybe Natural)
starter tell ask seats options = case PlayOptions.first options of
  Just seat -> pure (Just seat)
  Nothing -> do
    chooser <- Generator.Play.pick seats
    Trans.lift (tell (Text.pack ("Seat " <> show chooser <> " was picked at random to choose who plays first.")))
    chosen <-
      if Map.member chooser (PlayOptions.bots options)
        then fmap Just (Bot.starting chooser seats)
        else Trans.lift (ask chooser seats)
    Monad.forM chosen $ \seat -> do
      Trans.lift (tell (Text.pack ("Seat " <> show seat <> " plays first.")))
      pure seat
