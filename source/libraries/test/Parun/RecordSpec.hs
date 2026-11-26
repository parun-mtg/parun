-- Covers Parun.Play.Record and Parun.Scenario's game start: a played game saved
-- as a scenario, and that scenario replayed, as the web client keeps it
-- (Parun.Web.History) too.
module Parun.RecordSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.CompanionSpec as CompanionSpec
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Record as Record
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Reply as ScenarioReply
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Existence as Existence
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Move as Move
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Program as Program
import qualified Parun.Types.Reference as Reference
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Reply as Reply
import qualified Parun.Types.Result as Result
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.Start as Start
import qualified Parun.Types.Stop as Stop
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.History as History

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Record" $ do
  Spec.it s "a recorded bot game replays clean to the same result" $ do
    matchup <- BotSpec.sevens s registry
    let (outcome, timeline) = recorded bots matchup
    replayed <- Scenario.run registry (saved matchup timeline)
    case (outcome, replayed) of
      (Right (result, _), Right final) -> Spec.assertEqWith s "same result" (GameState.result final) (Just result)
      (_, Left failure) -> Spec.assertFailure s (Text.unpack (Scenario.render failure))
      (Left reason, _) -> Spec.assertFailure s (Text.unpack reason)

  Spec.it s "CR 103.1 a recorded game the second seat started replays each seat under its number" $ do
    matchup <- BotSpec.sevens s registry
    let started = NonEmpty.reverse matchup
        (outcome, timeline) = recorded bots started
    replayed <- Scenario.run registry (saved started timeline)
    case (outcome, replayed) of
      (Right (result, gs), Right final) -> do
        Spec.assertEqWith s "turn order" (GameState.seating final) (GameState.seating gs)
        Spec.assertEqWith s "same result" (GameState.result final) (Just result)
      (_, Left failure) -> Spec.assertFailure s (Text.unpack (Scenario.render failure))
      (Left reason, _) -> Spec.assertFailure s (Text.unpack reason)

  Spec.it s "CR 103.1 a starting player with no seat fails the scenario" $ do
    matchup <- BotSpec.sevens s registry
    let base = saved matchup Seq.empty
        unseated = case Scenario.Type.start base of
          Start.FromDecks format entrants _ -> base {Scenario.Type.start = Start.FromDecks format entrants (Just (Label.MkLabel (Text.pack "carol")))}
          Start.FromBoard _ -> base
    replayed <- Scenario.run registry unseated
    Spec.assertEq s (either (Just . Scenario.render) (const Nothing) replayed) (Just (Text.pack "carol is active but has no seat"))

  Spec.it s "a game the web client keeps replays to the state it ended in" $ do
    matchup <- BotSpec.sevens s registry
    let (outcome, timeline) = recorded bots matchup
        file = Command.rendered (Record.scenario PrintingTable.empty Text.empty GameSettings.plain (fmap (\(pid, deck) -> (pid, Record.labelOf labels pid, GraveyardOrder.Indifferent, deck)) matchup) timeline)
        kept = History.recordOf 0 0 (Text.pack "build") [] (Ending.MkFinished (Ending.MkEnding Ending.MkWon [] file Seq.empty S.alice False []))
    case (outcome, kept) of
      (Right (_, final), Just (game, stored)) -> do
        Spec.assertEqWith s "the summary reads back" (History.decode (History.encode game)) (Right game)
        replayed <- either (Spec.assertFailure s . Text.unpack) (Scenario.run registry) (History.scenarioOf stored)
        either (Spec.assertFailure s . Text.unpack . Scenario.render) (\gs -> Spec.assertBool s (gs == final) "the state it ended in") replayed
      (Left reason, _) -> Spec.assertFailure s (Text.unpack reason)
      (_, Nothing) -> Spec.assertFailure s "not kept"

  Spec.it s "a game cut short replays to where it stopped" $ do
    matchup <- BotSpec.bears s registry
    let (outcome, (timeline, cut)) = watched (Just 2) 1 GameSettings.plain bots Map.empty matchup
    replayed <- Scenario.run registry (saved matchup timeline)
    Spec.assertEqWith s "cut" (fmap fst outcome) (Left (Text.pack "curfew"))
    case replayed of
      Right final -> Spec.assertBool s (Just final == cut) "stopped where it was cut"
      Left failure -> Spec.assertFailure s (Text.unpack (Scenario.render failure))

  -- The web client's replay starts at position 0 (Parun.Browser.Replay).
  Spec.it s "CR 103.2a a recorded game's first position holds every seat's deck in its library" $ do
    matchup <- BotSpec.sevens s registry
    let (_, timeline) = recorded bots matchup
    (_, stop) <- Scenario.replay registry Scenario.unkept (Just 0) (saved matchup timeline)
    case stop of
      Right reached -> Spec.assertEqWith s "each library's size" (fmap length (GameState.library (Stop.state reached))) (Map.fromList [(S.alice, 7), (S.bob, 7)])
      Left failure -> Spec.assertFailure s (Text.unpack (Scenario.render failure))

  -- The web client's replay (Parun.Browser.Replay) resumes each run from
  -- what the runs before it kept (#9655).
  Spec.it s "a replay resumed from the steps kept stops where a fresh one does" $ do
    matchup <- BotSpec.sevens s registry
    let (_, timeline) = recorded bots matchup
        scenario = saved matchup timeline
        count = Natural.length timeline
        positions = [0, count `div` 2, count `div` 2 + 1, count `div` 4, count, count + 1, 1, count - 1]
        reached :: [Either e Stop.Stop] -> [Either e GameState.GameState]
        reached = fmap (fmap Stop.state)
        visit (before, stops) at = do
          (kept, stop) <- Scenario.replay registry before (Just at) scenario
          pure (kept, stops <> [stop])
    (held, resumed) <- Monad.foldM visit (Scenario.keeping, []) positions
    fresh <- traverse (\at -> snd <$> Scenario.replay registry Scenario.unkept (Just at) scenario) positions
    Spec.assertEqWith s "each position, resumed and fresh" (reached resumed) (reached fresh)
    -- A run that resumed never reads the timeline past the step it resumed at.
    (_, blind) <- Scenario.replay registry held (Just (count - 1)) scenario {Scenario.Type.timeline = Seq.empty}
    (_, freshLast) <- Scenario.replay registry Scenario.unkept (Just (count - 1)) scenario
    Spec.assertEqWith s "the last position but one, resumed with no timeline to read" (reached [blind]) (reached [freshLast])

  Spec.it s "CR 404.3: a seat arranging its graveyard is asked again on replay" $ do
    matchup <- scours s registry
    let orders = Map.fromList [(S.alice, GraveyardOrder.Matters), (S.bob, GraveyardOrder.Matters)]
        (_, (timeline, _)) = watched (Just 3) 1 GameSettings.plain bots orders matchup
        arranged = Seq.filter ((== Just (Text.pack "ArrangeGraveyardArrivals")) . answered) timeline
    Spec.assertBool s (not (Seq.null arranged)) "a seat arranged a batch"
    replaysClean s registry (savedWith orders matchup timeline)

  Spec.it s "CR 405.2: a seat targeting a trigger names it by its place on the stack" $ do
    matchup <- stifles s registry
    -- Seed 5's game has a Stifle cast at a trigger on turn 9.
    let (_, (timeline, _)) = watched (Just 9) 5 GameSettings.plain bots Map.empty matchup
    Spec.assertBool s (any targetsStack timeline) "a seat targeted the stack"
    replaysClean s registry (saved matchup timeline)

  -- CR 702.139a: Yorion, Sky Nomad's condition reads the game's format, which
  -- `parun play --format` names and its saved game carries. CR 100.2b: sixty
  -- cards are twenty over limited's forty. CR 103.2b offers the companion
  -- before the game begins, so its first turn is all either game needs.
  Spec.it s "CR 400.7 an object that has left the game is recorded, and described, among the departed of its name" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (first, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (second, g2) = S.addPermanent piker S.alice g1
        (live, g3) = S.addPermanent piker S.alice g2
        gs = S.runPure S.identityAnswer g3 (Event.destroy Settle.none Regenerability.Regenerable [first, second])
        departed = Reference.Printed Existence.Departed (CardName.MkCardName (Text.pack "Goblin Piker"))
        -- Each destroyed Piker's card is a live object in the graveyard, so
        -- the live permanent is the third of its name.
        expected = [departed 1, departed 2, Reference.Printed Existence.Live (CardName.MkCardName (Text.pack "Goblin Piker")) 1]
    Spec.assertEqWith s "recorded" (fmap (ScenarioReply.nameObject (Record.namer Map.empty gs)) [first, second, live]) expected
    Spec.assertEqWith s "described" (fmap (Scenario.referenceTo Map.empty gs) [first, second, live]) expected
  Spec.it s "CR 100.2 a game played under a format offers Yorion, and replays under it" $ do
    matchup <- yorions s registry
    let limited = GameSettings.plain {GameSettings.format = Just (Format.MkFormat RuleFamily.Limited Nothing Set.empty)}
        (_, (timeline, _)) = watched (Just 1) 1 limited bots Map.empty matchup
        (_, (unformatted, _)) = watched (Just 1) 1 GameSettings.plain bots Map.empty matchup
        offers = Seq.length . Seq.filter ((== Just (Text.pack "ChooseCompanion")) . answered)
    Spec.assertEqWith s "CR 103.2b: each seat is offered Yorion" (offers timeline) 2
    Spec.assertEqWith s "CR 100.2: under no format, neither is" (offers unformatted) 0
    replaysClean s registry (savedUnder limited Map.empty matchup timeline)

  -- The lobby's options reach the game through Game.run and its saved game
  -- through Record.scenario, as the format does above.
  Spec.it s "CR 407.2 a game played for ante antes a card from each library, and replays for ante" $ do
    matchup <- BotSpec.bears s registry
    let ante = GameSettings.plain {GameSettings.ante = True}
        anted = maybe 0 (Set.size . GameState.ante)
        (_, (timeline, played)) = watched (Just 1) 1 ante bots Map.empty matchup
        (_, (_, unplayed)) = watched (Just 1) 1 GameSettings.plain bots Map.empty matchup
    Spec.assertEqWith s "CR 407.2: one card from each seat's library" (anted played) 2
    Spec.assertEqWith s "CR 407.1: none in a game not for ante" (anted unplayed) 0
    replayed <- Scenario.run registry (savedUnder ante Map.empty matchup timeline)
    either (Spec.assertFailure s . Text.unpack . Scenario.render) (\final -> Spec.assertEqWith s "the replay antes too" (anted (Just final)) 2) replayed

  Spec.it s "a timeline missing an answer fails to replay" $ do
    matchup <- BotSpec.bears s registry
    -- Seed 3's game attacks on turn 5.
    let (_, (timeline, _)) = watched (Just 6) 3 GameSettings.plain bots Map.empty matchup
        half = Seq.length timeline `div` 2
    -- An attack from the second half on: a passed priority left out replays
    -- the same, the runner passing for a seat with nothing scheduled.
    missing <- maybe (Spec.assertFailure s "no attack to leave out") (pure . (+ half)) (Seq.findIndexL attacks (Seq.drop half timeline))
    replayed <- Scenario.run registry (saved matchup (Seq.deleteAt missing timeline))
    Spec.assertBool s (Either.isLeft replayed) "desync"

-- Asserts that a recording replays without a desync: every answer it holds
-- is asked for where it stands, as "a timeline missing an answer" shows a
-- replay demands.
replaysClean :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Scenario.Type.Scenario -> m ()
replaysClean s registry scenario = do
  replayed <- Scenario.run registry scenario
  either (Spec.assertFailure s . Text.unpack . Scenario.render) (const (pure ())) replayed

-- Whether an entry chooses targets, one of them named by its place on the
-- stack.
targetsStack :: Timed.Timed -> Bool
targetsStack timed = case Timed.entry timed of
  Entry.Do (Move.Answer answer) -> Answer.prompt answer == Text.pack "ChooseTargets" && onStack (Answer.with answer)
  _ -> False
  where
    onStack reply = case reply of
      Reply.Text text -> Text.isPrefixOf (Text.pack "stack ") text
      Reply.Array replies -> any onStack replies
      Reply.Object fields -> any (onStack . snd) fields
      _ -> False

-- Two seats of Council of Advisors, whose enters trigger is on the stack for a
-- Stifle to target, beside Soul Warden's: one trigger alone is bound unasked
-- (CR 601.2c), so the Wardens give the Stifle a choice.
stifles :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
stifles s registry = do
  council <- S.printingOf s registry "Council of Advisors"
  stifle <- S.printingOf s registry "Stifle"
  warden <- S.printingOf s registry "Soul Warden"
  island <- S.printingOf s registry "Island"
  plains <- S.printingOf s registry "Plains"
  let deck = Deck.fromCards (Map.fromList [(council, 8), (stifle, 8), (warden, 4), (island, 8), (plains, 4)])
  pure ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])

-- Twelve Tome Scours and eight Islands apiece: a Scour mills five cards at
-- once, a batch its owner arranges (CR 404.3), from the first turn.
scours :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
scours s registry = do
  scour <- S.printingOf s registry "Tome Scour"
  island <- S.printingOf s registry "Island"
  let deck = Deck.fromCards (Map.fromList [(scour, 12), (island, 8)])
  pure ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])

-- Two seats of Grizzly Bears and Forests, sixty cards with Yorion, Sky Nomad
-- in the sideboard.
yorions :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
yorions s registry = do
  yorion <- S.printingOf s registry "Yorion, Sky Nomad"
  bear <- S.printingOf s registry "Grizzly Bears"
  forest <- S.printingOf s registry "Forest"
  let deck = CompanionSpec.deckOf yorion [(bear, 36), (forest, 24)]
  pure ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])

-- The prompt an entry answers, if it answers one.
answered :: Timed.Timed -> Maybe Text.Text
answered timed = case Timed.entry timed of
  Entry.Do (Move.Answer answer) -> Just (Answer.prompt answer)
  _ -> Nothing

-- Whether an entry declares attackers, any at all.
attacks :: Timed.Timed -> Bool
attacks timed = case Timed.entry timed of
  Entry.Do (Move.Answer answer) -> Answer.prompt answer == Text.pack "DeclareAttackers" && Answer.with answer /= Reply.Array []
  _ -> False

-- The entries recorded so far and the state the last prompt was asked in.
type Base = State.State (Seq.Seq Timed.Timed, Maybe GameState.GameState)

-- A game seeded 1 between these seats, and every entry its answers recorded,
-- however it ended.
recorded :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Base)) -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (Either Text.Text (Result.Result, GameState.GameState), Seq.Seq Timed.Timed)
recorded seats matchup = fmap fst (watched Nothing 1 GameSettings.plain seats Map.empty matchup)

-- recorded's game at a seed under these options and format, each seat at its graveyard order
-- in `orders`.
watched :: Maybe Natural -> Word.Word32 -> GameSettings.GameSettings -> Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Base)) -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (Either Text.Text (Result.Result, GameState.GameState), (Seq.Seq Timed.Timed, Maybe GameState.GameState))
watched curfew seed settings seats orders matchup =
  let played = Game.run Program.foldProgramM (S.fixtureRegistry []) settings Limits.standard ([Record.recording labels sink, lastAsked] <> foldMap (pure . after) curfew) seats (S.seeded seed) orders matchup
   in State.runState played (Seq.empty, Nothing)

-- Stops play, as a seat out of input does, at the first question of a turn
-- after `turn`.
after :: Natural -> Observer.Observer (Game.Play Base)
after turn =
  Observer.MkObserver
    { Observer.before = \asked -> Monad.when (TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) > turn) (Game.stop (Text.pack "curfew")),
      Observer.after = \_ _ -> pure ()
    }

lastAsked :: Observer.Observer (Game.Play Base)
lastAsked = Observer.MkObserver {Observer.before = Game.underneath . State.modify' . fmap . const . Just . Asked.game, Observer.after = \_ _ -> pure ()}

sink :: Timed.Timed -> Game.Play Base ()
sink entry = Game.underneath (State.modify' (Bifunctor.first (Seq.|> entry)))

bots :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Base))
bots = Map.fromList [(S.alice, Game.bot (S.fixtureRegistry [])), (S.bob, Game.bot (S.fixtureRegistry []))]

saved :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Type.Scenario
saved = savedWith Map.empty

-- The scenario a game recorded at these graveyard orders saves as, read back.
savedWith :: Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Type.Scenario
savedWith = savedUnder GameSettings.plain

-- savedWith's scenario of a game played under these options and format.
savedUnder :: GameSettings.GameSettings -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Type.Scenario
savedUnder settings orders matchup timeline =
  let scenario = Record.scenario PrintingTable.empty Text.empty settings (fmap (\(pid, deck) -> (pid, Record.labelOf labels pid, Map.findWithDefault GraveyardOrder.Indifferent pid orders, deck)) matchup) timeline
   in Either.either (error . Text.unpack) id (Codec.decode Codec.Scenario.codec (Codec.encode Codec.Scenario.codec scenario))

labels :: Map.Map PlayerId.PlayerId Label.Label
labels = Map.fromList [(S.alice, Label.MkLabel (Text.pack "alice")), (S.bob, Label.MkLabel (Text.pack "bob"))]
