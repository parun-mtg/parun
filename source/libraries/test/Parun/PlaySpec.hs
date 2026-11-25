{-# LANGUAGE GADTs #-}

-- Covers Parun.Play.Game, Parun.Play.Wording and Parun.Play.Command's game
-- and branch: who plays first, the card names a seat may choose, how a
-- seat's view and a finished game are worded for people, what a saved game
-- says it was played with, a game played on from a position of its
-- recording, and when a bot's decision clock runs out (Command.timed).
module Parun.PlaySpec where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.BotSpec as BotSpec
import qualified Parun.CastProhibitionSpec as CastProhibitionSpec
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game.Engine
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Resolve.Effect as Resolve
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Command.Config as Config
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Search as Search
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.ResolveSpec as ResolveSpec
import qualified Parun.Scenario as Scenario.Runner
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.BecameAttached as BecameAttached
import qualified Parun.Types.BecameUnattached as BecameUnattached
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CoinFlipped as CoinFlipped
import qualified Parun.Types.Convoking as Convoking
import qualified Parun.Types.CountedAct as CountedAct
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterName as CounterName
import qualified Parun.Types.CountersPlaced as CountersPlaced
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.DieResult as DieResult
import qualified Parun.Types.Drew as Drew
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.Expended as Expended
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.Label as Label
import qualified Parun.Types.LifeChange as LifeChange
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Looked as Looked
import qualified Parun.Types.Move as Move
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.MulliganDecision as MulliganDecision
import qualified Parun.Types.MulliganOffer as MulliganOffer
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerCountersPlaced as PlayerCountersPlaced
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.RollReading as RollReading
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.Stop as Stop
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.When as When
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange
import qualified Parun.Web.Log as Log

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Play" $ do
  Spec.it s "CR 122.1 a counter is told by the name the card prints" $
    Spec.assertEq
      s
      (fmap (Wording.counters 2) [CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "charge")), CounterKind.powerToughness 1 0, CounterKind.powerToughness (-1) (-2), CounterKind.PlusOnePlusOne])
      (fmap Text.pack ["2 charge counters", "2 +1/+0 counters", "2 -1/-2 counters", "2 +1/+1 counters"])

  Spec.it s "a run of one player's draws is told as one" $ do
    gs <- started s registry
    let drew pid n = LoggedEvent.MkLoggedEvent (EventGroup.MkEventGroup n) (GameEvent.Drew (Drew.MkDrew pid n))
        view = PlayerView.viewFor S.alice (witnessing [drew S.bob 1, drew S.bob 2, drew S.alice 1, drew S.bob 3] gs)
    Spec.assertEqWith s "told" (filter (Text.isInfixOf (Text.pack " draws ")) (told view)) [Text.pack "Player 2 draws 2 cards.", Text.pack "Player 1 draws a card.", Text.pack "Player 2 draws a card."]

  Spec.it s "CR 103.5 another seat is told who kept, who mulliganed and how many cards each mulligan bottoms" $ do
    matchup <- BotSpec.bears s registry
    let answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.DeclareMulligan _ pid offer | pid == S.alice && MulliganOffer.taken offer < 2 -> MulliganDecision.Mulligan
          _ -> S.castAnswer p
        gs = snd (Engine.runGamePure answer (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup))
    Spec.assertEqWith
      s
      "told to Player 2"
      (told (PlayerView.viewFor S.bob gs))
      [ Text.pack "Player 1's library is shuffled.",
        Text.pack "Player 2's library is shuffled.",
        Text.pack "Player 1 draws 7 cards.",
        Text.pack "Player 2 draws 7 cards.",
        Text.pack "Player 1 mulligans, bottoming 1 card.",
        Text.pack "Player 2 keeps.",
        Text.pack "Player 1's library is shuffled.",
        Text.pack "Player 1 draws 7 cards.",
        Text.pack "Player 1 mulligans, bottoming 2 cards.",
        Text.pack "Player 1's library is shuffled.",
        Text.pack "Player 1 draws 7 cards.",
        Text.pack "Player 1 keeps."
      ]

  Spec.it s "CR 103.5 a goldfish game's opening hand is told as one draw before the first turn" $ do
    matchup <- BotSpec.bears s registry
    let alone = NonEmpty.head matchup NonEmpty.:| []
        gs = snd (Engine.runGamePure S.castAnswer (Setup.gameWith GameSettings.plain (fmap fst alone)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) alone))
    Spec.assertEqWith
      s
      "logged for Player 1"
      (snd (Log.unseen Map.empty (PlayerView.viewFor S.alice gs)))
      [ Log.MkPregame,
        Log.MkEvent Log.MkMajor (Text.pack "Your library is shuffled."),
        Log.MkEvent Log.MkMajor (Text.pack "You draw 7 cards."),
        Log.MkEvent Log.MkMajor (Text.pack "You keep.")
      ]
    -- Turn 1 begins at its first step (Turn.pregame), under its own heading.
    let opening = PlayerView.viewFor S.alice gs
        (marks, _) = Log.unseen Map.empty opening
        logged = inOneGroup [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Untap) S.alice), GameEvent.Drew (Drew.MkDrew S.alice 1)]
        begun = opening {PlayerView.Type.begun = True, PlayerView.Type.witnessed = PlayerView.Type.witnessed opening <> logged}
        drawn = [Log.MkTurn 1 S.alice, Log.MkEvent Log.MkMajor (Text.pack "You draw a card.")]
    Spec.assertEqWith s "read on into turn 1" (snd (Log.unseen marks begun)) drawn
    Spec.assertEqWith s "read first in turn 1" (snd (Log.unseen Map.empty begun)) (take 4 (snd (Log.unseen Map.empty opening)) <> drawn)

  Spec.it s "CR 103.5 the web client's log tells its own seat's mulligans to it as You" $ do
    matchup <- BotSpec.bears s registry
    let answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.DeclareMulligan _ pid offer | pid == S.alice && MulliganOffer.taken offer < 2 -> MulliganDecision.Mulligan
          _ -> S.castAnswer p
        gs = snd (Engine.runGamePure answer (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup))
    Spec.assertEqWith
      s
      "logged for Player 1"
      (Log.events S.alice (Seq.fromList (snd (Log.unseen Map.empty (PlayerView.viewFor S.alice gs)))))
      [ Text.pack "Your library is shuffled.",
        Text.pack "Player 2's library is shuffled.",
        Text.pack "You draw 7 cards.",
        Text.pack "Player 2 draws 7 cards.",
        Text.pack "You mulligan, bottoming 1 card.",
        Text.pack "Player 2 keeps.",
        Text.pack "Your library is shuffled.",
        Text.pack "You draw 7 cards.",
        Text.pack "You mulligan, bottoming 2 cards.",
        Text.pack "Your library is shuffled.",
        Text.pack "You draw 7 cards.",
        Text.pack "You keep."
      ]

  Spec.it s "an entry told to its own seat names it you, in the second person" $ do
    gs <- started s registry
    let acted action pid = GameEvent.PlayerActed (PlayerActed.MkPlayerActed action pid)
        poison putter pid n = GameEvent.PlayerCountersPut (PlayerCountersPlaced.MkPlayerCountersPlaced putter pid PlayerCounterKind.Poison 0 n)
        look oid name = GameEvent.Looked (Looked.MkLooked S.alice (ObjectId.MkObjectId oid) S.emptyCharacteristics {ProjectedCharacteristics.names = Set.singleton (CardName.MkCardName (Text.pack name))} (Set.singleton S.alice))
        logged =
          [ acted PlayerAction.Scry S.alice,
            acted PlayerAction.Scry S.bob,
            acted PlayerAction.SearchLibrary S.alice,
            acted PlayerAction.TemptedByRing S.alice,
            look 90 "Forest",
            look 91 "Island",
            GameEvent.Expended (Expended.MkExpended S.alice 0 3),
            GameEvent.CoinFlipped (CoinFlipped.MkCoinFlipped S.alice (Just True)),
            poison S.alice S.alice 1,
            poison S.bob S.alice 2,
            GameEvent.LifeLost (LifeChange.MkLifeChange S.alice 2 True)
          ]
        view = PlayerView.viewFor S.alice (witnessing (inOneGroup logged) gs)
    Spec.assertEqWith
      s
      "told to Player 1"
      (Wording.events (Just S.alice) view (PlayerView.Type.witnessed view))
      [ Text.pack "You scry.",
        Text.pack "Player 2 scries.",
        Text.pack "You search your library.",
        Text.pack "You are tempted by the Ring.",
        Text.pack "You look at Forest and Island.",
        Text.pack "You flip a coin and win the flip.",
        Text.pack "You put 1 poison counter on yourself.",
        Text.pack "Player 2 puts 2 poison counters on you.",
        Text.pack "You lose 2 life."
      ]
    Spec.assertEqWith
      s
      "told to nobody, as the line protocol"
      (Wording.events Nothing view (PlayerView.Type.witnessed view))
      [ Text.pack "Player 1 scries.",
        Text.pack "Player 2 scries.",
        Text.pack "Player 1 searches their library.",
        Text.pack "Player 1 is tempted by the Ring.",
        Text.pack "Player 1 looks at Forest and Island.",
        Text.pack "Player 1 flips a coin and wins the flip.",
        Text.pack "Player 1 puts 1 poison counter on Player 1.",
        Text.pack "Player 2 puts 2 poison counters on Player 1.",
        Text.pack "Player 1 loses 2 life."
      ]

  Spec.it s "CR 701.3a / 702.51a / 122.1 each logged entry is told with its subject, a token as created and a move to the stack as nothing" $ do
    gs <- started s registry
    ensoul <- S.printingOf s registry "Ensoul Artifact"
    bears <- S.printingOf s registry "Grizzly Bears"
    totem <- S.printingOf s registry "Living Totem"
    wurm <- S.printingOf s registry "Siege Wurm"
    let (aura, g1) = S.addPermanent ensoul S.alice gs
        (bear, g2) = S.addPermanent bears S.alice g1
        (totemId, g3) = S.addPermanent totem S.alice g2
        (wurmId, board) = S.addPermanent wurm S.alice g3
        soldier = S.emptyCharacteristics {ProjectedCharacteristics.names = Set.singleton (CardName.MkCardName (Text.pack "Soldier Token"))}
        moved from to = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange (ObjectId.MkObjectId 90) (ObjectId.MkObjectId 91) from to) soldier) {Moved.seenArriving = True}
        logged =
          [ GameEvent.BecameAttached (BecameAttached.MkBecameAttached aura (Recipient.ToObject bear)),
            GameEvent.BecameUnattached (BecameUnattached.MkBecameUnattached aura (Recipient.ToObject bear)),
            GameEvent.Convoked (Convoking.MkConvoking wurmId (Set.fromList [bear, totemId])),
            GameEvent.CountersPut (CountersPlaced.MkCountersPlaced S.alice wurmId CounterKind.PlusOnePlusOne 0 1 (S.permanentSnapshot wurmId S.alice 0)),
            moved Zone.Battlefield Zone.Battlefield,
            moved Zone.Hand Zone.Stack
          ]
    Spec.assertEqWith
      s
      "told"
      (told (PlayerView.viewFor S.alice (witnessing (inOneGroup logged) board)))
      [ Text.pack "Ensoul Artifact becomes attached to Grizzly Bears.",
        Text.pack "Ensoul Artifact becomes unattached from Grizzly Bears.",
        Text.pack "Grizzly Bears and Living Totem convoke Siege Wurm.",
        Text.pack "Player 1 puts 1 +1/+1 counter on Siege Wurm.",
        Text.pack "Soldier Token is created."
      ]

  Spec.it s "CR 701.23a / 701.24a another seat is told a search of a library and its shuffle" $ do
    board <- ResolveSpec.loreBoard s registry
    let view = PlayerView.viewFor S.bob (ResolveSpec.resolveLore (ResolveSpec.findPinned [ResolveSpec.loreForest board]) board)
        searching line = Text.isInfixOf (Text.pack "search") line || Text.isInfixOf (Text.pack "shuffle") line
    Spec.assertEqWith s "told to Player 2" (filter searching (told view)) [Text.pack "Player 1 searches their library.", Text.pack "Player 1's library is shuffled."]

  Spec.it s "CR 701.22a / 701.25a / 706.1 a scry and a surveil are told, a roll only by its result" $ do
    gs <- started s registry
    let logged =
          [ GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.Scry S.alice),
            GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.Surveil S.bob),
            GameEvent.CountedAct (CountedAct.MkCountedAct PlayerAction.RollDice S.alice 4),
            GameEvent.DieResultSettled DieResult.MkDieResult {DieResult.roller = S.alice, DieResult.result = 4, DieResult.reading = RollReading.FinalResult},
            GameEvent.DieResultSettled DieResult.MkDieResult {DieResult.roller = S.alice, DieResult.result = 4, DieResult.reading = RollReading.NaturalResult}
          ]
    Spec.assertEqWith s "told" (told (PlayerView.viewFor S.bob (witnessing (inOneGroup logged) gs))) [Text.pack "Player 1 scries.", Text.pack "Player 2 surveils.", Text.pack "Player 1 rolls 4."]

  Spec.it s "CR 104.3c a finished game says how its loser left" $ do
    matchup <- BotSpec.sevens s registry
    let bots :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Identity.Identity))
        bots = Map.fromList [(S.alice, Game.bot (S.fixtureRegistry [])), (S.bob, Game.bot (S.fixtureRegistry []))]
    case Identity.runIdentity (Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] bots (S.seeded 1) Map.empty matchup) of
      Right (Result.Won winner, gs) ->
        let loser = if winner == S.alice then S.bob else S.alice
         in Spec.assertEq s (Wording.departures Nothing gs) [Wording.player loser <> Text.pack " lost the game: drew from an empty library."]
      outcome -> Spec.assertBool s False ("won: " <> show (fmap fst outcome))

  Spec.it s "the goldfish never casts a spell or plays a land" $ do
    matchup <- BotSpec.bears s registry
    let seats :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Identity.Identity))
        seats = Map.fromList [(S.alice, Game.goldfish (S.fixtureRegistry [])), (S.bob, Game.bot (S.fixtureRegistry []))]
        ownedBy pid gs = [oid | oid <- Set.toList (GameState.battlefield gs), Just o <- [Map.lookup oid (GameState.objects gs)], Object.owner o == pid]
    case Identity.runIdentity (Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] seats (S.seeded 1) Map.empty matchup) of
      Right (_, gs) -> Spec.assertEqWith s "the goldfish's permanents, and the bot's" (null (ownedBy S.alice gs), null (ownedBy S.bob gs)) (True, False)
      Left reason -> Spec.assertBool s False ("finished: " <> Text.unpack reason)

  Spec.it s "CR 201.4 the goldfish names a card the reference holds and the choice allows" $ do
    reference <- mapM (S.cardOf s registry) ["Forest", "Goblin Piker"]
    let fixture :: Registry.Registry Identity.Identity
        fixture = S.fixtureRegistry reference
        naming = Prompt.ChooseCardName (Decider.MkDecider S.alice) S.alice (ObjectId.MkObjectId 7) CastProhibitionSpec.nonBasicLandName Seq.empty
        asked = Asked.MkAsked {Asked.enclosing = [], Asked.game = Setup.emptyGame S.bothPlayers, Asked.prompt = naming}
        named = Identity.runIdentity (Game.playing (S.seeded 1) (Answerer.unwrap (Game.consulting fixture (Map.singleton S.alice (Game.goldfish fixture))) asked))
    Spec.assertEq s named (Right (CardName.MkCardName (Text.pack "Goblin Piker")))

  Spec.it s "CR 104.4a a game of one seat runs until that player leaves, a draw" $ do
    alone <- NonEmpty.fromList . NonEmpty.take 1 <$> BotSpec.sevens s registry
    let bots :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play Identity.Identity))
        bots = Map.singleton S.alice (Game.bot (S.fixtureRegistry []))
    case Identity.runIdentity (Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] bots (S.seeded 1) Map.empty alone) of
      Right (Result.Drawn, gs) -> Spec.assertEq s (Wording.departures Nothing gs) [Wording.player S.alice <> Text.pack " lost the game: drew from an empty library."]
      outcome -> Spec.assertBool s False ("drawn: " <> show (fmap fst outcome))

  Spec.it s "CR 704.5 a player who lost is told by every state-based action they met" $ do
    let board = Setup.emptyGame S.bothPlayers
        dying player = player {Player.life = -2, Player.counters = Map.singleton PlayerCounterKind.Poison 10}
        settled = S.settleSba board {GameState.players = Map.adjust dying S.alice (GameState.players board), GameState.drewFromEmpty = Set.singleton S.bob}
    Spec.assertEq s (Wording.departures Nothing settled) [Text.pack "Player 1 lost the game: life total -2 and 10 poison counters.", Text.pack "Player 2 lost the game: drew from an empty library."]
    Spec.assertEqWith s "to the loser" (Wording.departures (Just S.alice) settled) [Text.pack "You lost the game: life total -2 and 10 poison counters.", Text.pack "Player 2 lost the game: drew from an empty library."]

  Spec.it s "CR 104.3a CR 104.3e a concession and an effect's loss are told apart" $ do
    let board = Setup.emptyGame S.bothPlayers
        left = S.runPure S.identityAnswer board (Departure.leaveGame Departure.Type.Conceded S.alice >> Departure.leaveGame Departure.Type.Lost S.bob)
    Spec.assertEq s (Wording.departures Nothing left) [Text.pack "Player 1 conceded.", Text.pack "Player 2 lost the game to an effect."]

  Spec.it s "CR 104.2a a win is told to its reader as the board names the seat" $
    Spec.assertEq s (fmap (\reader -> Wording.result reader (Result.Won S.alice)) [Nothing, Just S.alice, Just S.bob]) [Text.pack "Player 1 wins.", Text.pack "You win.", Text.pack "Player 1 wins."]

  Spec.it s "CR 104.2c a team's win names the team" $
    Spec.assertEq s (Wording.result Nothing (Result.TeamWon (TeamId.MkTeamId 1))) (Text.pack "Team 2 wins.")

  Spec.it s "CR 201.4 / 201.4a a name chosen over the line is refused, saying why, unless the reference holds it and the choice allows it" $ do
    reference <- mapM (S.cardOf s registry) ["Forest", "Goblin Piker"]
    let naming = Prompt.ChooseCardName (Decider.MkDecider S.alice) S.alice (ObjectId.MkObjectId 7) CastProhibitionSpec.nonBasicLandName Seq.empty
        asked = Asked.MkAsked {Asked.enclosing = [], Asked.game = Setup.emptyGame S.bothPlayers, Asked.prompt = naming}
        judged name = Identity.runIdentity (Game.playing (S.seeded 1) (Game.judgingNames (S.fixtureRegistry reference) asked (CardName.MkCardName (Text.pack name))))
    Spec.assertEqWith s "CR 201.4a a name the choice forbids" (judged "Forest") (Right (Left (Text.pack "Forest is not a card name this choice allows")))
    Spec.assertEqWith s "CR 201.4 the name of no card" (judged "Goblin Pikr") (Right (Left (Text.pack "Goblin Pikr is not the name of a card")))
    Spec.assertEqWith s "a name allowed" (judged "Goblin Piker") (Right (Right (CardName.MkCardName (Text.pack "Goblin Piker"))))

  -- CR 201.4 / 201.4a / 108.1: Null Chamber's two names, each chosen by a bot
  -- seat through Game.consulting over a reference of Plains, Goblin Piker and
  -- Cancel. Plains is refused by the card's restriction and asked again, so
  -- each seat names one of the other two, each over the seeds; each name is
  -- looked up in that reference.
  Spec.it s "CR 201.4 a bot seat names a card the reference holds and the restriction allows" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    nullChamber <- S.printingOf s registry "Null Chamber"
    reference <- mapM (S.cardOf s registry) ["Plains", "Goblin Piker", "Cancel"]
    let (oid, board) = CastProhibitionSpec.nullChamberBoard plains mountain nullChamber
        fixture :: Registry.Registry Identity.Identity
        fixture = S.fixtureRegistry reference
        seats = Map.fromList [(S.alice, Game.bot fixture), (S.bob, Game.bot fixture)]
        answer :: Asked.Asked GameState.GameState r -> Game.Play Identity.Identity r
        answer asked = case Asked.prompt asked of
          Prompt.ChooseCardName {} -> Answerer.unwrap (Game.consulting fixture seats) asked
          Prompt.LookUpCard {} -> Answerer.unwrap (Game.consulting fixture seats) asked
          p -> pure (CastProhibitionSpec.chamberAnswer S.bob (const (CardName.MkCardName Text.empty)) p)
        played seed = Identity.runIdentity (Except.runExceptT (State.evalStateT (Engine.runGameAsked answer board (S.cast S.alice oid >> Stack.resolveTop)) (S.seeded seed)))
        chosen seed = case played seed of
          Right (_, after) -> do
            chamber <- CastProhibitionSpec.enteredOne board after >>= (`Game.Engine.lookupObject` after)
            Just (Map.findWithDefault Map.empty Acquisition.Own (Object.chosenNames chamber), Game.Engine.referenceFaces after)
          Left _ -> Nothing
        runs = fmap chosen [1 .. 32 :: Word.Word32]
        allowed = Set.fromList (fmap CardName.MkCardName [Text.pack "Goblin Piker", Text.pack "Cancel"])
        namedBy pid = Set.unions [name | Just (names, _) <- runs, Just name <- [Map.lookup pid names]]
    Spec.assertBool s (all Maybe.isJust runs) "Null Chamber reached the battlefield every time"
    Spec.assertEqWith s "alice named each allowed name, and only those" (namedBy S.alice) allowed
    Spec.assertEqWith s "bob named each allowed name, and only those" (namedBy S.bob) allowed
    Spec.assertBool s (and [all (`Map.member` faces) (Set.unions (Map.elems names)) | Just (names, faces) <- runs]) "CR 108.1 each name was looked up in the reference"

  Spec.it s "CR 103.1 a seat picked at random from the seed chooses to play first or draw" $ do
    let seeds = [0 .. 15 :: Word.Word32]
        firstWhen given seed = fst (fst (starting PlayOptions.none given seed))
        askedOf seed = asks (snd (starting PlayOptions.none [plays] seed))
    Spec.assertEqWith s "the answer decides" (fmap (\seed -> (firstWhen [plays] seed, firstWhen [draws] seed)) seeds) (fmap (\seed -> let chooser = firstWhen [plays] seed in (chooser, fmap otherSeat chooser)) seeds)
    Spec.assertEqWith s "either seat may be picked" (Set.fromList (fmap (firstWhen [plays]) seeds)) (Set.fromList [Just 1, Just 2])
    Spec.assertEqWith s "the picked seat is asked" (fmap askedOf seeds) (fmap (Maybe.maybeToList . firstWhen [plays]) seeds)
    Spec.assertBool s (all (\seed -> snd (fst (starting PlayOptions.none [plays] seed)) /= S.seeded seed) seeds) "drawn from the game's generator"
    Spec.assertEqWith s "end of input" (fst (fst (starting PlayOptions.none [] 3))) Nothing

  Spec.it s "CR 103.1 among three seats, the seat picked chooses which seat takes the first turn" $ do
    let seeds = [0 .. 15 :: Word.Word32]
        three = 1 NonEmpty.:| [2, 3 :: Natural]
        run = startingAmong three PlayOptions.none
        third _ _ = Just 3
        chooserOf seed = Maybe.listToMaybe (pickedIn (snd (run [third] seed)))
    Spec.assertEqWith s "the seat answered plays first" (fmap (fst . fst . run [third]) seeds) (fmap (const (Just 3)) seeds)
    Spec.assertEqWith s "any seat may be picked to choose" (Set.fromList (fmap chooserOf seeds)) (Set.fromList (fmap Just [1, 2, 3]))
    Spec.assertEqWith s "the picked seat is asked" (fmap (asks . snd . run [third]) seeds) (fmap (Maybe.maybeToList . chooserOf) seeds)
    let bots = fmap (startingAmong three PlayOptions.none {PlayOptions.bots = Map.fromList [(n, PlayOptions.bot) | n <- [1, 2, 3]]} []) seeds
    Spec.assertEqWith s "a bot chooses any seat" (Set.fromList (fmap (fst . fst) bots)) (Set.fromList (fmap Just [1, 2, 3]))
    let choices = Map.fromListWith Set.union [(picked, Set.singleton first) | ((first, _), heard) <- bots, picked <- pickedIn heard]
    Spec.assertBool s (any ((> 1) . Set.size) choices) "a bot does not always choose the same seat"

  Spec.it s "CR 103.1 --first names the seat that plays first, asking and drawing nothing" $ do
    let ((first, generator), heard) = starting PlayOptions.none {PlayOptions.first = Just 2} [] 5
    Spec.assertEqWith s "first" first (Just 2)
    Spec.assertEqWith s "generator" generator (S.seeded 5)
    Spec.assertEqWith s "told and asked" (said heard, asks heard) ([], [])

  Spec.it s "CR 103.1 a bot picked to choose answers for itself" $ do
    let bots = PlayOptions.none {PlayOptions.bots = Map.fromList [(n, PlayOptions.bot) | n <- [1, 2]]}
        runs = fmap (starting bots []) [0 .. 15 :: Word.Word32]
    Spec.assertEqWith s "first" (Set.fromList (fmap (fst . fst) runs)) (Set.fromList [Just 1, Just 2])
    Spec.assertEqWith s "asked" (fmap (asks . snd) runs) (fmap (const []) runs)
    Spec.assertBool s (any (\((first, _), heard) -> any (\n -> otherSeat n `elem` pickedIn heard) first) runs) "the bot sometimes draws"

  Spec.it s "parun play refuses a seat neither the bot nor the line protocol plays" $ do
    Spec.assertEqWith s "no seat the bot's" (Config.unplayed PlayOptions.none 2) (Just 1)
    Spec.assertEqWith s "one seat the bot's" (Config.unplayed PlayOptions.none {PlayOptions.bots = Map.singleton 1 PlayOptions.bot} 3) (Just 2)
    Spec.assertEqWith s "every seat the bot's" (Config.unplayed PlayOptions.none {PlayOptions.bots = Map.fromList [(n, PlayOptions.bot) | n <- [1, 2]]} 2) Nothing
    Spec.assertEqWith s "over the line protocol" (Config.unplayed PlayOptions.none {PlayOptions.json = True} 2) Nothing

-- A game after setup: alice red, bob green, hands drawn.
started :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
started s registry = do
  matchup <- BotSpec.bears s registry
  pure (snd (Engine.runGamePure S.castAnswer (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup)))

-- The game with these entries as its turn's log.
witnessing :: [LoggedEvent.LoggedEvent] -> GameState.GameState -> GameState.GameState
witnessing logged gs = gs {GameState.turn = (GameState.turn gs) {TurnRegisters.events = Seq.fromList logged}}

inOneGroup :: [GameEvent.GameEvent] -> [LoggedEvent.LoggedEvent]
inOneGroup = fmap (LoggedEvent.MkLoggedEvent (EventGroup.MkEventGroup 0))

-- Every entry the view says its seat witnessed, as sentences.
told :: PlayerView.Type.PlayerView -> [Text.Text]
told view = Wording.events Nothing view (Wording.witnessed view)

otherSeat :: Natural -> Natural
otherSeat n = if n == 1 then 2 else 1

-- A seat asked who plays first: itself, or the next seat.
plays, draws :: Natural -> NonEmpty.NonEmpty Natural -> Maybe Natural
plays chooser _ = Just chooser
draws chooser _ = Just (otherSeat chooser)

-- What Game.starter told, and each seat it asked, in order; and the answers
-- not yet given.
data Heard = MkHeard {said :: [Text.Text], asks :: [Natural], answers :: [Natural -> NonEmpty.NonEmpty Natural -> Maybe Natural]}

-- Game.starter from this seed between two seats, the seat asked answering
-- each of these in turn and nothing after: the seat that plays first and the
-- generator after, and what was heard.
starting :: PlayOptions.PlayOptions -> [Natural -> NonEmpty.NonEmpty Natural -> Maybe Natural] -> Word.Word32 -> ((Maybe Natural, Generator.Generator), Heard)
starting = startingAmong (1 NonEmpty.:| [2])

-- 'starting' among these seats.
startingAmong :: NonEmpty.NonEmpty Natural -> PlayOptions.PlayOptions -> [Natural -> NonEmpty.NonEmpty Natural -> Maybe Natural] -> Word.Word32 -> ((Maybe Natural, Generator.Generator), Heard)
startingAmong seats options given seed =
  State.runState (State.runStateT (Game.starter tell ask seats options) (S.seeded seed)) (MkHeard [] [] given)
  where
    tell line = State.modify' (\h -> h {said = said h <> [line]})
    ask chooser among = do
      h <- State.get
      case answers h of
        [] -> State.put h {asks = asks h <> [chooser]} >> pure Nothing
        next : rest -> State.put h {asks = asks h <> [chooser], answers = rest} >> pure (next chooser among)

-- The seats told they were picked to choose who plays first.
pickedIn :: Heard -> [Natural]
pickedIn heard = [n | n <- [1, 2, 3], Text.pack ("Seat " <> show n <> " was picked at random to choose who plays first.") `elem` said heard]

-- | Command.game, which runs in IO.
gameSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
gameSpec s registry = Spec.describe s "Play.Command" $ do
  Spec.it s "a game's saved scenario names what it was played with" $ do
    let seed = Generator.key (S.seeded 7)
        deck = Deck.fromCards Map.empty
    (_, recorded) <- Command.game (Text.pack "the web client") (const (pure Nothing)) (pure Nothing) (const (pure ())) [] (pure ()) PlayOptions.none Search.known (S.fixtureRegistry []) GameSettings.plain (deck NonEmpty.:| [deck]) seed
    Spec.assertEqWith s "description" (Scenario.description recorded) (Text.pack "Played with the web client, seed " <> Key.toHex seed <> Text.pack ".")

  Spec.it s "CR 104.3a / 104.2a a concession while the other seat decides ends a game of two there, its recording replaying to it" $ do
    matchup <- BotSpec.bears s registry
    let seed = Generator.key (S.seeded 7)
        other seat = if seat == S.alice then S.bob else S.alice
    askedRef <- IORef.newIORef Nothing
    -- The first seat asked a question of the game is left deciding while
    -- the other concedes.
    let exchange request = case Request.view request of
          Nothing -> pure (Just (Right (R.Text (Text.pack "Play"))))
          Just _ -> do
            let seat = Question.seat (Request.question request)
            IORef.writeIORef askedRef (Just seat)
            Exception.throwIO (Command.MkConceded (other seat))
    (outcome, recorded) <- Command.game (Text.pack "the web client") exchange (pure Nothing) (const (pure ())) [] (pure ()) PlayOptions.none Search.known (S.fixtureRegistry []) GameSettings.plain (fmap snd matchup) seed
    asked <- IORef.readIORef askedRef
    let timeline = Scenario.timeline recorded
        final = Seq.lookup (Seq.length timeline - 1) timeline
    Spec.assertEqWith s "the game stops" (fmap fst outcome) (Left (Text.pack "conceded"))
    Spec.assertEqWith s "its recording ends on the other seat's concession" (fmap (\t -> (When.player (Timed.when t), Timed.source t, Timed.entry t)) final) (fmap (\seat -> (Map.findWithDefault (Label.MkLabel Text.empty) (other seat) (Command.labels 2), Nothing, Entry.Do Move.Concede)) asked)
    (_, replayed) <- Scenario.Runner.replay registry Scenario.Runner.unkept Nothing recorded
    case replayed of
      Right (Stop.MkEnded _) -> pure ()
      Right Stop.MkBefore {} -> Spec.assertBool s False "the replay stopped short"
      Left failure -> Spec.assertBool s False ("the replay takes the concession where it was made: " <> Text.unpack (Scenario.Runner.render failure))

  Spec.it s "the goldfish answers each seat the options give it, the exchange asked nothing, and a game played on from its recording's start ends as it did" $ do
    let seed = Generator.key (S.seeded 7)
        deck = Deck.fromCards Map.empty
        options = PlayOptions.none {PlayOptions.first = Just 1, PlayOptions.goldfish = Set.fromList [1, 2]}
    askedRef <- IORef.newIORef (0 :: Int)
    let exchange _ = Nothing <$ IORef.modifyIORef' askedRef (+ 1)
    (outcome, recorded) <- Command.game (Text.pack "the web client") exchange (pure Nothing) (const (pure ())) [] (pure ()) options Search.known (S.fixtureRegistry []) GameSettings.plain (deck NonEmpty.:| [deck]) seed
    branched <- Command.branch exchange (pure Nothing) (const (pure ())) [] (pure ()) options Search.known (S.fixtureRegistry []) recorded 0 seed
    asked <- IORef.readIORef askedRef
    Spec.assertEqWith s "the exchange asked" asked 0
    Spec.assertBool s (Either.isRight outcome) ("the game has a result: " <> show (fmap fst outcome))
    Spec.assertEqWith s "the game played on's result" (fmap fst branched) (fmap fst outcome)

  Spec.it s "CR 104.3a a concession while a game played on from a position is pending ends it there" $ do
    let seed = Generator.key (S.seeded 7)
        deck = Deck.fromCards Map.empty
        options = PlayOptions.none {PlayOptions.first = Just 1, PlayOptions.goldfish = Set.fromList [1, 2]}
    (_, recorded) <- Command.game (Text.pack "the web client") (const (pure Nothing)) (pure Nothing) (const (pure ())) [] (pure ()) options Search.known (S.fixtureRegistry []) GameSettings.plain (deck NonEmpty.:| [deck]) seed
    let exchange request = Exception.throwIO (Command.MkConceded (Question.seat (Request.question request)))
    branched <- Command.branch exchange (pure Nothing) (const (pure ())) [] (pure ()) PlayOptions.none {PlayOptions.goldfish = Set.singleton 1} Search.known (S.fixtureRegistry []) recorded 0 seed
    Spec.assertEqWith s "the game stops" (fmap fst branched) (Left (Text.pack "conceded"))

  Spec.it s "a bot's decision clock runs out once its think limit has passed, and never at 0" $ do
    let asked limit = do
          expired <- Command.timed limit
          before <- expired
          Game.underneath (Clock.getMonotonicTimeNSec >>= waitPast)
          after <- expired
          pure (before, after)
        -- Until 20 milliseconds have passed since `start`.
        waitPast start = Clock.getMonotonicTimeNSec >>= \now -> Monad.when (now - start < 20000000) (waitPast start)
        run limit = Except.runExceptT (State.evalStateT (asked limit) (S.seeded 1))
    unlimited <- run 0
    Spec.assertEqWith s "at 0, never out" unlimited (Right (False, False))
    limited <- run 10
    Spec.assertEqWith s "at 10 milliseconds, out 20 milliseconds on" (fmap snd limited) (Right True)
