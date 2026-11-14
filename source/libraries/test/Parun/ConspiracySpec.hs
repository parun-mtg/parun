{-# LANGUAGE GADTs #-}

-- Covers Parun.Engine.Conspiracy and the CR 315 readings its callers make:
-- Parun.Engine.Setup's CR 315.2 placement and CR 315.3 restart hold,
-- Parun.Engine.Vanguard.functionsFromCommandZone's conspiracy arm (CR 113.6p /
-- 315.5), and CR 702.106's hidden agenda. Parun.OutsideTheGameSpec's Ring of Ma'rûf pair covers CR 315.3's last
-- sentence.
--
-- Sentinel Dispatch (Conspiracy, no mana cost; "(Start the game with this
-- conspiracy face up in the command zone.) At the beginning of the first upkeep,
-- create a 1/1 colorless Construct artifact creature token with defender." --
-- name, type line and Oracle text checked against api.scryfall.com 2026-09-22,
-- paper printing `cns`) is the producer. Its "the first upkeep" is transcribed
-- as an each-upkeep trigger that triggers only once: CR 315.3 keeps the card in
-- the command zone from before the first upkeep to the end of the game, so its
-- first triggering is at the game's first upkeep.
--
-- Power Play (Conspiracy, no mana cost; "(Start the game with this conspiracy
-- face up in the command zone.) You are the starting player. If multiple players
-- would be the starting player, one of those players is chosen at random." --
-- checked against api.scryfall.com 2026-09-29, paper printing `cns`) is CR
-- 103.1c's card, and Parun.Engine.Setup.claimStartingPlayer its reader.
--
-- Backup Plan (Conspiracy, no mana cost; "(Start the game with this conspiracy
-- face up in the command zone.) Draw an additional hand of seven cards as the
-- game begins. Before taking mulligans, shuffle all but one of your hands into
-- your library." -- checked against api.scryfall.com 2026-11-13, paper printing
-- `cns`) is CR 315.5a's card, and Parun.Engine.Mulligan.additionalHands its
-- reader.
--
-- Immediate Action (Conspiracy, no mana cost; "Hidden agenda (Start the game
-- with this conspiracy face down in the command zone and secretly choose a card
-- name. You may turn this conspiracy face up any time and reveal that name.)
-- Creatures you control with the chosen name have haste." -- checked against
-- api.scryfall.com 2026-10-11, paper printing `cns`) is CR 702.106's card.
--
-- Summoner's Bond (Conspiracy, no mana cost; "Double agenda (Start the game
-- with this conspiracy face down in the command zone and secretly choose two
-- different card names. You may turn this conspiracy face up any time and
-- reveal those names.) Whenever you cast a creature spell with one of the chosen
-- names, you may search your library for a creature card with the other chosen
-- name, reveal it, put it into your hand, then shuffle." -- checked against
-- api.scryfall.com 2026-11-14, paper printing `cn2`) is CR 702.106f's card.
module Parun.ConspiracySpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Conspiracy as Conspiracy
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Mulligan as Mulligan
import qualified Parun.Engine.Projection as Projection.Full
import qualified Parun.Engine.Projection.View as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Action as Action.Type
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.MulliganDecision as MulliganDecision
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.RestartSignal as RestartSignal
import qualified Parun.Types.Revealed as Revealed
import qualified Parun.Types.Sickness as Sickness
import qualified Parun.Types.Status as Status
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Zone as Zone

-- Thirty Mountains, plus whichever conspiracies the leg brings.
deckOf :: Printing.Printing -> [Printing.Printing] -> Deck.Deck
deckOf mountain conspiracies =
  (Deck.fromCards (Map.singleton mountain 30))
    { Deck.conspiracies = Map.fromListWith (+) (fmap (\p -> (p, 1)) conspiracies)
    }

-- BOB brings the conspiracies and alice, the starting player, brings none: the
-- game's first upkeep is then alice's, so a trigger that fires there is not
-- reading "your first upkeep". Both libraries are stocked for the opening draws
-- (CR 104.3c).
built :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
built mountain conspiracies =
  S.runPure keepAnswer (Setup.emptyGame S.bothPlayers) $ do
    Setup.createDeck S.alice (deckOf mountain [])
    Setup.createDeck S.bob (deckOf mountain conspiracies)
    Mulligan.openingHands S.performer [S.alice, S.bob]

keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

zoneNames :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
zoneNames zone pid gs = fmap (\oid -> maybe (CardName.MkCardName (Text.pack "?")) S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- What a whole-setup run asked: who declared a mulligan, in order, and the
-- candidates of each CR 103.1c random draw.
data Asked
  = DeclaredBy PlayerId.PlayerId
  | DrewFrom [PlayerId.PlayerId]
  | NamedBy PlayerId.PlayerId ObjectId
  | KeptAmong PlayerId.PlayerId [[ObjectId]]
  | Shuffled Int
  deriving (Eq, Show)

-- The name every CR 702.106a choice below is answered with.
bears :: CardName.CardName
bears = CardName.MkCardName (Text.pack "Grizzly Bears")

-- Keeps every hand and takes every offered CR 103.6 action, recording each ask.
-- A CR 103.1c draw is answered with the candidate at index `pick`, pinned by
-- position so the answer cannot go looking for the right player. A CR 702.106a
-- name is Grizzly Bears, which is not the answerer's default.
recording :: Int -> Prompt.Prompt r -> State.State [Asked] r
recording pick p = case p of
  Prompt.DeclareMulligan _ pid _ -> do
    State.modify' (DeclaredBy pid :)
    pure MulliganDecision.Keep
  Prompt.ChooseCardName _ pid oid _ _ -> do
    State.modify' (NamedBy pid oid :)
    pure bears
  Prompt.OpeningHandAction _ _ candidates -> pure (Maybe.listToMaybe candidates)
  Prompt.RandomFirstPlayer candidates -> do
    State.modify' (DrewFrom (NonEmpty.toList candidates) :)
    pure (Maybe.fromMaybe (NonEmpty.head candidates) (Maybe.listToMaybe (drop pick (NonEmpty.toList candidates))))
  _ -> pure (S.identityAnswer p)

-- `recording 0`, but keeping the hand at index `keep` of those CR 315.5a
-- offers, and recording that ask and the size of every shuffle after it.
keeping :: Natural -> Prompt.Prompt r -> State.State [Asked] r
keeping keep p = case p of
  Prompt.KeepHand _ pid hands -> do
    State.modify' (KeptAmong pid (NonEmpty.toList hands) :)
    pure keep
  Prompt.Shuffle cards -> do
    State.modify' (\asked -> if any isKept asked then Shuffled (length cards) : asked else asked)
    pure cards
  _ -> recording 0 p
  where
    isKept asked = case asked of
      KeptAmong {} -> True
      _ -> False

-- `started`, answered by `keeping keep`.
startedKeeping :: Natural -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [Asked])
startedKeeping keep matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame (keeping keep) (Setup.emptyGame (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

-- The whole of Setup.newGame over this matchup, seated in its order -- so its
-- head is CR 103.1's determination -- with what `recording pick` was asked.
started :: Int -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [Asked])
started pick matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame (recording pick) (Setup.emptyGame (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

declarers :: [Asked] -> [PlayerId.PlayerId]
declarers asked = [pid | DeclaredBy pid <- asked]

draws :: [Asked] -> [[PlayerId.PlayerId]]
draws asked = [pids | DrewFrom pids <- asked]

-- Run the upkeep of the given turn, with the given player active.
upkeep :: PlayerId.PlayerId -> Natural -> GameState.GameState -> GameState.GameState
upkeep active turn gs =
  S.runPure
    S.identityAnswer
    ( gs
        { GameState.turn =
            (GameState.turn gs)
              { TurnRegisters.activePlayer = active,
                TurnRegisters.phase = Phase.Beginning BeginningStep.Upkeep,
                TurnRegisters.turnNumber = turn
              },
          GameState.restartSignal = RestartSignal.Playing
        }
    )
    Engine.runStep

-- The Construct tokens on the battlefield, by who CONTROLS them (CR 110.2).
constructsControlledBy :: PlayerId.PlayerId -> GameState.GameState -> Int
constructsControlledBy pid gs =
  length
    [ oid
    | oid <- Set.toList (GameState.battlefield gs),
      fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Construct Token")),
      Projection.controllerOf oid gs == Just pid
    ]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Conspiracy" $ do
  -- CR 315.2 / 315.6: the conspiracy starts face up in its owner's command zone
  -- and is not one of the deck's cards.
  Spec.it s "CR 315.2 a conspiracy begins the game face up in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let with = built mountain [dispatch]
    Spec.assertEqWith s "the conspiracy is in bob's command zone" (zoneNames Zone.Command S.bob with) [CardName.MkCardName (Text.pack "Sentinel Dispatch")]
    Spec.assertEqWith s "face up" (fmap (\oid -> fmap Object.facing (Game.lookupObject oid with)) (Game.zoneMembers Zone.Command S.bob with)) [Just Facing.FaceUp]
    Spec.assertEqWith s "and not among the thirty cards of his library and hand" (length (Game.zoneMembers Zone.Library S.bob with) + length (Game.zoneMembers Zone.Hand S.bob with)) 30
    Spec.assertEqWith s "alice, who brought none, has an empty command zone" (Game.zoneMembers Zone.Command S.alice with) []

  -- CR 315.5 / 113.6p: the conspiracy's triggered ability triggers from the
  -- command zone, at the game's first upkeep -- alice's, not bob's -- and at no
  -- upkeep after it. The control leg differs only in the conspiracy.
  Spec.it s "CR 315.5 Sentinel Dispatch creates its Construct at the first upkeep and never again" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let first = upkeep S.alice 1 (built mountain [dispatch])
        second = upkeep S.bob 2 first
        control = upkeep S.alice 1 (built mountain [])
    Spec.assertEqWith s "at alice's first upkeep, bob gets a Construct" (constructsControlledBy S.bob first) 1
    Spec.assertEqWith s "and at bob's own upkeep after it, no second one" (constructsControlledBy S.bob second) 1
    Spec.assertEqWith s "without the conspiracy, no Construct" (constructsControlledBy S.bob control) 0
    Spec.assertEqWith s "and alice, who has none, gets none" (constructsControlledBy S.alice second) 0

  -- CR 315.3 against CR 728.2: a restart puts every card into its owner's new
  -- library, and the conspiracy stays in the command zone -- where the new game's
  -- first upkeep triggers it again.
  Spec.it s "CR 315.3 / 728.1 a restarted game keeps the conspiracy in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let played = upkeep S.bob 2 (upkeep S.alice 1 (built mountain [dispatch]))
        restarted = S.runPure keepAnswer played (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "still in bob's command zone" (zoneNames Zone.Command S.bob restarted) [CardName.MkCardName (Text.pack "Sentinel Dispatch")]
    Spec.assertEqWith s "not shuffled into his thirty-card library" (length (Game.zoneMembers Zone.Library S.bob restarted) + length (Game.zoneMembers Zone.Hand S.bob restarted)) 30
    Spec.assertEqWith s "and the new game's first upkeep makes a Construct again" (constructsControlledBy S.bob (upkeep S.alice 1 restarted)) 1

  -- CR 103.1c: alice is seated first, so CR 103.1 makes her the starting player
  -- -- until bob's Power Play supersedes it. Gemstone Caverns is the observer:
  -- "if ... you're not the starting player" reads Quantity.IsStartingPlayer. Both
  -- decks are all Caverns, so the two seats differ only in the conspiracy, and
  -- the control leg differs from this one only in that.
  Spec.describe s "Power Play" $ do
    Spec.it s "CR 103.1c Power Play makes its controller the starting player" $ do
      caverns <- S.printingOf s registry "Gemstone Caverns"
      powerPlay <- S.printingOf s registry "Power Play"
      let leg conspiracies = started 0 ((S.alice, deckOf caverns []) NonEmpty.:| [(S.bob, deckOf caverns conspiracies)])
          (with, asked) = leg [powerPlay]
          (control, controlAsked) = leg []
          begunWith pid gs = not (null (Game.zoneMembers Zone.Battlefield pid gs))
      Spec.assertEqWith s "alice, no longer the starting player, begins with her Caverns" (begunWith S.alice with) True
      Spec.assertEqWith s "bob, now the starting player, does not" (begunWith S.bob with) False
      Spec.assertEqWith s "without Power Play, alice starts and has none" (begunWith S.alice control) False
      Spec.assertEqWith s "and bob has his" (begunWith S.bob control) True
      Spec.assertEqWith s "bob takes the first turn" (TurnRegisters.activePlayer (GameState.turn with)) S.bob
      Spec.assertEqWith s "the turn order begins with him" (GameState.seating with) [S.bob, S.alice]
      -- CR 500.7: Serra Avenger's count of turns of the game.
      Spec.assertEqWith s "and the first turn is his first turn of the game, not alice's" (GameState.turnsBegun with) (Map.singleton S.bob 1)
      -- CR 103.5: the starting player declares first.
      Spec.assertEqWith s "bob declares his mulligan first" (declarers asked) [S.bob, S.alice]
      Spec.assertEqWith s "the control leg is untouched" (declarers controlAsked) [S.alice, S.bob]
      -- One claimant is no draw.
      Spec.assertEqWith s "nothing is drawn at random" (draws asked) []

    -- Power Play's second sentence, and its 2014-05-29 ruling: the draw is over
    -- exactly the two claimants, and the order is rotated, not reseated, so the
    -- loser keeps their place. Two logs, one landing on each claimant; three
    -- seats so the non-claimant is a third player the draw must leave out.
    Spec.it s "CR 103.1c two Power Plays: one claimant is drawn at random" $ do
      mountain <- S.printingOf s registry "Mountain"
      powerPlay <- S.printingOf s registry "Power Play"
      let matchup = (S.alice, deckOf mountain []) NonEmpty.:| [(S.bob, deckOf mountain [powerPlay]), (S.carol, deckOf mountain [powerPlay])]
          (toCarol, carolAsked) = started 1 matchup
          (toBob, bobAsked) = started 0 matchup
      Spec.assertEqWith s "landing on carol, she starts and bob goes last" (GameState.seating toCarol) [S.carol, S.alice, S.bob]
      Spec.assertEqWith s "landing on bob, he starts and alice goes last" (GameState.seating toBob) [S.bob, S.carol, S.alice]
      Spec.assertEqWith s "carol takes the first turn" (TurnRegisters.activePlayer (GameState.turn toCarol)) S.carol
      Spec.assertEqWith s "one draw, over bob and carol alone" (draws carolAsked) [[S.bob, S.carol]]
      Spec.assertEqWith s "the same draw in the other log" (draws bobAsked) [[S.bob, S.carol]]

    -- CR 728.1a then CR 103.1c: alice restarts the game, and bob's Power Play,
    -- still in his command zone (CR 315.3), supersedes her claim.
    Spec.it s "CR 103.1c / 728.1a Power Play supersedes a restart's starting player" $ do
      mountain <- S.printingOf s registry "Mountain"
      powerPlay <- S.printingOf s registry "Power Play"
      let restart conspiracies = S.runPure keepAnswer (built mountain conspiracies) (Setup.restartGame S.performer Set.empty S.alice)
      Spec.assertEqWith s "with Power Play, bob starts the new game" (GameState.seating (restart [powerPlay])) [S.bob, S.alice]
      Spec.assertEqWith s "its first turn his first turn of the game (CR 500.7)" (GameState.turnsBegun (restart [powerPlay])) (Map.singleton S.bob 1)
      Spec.assertEqWith s "without it, alice does" (GameState.seating (restart [])) [S.alice, S.bob]

  -- CR 315.5a: alice brings Backup Plan over thirty Mountains, and bob brings
  -- nothing. Every board is the whole of Setup.newGame.
  Spec.describe s "Backup Plan" $ do
    Spec.it s "CR 315.5a Backup Plan draws a second hand of seven and keeps the one chosen" $ do
      mountain <- S.printingOf s registry "Mountain"
      plan <- S.printingOf s registry "Backup Plan"
      let leg keep = startedKeeping keep ((S.alice, deckOf mountain [plan]) NonEmpty.:| [(S.bob, deckOf mountain [])])
          (second, asked) = leg 1
          (first, _) = leg 0
      case asked of
        [KeptAmong pid [one, two], Shuffled shuffled, DeclaredBy a, DeclaredBy b] -> do
          Spec.assertEqWith s "alice is the one asked" pid S.alice
          Spec.assertEqWith s "of two hands of seven" (fmap length [one, two]) [7, 7]
          Spec.assertEqWith s "sharing no card" (List.intersect one two) []
          Spec.assertEqWith s "keeping the second, her hand is the second" (Game.zoneMembers Zone.Hand S.alice second) two
          Spec.assertEqWith s "keeping the first, it is the first" (Game.zoneMembers Zone.Hand S.alice first) one
          Spec.assertEqWith s "the other hand is shuffled into her library" shuffled 23
          Spec.assertEqWith s "which holds every other card" (length (Game.zoneMembers Zone.Library S.alice second)) 23
          Spec.assertEqWith s "all before any mulligan is declared (CR 103.5)" [a, b] [S.alice, S.bob]
        _ -> Spec.assertFailure s ("setup: asked " <> show asked)
      Spec.assertEqWith s "bob, without one, draws one hand" (length (Game.zoneMembers Zone.Hand S.bob second), length (Game.zoneMembers Zone.Library S.bob second)) (7, 23)

    -- Backup Plan's 2014-05-29 ruling: the effect is cumulative.
    Spec.it s "CR 315.5a two Backup Plans draw three hands and keep one" $ do
      mountain <- S.printingOf s registry "Mountain"
      plan <- S.printingOf s registry "Backup Plan"
      let (gs, asked) = startedKeeping 2 ((S.alice, deckOf mountain [plan, plan]) NonEmpty.:| [(S.bob, deckOf mountain [])])
      case asked of
        KeptAmong _ hands@[_, _, third] : Shuffled shuffled : _ -> do
          Spec.assertEqWith s "three hands of seven" (fmap length hands) [7, 7, 7]
          Spec.assertEqWith s "she keeps the third" (Game.zoneMembers Zone.Hand S.alice gs) third
          Spec.assertEqWith s "and the other two are shuffled in" shuffled 23
        _ -> Spec.assertFailure s ("setup: asked " <> show asked)

    -- CR 313.6: Gerrard's hand modifier is -4, so alice's starting hand is three
    -- cards, and the additional hand is still the seven Backup Plan prints.
    Spec.it s "CR 315.5a the additional hand is seven cards whatever the starting hand size" $ do
      mountain <- S.printingOf s registry "Mountain"
      plan <- S.printingOf s registry "Backup Plan"
      gerrard <- S.printingOf s registry "Gerrard"
      let (_, asked) = startedKeeping 0 ((S.alice, (deckOf mountain [plan]) {Deck.vanguard = Just gerrard}) NonEmpty.:| [(S.bob, deckOf mountain [])])
      Spec.assertEqWith s "a hand of three and a hand of seven" [fmap length hands | KeptAmong _ hands <- asked] [[3, 7]]

    -- CR 704.5b, and Backup Plan's 2014-05-29 ruling: ten cards cannot fill two
    -- hands of seven.
    Spec.it s "CR 704.5b a library too short for the additional hand loses the game" $ do
      mountain <- S.printingOf s registry "Mountain"
      plan <- S.printingOf s registry "Backup Plan"
      let short = (Deck.fromCards (Map.singleton mountain 10)) {Deck.conspiracies = Map.singleton plan 1}
          (gs, asked) = startedKeeping 0 ((S.alice, short) NonEmpty.:| [(S.bob, deckOf mountain [])])
      Spec.assertEqWith s "the second hand is the three cards left" [fmap length hands | KeptAmong _ hands <- asked] [[7, 3]]
      Spec.assertEqWith s "alice drew from an empty library" (GameState.drewFromEmpty gs) (Set.singleton S.alice)
      Spec.assertEqWith s "and loses when state-based actions are checked" (fmap Player.status (Map.lookup S.alice (GameState.players (S.settleSba gs)))) (Just (Status.Departed Departure.Type.Lost))

  -- CR 702.106: alice brings Immediate Action and names Grizzly Bears as it goes
  -- into her command zone. Every board is the whole of Setup.newGame.
  Spec.describe s "Immediate Action" $ do
    Spec.it s "CR 315.2 / 702.106a hidden agenda puts it face down and asks its name before any mulligan" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      let (gs, asked) = started 0 ((S.alice, deckOf mountain [action]) NonEmpty.:| [(S.bob, deckOf mountain [])])
      case commandCard S.alice gs of
        Nothing -> Spec.assertFailure s "setup: Immediate Action is not alone in alice's command zone"
        Just agenda -> do
          Spec.assertEqWith s "CR 702.106a alice names a card for it before the first mulligan declaration (CR 315.2, 103.5)" asked [NamedBy S.alice agenda, DeclaredBy S.alice, DeclaredBy S.bob]
          Spec.assertEqWith s "it lies face down" (isFaceDown agenda gs) True
          Spec.assertEqWith s "with her name noted (CR 702.106b)" (fmap (Map.findWithDefault Map.empty Acquisition.Own . Object.chosenNames) (Game.lookupObject agenda gs)) (Just (Map.singleton S.alice (Set.singleton bears)))
          let pc = Projection.Full.project agenda gs
          Spec.assertEqWith s "CR 315.5b it has no name" (PC.names pc) Set.empty
          Spec.assertEqWith s "and no card type" (PC.cardTypes pc) Set.empty
          Spec.assertEqWith s "CR 315.7 alice may look at it" (Visibility.sees S.alice agenda gs) Visibility.Type.Visible
          Spec.assertEqWith s "and bob may not" (Visibility.sees S.bob agenda gs) Visibility.Type.Obscured
          Spec.assertBool s (elem (Action.Type.TurnConspiracyFaceUp agenda) (Action.legalActions S.alice gs)) "CR 116.2j alice may turn it face up"
          Spec.assertBool s (notElem (Action.Type.TurnConspiracyFaceUp agenda) (Action.legalActions S.bob gs)) "and bob may not"

    -- The plan's proof: a Grizzly Bears and a Runeclaw Bear arrive under alice
    -- this turn, and she attacks with everything that can. The two legs differ
    -- only in whether she takes the special action first, and bob's life says
    -- which creatures attacked.
    Spec.it s "CR 702.106c turned face up, it gives the named creature haste" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      grizzly <- S.printingOf s registry "Grizzly Bears"
      runeclaw <- S.printingOf s registry "Runeclaw Bear"
      let (setUp, _) = started 0 ((S.alice, deckOf mountain [action]) NonEmpty.:| [(S.bob, deckOf mountain [])])
          board = arriving runeclaw S.alice (arriving grizzly S.alice setUp)
          main =
            board
              { GameState.turn =
                  (GameState.turn board)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice,
                      TurnRegisters.passed = Set.empty
                    },
                GameState.restartSignal = RestartSignal.Playing
              }
          flipped = withFlip main
          declined = S.runPure S.identityAnswer main Engine.priorityLoop
      case commandCard S.alice setUp of
        Nothing -> Spec.assertFailure s "setup: Immediate Action is not alone in alice's command zone"
        Just agenda -> do
          Spec.assertEqWith s "CR 702.10b turned face up, it lets the Grizzly Bears attack the turn they arrive, and not the Runeclaw Bear" (S.lifeOf S.bob (attackingWithAll flipped)) (Just 18)
          Spec.assertEqWith s "CR 315.5 left face down, it gives nothing haste" (S.lifeOf S.bob (attackingWithAll declined)) (Just 20)
          Spec.assertEqWith s "the action turned it face up" (isFaceDown agenda flipped) False
          Spec.assertEqWith s "CR 702.106c revealing it to bob" (Visibility.sees S.bob agenda flipped) Visibility.Type.Visible

    -- CR 702.106e's first sentence, on three seats so the game goes on. The two
    -- legs differ only in whether carol turned it face up before she conceded.
    Spec.it s "CR 702.106e a face-down conspiracy is revealed when its owner leaves the game" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      let (gs, _) = started 0 ((S.alice, deckOf mountain []) NonEmpty.:| [(S.bob, deckOf mountain []), (S.carol, deckOf mountain [action])])
          concede board = S.runPure S.identityAnswer board (Departure.leaveGame Departure.Type.Conceded S.carol)
          revealsIn board = [(Revealed.player r, Revealed.card r) | GameEvent.Revealed r <- S.eventsOf board]
      case commandCard S.carol gs of
        Nothing -> Spec.assertFailure s "setup: Immediate Action is not alone in carol's command zone"
        Just agenda -> do
          let up = S.runPure S.identityAnswer gs (Conspiracy.turnFaceUp S.carol agenda)
          Spec.assertEqWith s "carol reveals it as she leaves" (revealsIn (concede gs)) [(S.carol, agenda)]
          Spec.assertEqWith s "and had she turned it face up, there is nothing to reveal" (revealsIn (concede up)) []

    -- CR 702.106e's second sentence: on two seats bob's concession ends the
    -- game, and alice's face-down conspiracy is revealed with it.
    Spec.it s "CR 702.106e a face-down conspiracy is revealed at the end of the game" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      let (gs, _) = started 0 ((S.alice, deckOf mountain [action]) NonEmpty.:| [(S.bob, deckOf mountain [])])
          ended board = S.runPure S.identityAnswer board (Departure.leaveGame Departure.Type.Conceded S.bob)
          revealsIn board = [(Revealed.player r, Revealed.card r) | GameEvent.Revealed r <- S.eventsOf board]
      case commandCard S.alice gs of
        Nothing -> Spec.assertFailure s "setup: Immediate Action is not alone in alice's command zone"
        Just agenda -> do
          let up = S.runPure S.identityAnswer gs (Conspiracy.turnFaceUp S.alice agenda)
          Spec.assertEqWith s "alice reveals it as the game ends" (revealsIn (ended gs)) [(S.alice, agenda)]
          Spec.assertEqWith s "and had she turned it face up, there is nothing to reveal" (revealsIn (ended up)) []

    -- CR 728.1 / 103.2e / 315.2: the new game places its conspiracies again,
    -- so hidden agenda turns the card face down and asks again -- even one alice
    -- turned face up in the restarted game, since nothing done there carries
    -- over (Karn Liberated's ruling; Immediate Action's "you can
    -- name a different card in each new game").
    Spec.it s "CR 728.1 / 702.106a a restarted game puts a hidden agenda face down again and asks a fresh name" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      let (gs, _) = started 0 ((S.alice, deckOf mountain [action]) NonEmpty.:| [(S.bob, deckOf mountain [])])
      case commandCard S.alice gs of
        Nothing -> Spec.assertFailure s "setup: Immediate Action is not alone in alice's command zone"
        Just agenda -> do
          let up = S.runPure S.identityAnswer gs (Conspiracy.turnFaceUp S.alice agenda)
              restarted = S.runPure renaming up (Setup.restartGame S.performer Set.empty S.alice)
          Spec.assertEqWith s "setup: alice turned it face up before the restart" (isFaceDown agenda up) False
          Spec.assertEqWith s "it is face down again" (isFaceDown agenda restarted) True
          Spec.assertEqWith s "naming the new game's choice, Runeclaw Bear" (fmap (Map.findWithDefault Map.empty Acquisition.Own . Object.chosenNames) (Game.lookupObject agenda restarted)) (Just (Map.singleton S.alice (Set.singleton runeclawName)))

    -- CR 613.7j: turned face up, the conspiracy takes a new timestamp, so its
    -- grant is later than a Humility that entered after the game began, and the
    -- Grizzly Bears (a 1/1 under Humility) keep the haste it grants.
    Spec.it s "CR 613.7j turned face up, its haste grant is later than an earlier Humility" $ do
      mountain <- S.printingOf s registry "Mountain"
      action <- S.printingOf s registry "Immediate Action"
      grizzly <- S.printingOf s registry "Grizzly Bears"
      humility <- S.printingOf s registry "Humility"
      let (setUp, _) = started 0 ((S.alice, deckOf mountain [action]) NonEmpty.:| [(S.bob, deckOf mountain [])])
          board = arriving grizzly S.alice (snd (S.addPermanent humility S.bob setUp))
          main =
            board
              { GameState.turn =
                  (GameState.turn board)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice,
                      TurnRegisters.passed = Set.empty
                    },
                GameState.restartSignal = RestartSignal.Playing
              }
      Spec.assertEqWith s "CR 613.7j the 1/1 Grizzly Bears attack the turn they arrive" (S.lifeOf S.bob (attackingWithAll (withFlip main))) (Just 19)
      Spec.assertEqWith s "and without the action they do not" (S.lifeOf S.bob (attackingWithAll (S.runPure S.identityAnswer main Engine.priorityLoop))) (Just 20)

  -- CR 702.106f: alice brings Summoner's Bond and names Grizzly Bears, then
  -- Runeclaw Bear, as it goes into her command zone. Every board is the whole
  -- of Setup.newGame, with Forests for her deck.
  Spec.describe s "Summoner's Bond" $ do
    Spec.it s "CR 702.106f double agenda asks for two different names before any mulligan" $ do
      forest <- S.printingOf s registry "Forest"
      bond <- S.printingOf s registry "Summoner's Bond"
      let (gs, restrictions) = bonded forest bond
      case commandCard S.alice gs of
        Nothing -> Spec.assertFailure s "setup: Summoner's Bond is not alone in alice's command zone"
        Just agenda -> do
          Spec.assertEqWith s "it lies face down" (isFaceDown agenda gs) True
          Spec.assertEqWith s "alice was asked twice, the second time refusing her first name" restrictions [Filter.And [], Filter.And [Filter.Not (Filter.HasName bears)]]
          Spec.assertEqWith s "with both names noted under her (CR 702.106b)" (fmap (Map.findWithDefault Map.empty Acquisition.Own . Object.chosenNames) (Game.lookupObject agenda gs)) (Just (Map.singleton S.alice (Set.fromList [bears, runeclawName])))

    -- The plan's proof. Alice's library holds a Grizzly Bears, a Runeclaw Bear
    -- and a Llanowar Elves; she casts one creature from her hand, and the search
    -- offers only a card with the other chosen name.
    Spec.it s "CR 702.106f casting a creature with one chosen name finds a creature with the other" $ do
      forest <- S.printingOf s registry "Forest"
      bond <- S.printingOf s registry "Summoner's Bond"
      grizzly <- S.printingOf s registry "Grizzly Bears"
      runeclaw <- S.printingOf s registry "Runeclaw Bear"
      elves <- S.printingOf s registry "Llanowar Elves"
      let (setUp, _) = bonded forest bond
          (libGrizzly, g1) = S.addObjectIn Zone.Library grizzly S.alice setUp
          (libRuneclaw, g2) = S.addObjectIn Zone.Library runeclaw S.alice g1
          (_, g3) = S.addObjectIn Zone.Library elves S.alice g2
          (_, g4) = S.addPermanent forest S.alice g3
          (_, g5) = S.addPermanent forest S.alice g4
          (handGrizzly, g6) = S.addObjectIn Zone.Hand grizzly S.alice g5
          (handRuneclaw, g7) = S.addObjectIn Zone.Hand runeclaw S.alice g6
          (handElves, board) = S.addObjectIn Zone.Hand elves S.alice g7
          main =
            board
              { GameState.turn =
                  (GameState.turn board)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice,
                      TurnRegisters.passed = Set.empty
                    },
                GameState.restartSignal = RestartSignal.Playing
              }
      case commandCard S.alice setUp of
        Nothing -> Spec.assertFailure s "setup: Summoner's Bond is not alone in alice's command zone"
        Just agenda -> do
          let up = S.runPure S.identityAnswer main (Conspiracy.turnFaceUp S.alice agenda)
              -- CR 400.7: the card found is a new object in her hand.
              inHand name gs = length (filter (== name) (zoneNames Zone.Hand S.alice gs))
              (searchedGrizzly, afterGrizzly) = casting handGrizzly up
              (searchedRuneclaw, afterRuneclaw) = casting handRuneclaw up
              (searchedElves, _) = casting handElves up
              (searchedDown, afterDown) = casting handGrizzly main
          Spec.assertEqWith s "casting Grizzly Bears offers the Runeclaw Bear alone" searchedGrizzly [[libRuneclaw]]
          Spec.assertEqWith s "which joins the Runeclaw Bear in her hand" (inHand runeclawName afterGrizzly) 2
          Spec.assertEqWith s "casting Runeclaw Bear offers the Grizzly Bears alone" searchedRuneclaw [[libGrizzly]]
          Spec.assertEqWith s "which joins the Grizzly Bears in her hand" (inHand bears afterRuneclaw) 2
          Spec.assertEqWith s "casting Llanowar Elves, no chosen name, searches nothing" searchedElves []
          Spec.assertEqWith s "CR 315.5b left face down, it has no ability and searches nothing" searchedDown []
          Spec.assertEqWith s "and the Runeclaw Bear stays in her library" (inHand runeclawName afterDown) 1

-- Setup.newGame with alice bringing Summoner's Bond and a deck of this land,
-- bob a deck of it alone, naming Grizzly Bears then Runeclaw Bear for CR
-- 702.106f's choices, with each choice's restriction in order.
bonded :: Printing.Printing -> Printing.Printing -> (GameState.GameState, [Filter.Filter Keyword.Keyword])
bonded land bond =
  let naming :: Prompt.Prompt r -> State.State [Filter.Filter Keyword.Keyword] r
      naming p = case p of
        Prompt.ChooseCardName _ _ _ restriction _ -> do
          asked <- State.get
          State.put (asked <> [restriction])
          pure (if null asked then bears else runeclawName)
        _ -> pure (keepAnswer p)
      matchup = (S.alice, deckOf land [bond]) NonEmpty.:| [(S.bob, deckOf land [])]
      ((_, gs), restrictions) = State.runState (Engine.runGame naming (Setup.emptyGame (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, restrictions)

-- Alice casts this card off her hand and both players pass until the stack is
-- empty, taking every optional and the first card each search offers; with
-- what each search offered.
casting :: ObjectId -> GameState.GameState -> ([[ObjectId]], GameState.GameState)
casting oid gs =
  let answer :: Prompt.Prompt r -> State.State [[ObjectId]] r
      answer p = case p of
        Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
        Prompt.Search _ _ offered cap -> do
          State.modify' (<> [offered])
          pure (List.genericTake cap offered)
        _ -> pure (S.identityAnswer p)
      ((_, after), searched) = State.runState (Engine.runGame answer gs (S.cast S.alice oid >> Engine.priorityLoop)) []
   in (searched, after)

runeclawName :: CardName.CardName
runeclawName = CardName.MkCardName (Text.pack "Runeclaw Bear")

-- keepAnswer, naming Runeclaw Bear for every CR 702.106a choice.
renaming :: Prompt.Prompt r -> r
renaming p = case p of
  Prompt.ChooseCardName {} -> runeclawName
  _ -> keepAnswer p

-- The one card in this player's command zone, Nothing unless there is exactly
-- one.
commandCard :: PlayerId.PlayerId -> GameState.GameState -> Maybe ObjectId
commandCard pid gs = case Game.zoneMembers Zone.Command pid gs of
  [oid] -> Just oid
  _ -> Nothing

isFaceDown :: ObjectId -> GameState.GameState -> Bool
isFaceDown oid gs = maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs)

-- A creature that came under this player's control this turn (CR 302.6).
arriving :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
arriving printing pid gs =
  let (oid, placed) = S.addPermanent printing pid gs
   in placed {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects placed)}

-- Takes CR 116.2j's action the first time it is offered, and passes otherwise.
-- Counted, so a turning-over that fails to happen ends the loop rather than
-- being offered and taken forever.
flippingOnce :: Prompt.Prompt r -> State.State Int r
flippingOnce p = case p of
  Prompt.ChooseAction _ _ actions -> do
    taken <- State.get
    case (taken, List.find isFlip actions) of
      (0, Just flip') -> State.put 1 >> pure flip'
      _ -> pure Action.Type.Pass
  _ -> pure (S.identityAnswer p)
  where
    isFlip a = case a of
      Action.Type.TurnConspiracyFaceUp _ -> True
      _ -> False

-- The priority loop under flippingOnce.
withFlip :: GameState.GameState -> GameState.GameState
withFlip gs = snd (fst (State.runState (Engine.runGame flippingOnce gs Engine.priorityLoop) (0 :: Int)))

-- Alice's combat against bob, declaring every creature that may attack.
attackingWithAll :: GameState.GameState -> GameState.GameState
attackingWithAll gs =
  S.runCombat
    S.aggressiveAnswer
    gs
      { GameState.turn =
          (GameState.turn gs)
            { TurnRegisters.phase = Phase.Combat CombatStep.DeclareAttackers,
              TurnRegisters.priority = Nothing,
              TurnRegisters.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
              TurnRegisters.remaining = S.phasesAfter (Phase.Combat CombatStep.DeclareAttackers)
            }
      }
