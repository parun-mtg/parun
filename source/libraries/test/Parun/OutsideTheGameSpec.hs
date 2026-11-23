{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Parun.Engine.Event's CR 400.11 group -- `eligible`, `bringInto`,
-- `bringIn` and `bringInFrom`, the pool and the roads into the game -- the two
-- fields that carry it -- Parun.Types.Deck's sideboard (CR 103.2a)
-- and Parun.Types.Player's outsideTheGame -- Parun.Engine.Setup.createDeck's copy
-- between them, and Parun.Engine.Resolve's two arms:
-- Effect.FromOutsideTheGame and Effect.ExileThisSpell (CR 608.2n). Also
-- CR 730.4's second place a card can be outside the game: the main game as a
-- subgame sees it, which is Parun.Engine.Setup's subgameStateFrom and
-- applyCrossings with Parun.Engine.Engine.playSubgame between them.
--
-- Gameplay-level but for the CR 103.2a setup case and the CR 730.4 group that
-- follows it, which reach Event.eligible and Event.bringInto directly on a board
-- Setup.subgameStateFrom built -- the main game those cases
-- read is played out through the stack, but the wish inside the subgame is
-- called rather than cast. Every other case casts a printed wish and resolves it
-- through the stack, so what is asserted is the whole path from card JSON to the
-- card in the zone the wish names -- Burning Wish ({1}{R} sorcery, "You may
-- reveal a sorcery card you own from outside the game and put it into your hand.
-- Exile Burning Wish.")
-- for the pool, with The Raven's Warning for a destination that
-- is not the hand, Research for a count other than one, Extrapolate the
-- Impossible for CR 201.2b's different names and an opponent's pick among the
-- revealed cards, Turtles Forever for a search's finds out there revealed
-- for an opponent to split, and a Shahrazad subgame for CR 730.4's main
-- game -- Burning Wish reaching a main-game graveyard (CR 730.4a). Living
-- Wish reaching two main-game creatures, Death Wish reaching a main-game
-- Titania's Song and Burning Wish reaching the resolving Shahrazad are
-- scenarios under data/scenarios/outside-the-game.
--
-- The last group takes the OTHER road in: Ring of Ma'rûf's CR 614.6 draw
-- replacement, whose rewrite reaches the same `bringInto` the resolution arm
-- does, and Synthetic Wishful Djinn's, two of which put CR 616.1's choice.
--
-- The cases a board with cards outside the game can play are scenarios, under
-- data/scenarios/outside-the-game.
module Parun.OutsideTheGameSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Ante as Ante
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Phasing as Phasing
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Int as Int
import Parun.PreventionSpec (answersFor, wasAskedToReplace)
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.ActiveReplacement as ActiveReplacement
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ComponentCard as ComponentCard
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.DrawR as DrawR
import qualified Parun.Types.DrawRewrite as DrawRewrite
import qualified Parun.Types.ExileLink as ExileLink
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import Parun.Types.Game (Game)
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LeftTheGame as LeftTheGame
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.LoopTrail as LoopTrail
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.MergeComponent as MergeComponent
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.OutsideDestination as OutsideDestination
import qualified Parun.Types.OutsideObject as OutsideObject
import qualified Parun.Types.OwnershipChange as OwnershipChange
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PhasedOut as PhasedOut
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.Response as Response
import qualified Parun.Types.SearchPlace as SearchPlace
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.ShownComponent as ShownComponent
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Source as Source
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.Trait as Trait
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- alice, four untapped lands of one printing, one wish in hand, and the given
-- multiset of printings outside the game.
--
-- The pool is written onto the player directly, Parun.DungeonSpec's posture for
-- Player.dungeons: what CR 103.2a's road from a Deck does is the last case below,
-- and every case in between is about what a card can do with a pool that exists.
-- The printings are interned first, since Player.outsideTheGame names them by id.
wishBoard :: Printing.Printing -> Printing.Printing -> [(Printing.Printing, Natural.Natural)] -> (GameState.GameState, ObjectId.ObjectId)
wishBoard mountain wish sideboard =
  -- FOUR Mountains rather than two: the cases that cast a second wish off the
  -- board the first one left need mana the first cast did not tap.
  let (board, wishId) = S.handOne wish (S.landsInPlay mountain 4)
   in (stockPool sideboard board, wishId)

-- alice's pool outside the game set to the given multiset of printings.
stockPool :: [(Printing.Printing, Natural.Natural)] -> GameState.GameState -> GameState.GameState
stockPool sideboard board =
  let -- Interned in LIST ORDER, so the ids ascend with the list and the case
      -- below can name what Prompt.ChooseFromOutsideTheGame offers last.
      intern (printing, n) (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, n)], gs2)
      (entries, interned) = List.foldl' (flip intern) ([], board) sideboard
      stock p = p {Player.outsideTheGame = Map.fromList entries}
   in interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}

-- Burning Wish's printed "You may", taken. S.identityAnswer declines every
-- optional clause (CR 608.2d), so a case that wants the wish to do anything says
-- so here; the two cases that assert nothing happens still go through it, which
-- is what keeps "the filter admitted nothing" apart from "she declined".
exercising :: Prompt.Prompt r -> r
exercising p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Cast the wish and resolve it, under the given answer.
resolveWish :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
resolveWish answer wishId gs =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice wishId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- The printings of the cards in a zone, so a case can say WHICH card arrived
-- rather than only how many did.
printingsIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Printing.Printing]
printingsIn zone pid gs = Maybe.mapMaybe (\oid -> Game.printingOfObject oid gs) (Game.zoneMembers zone pid gs)

-- What is left in a player's pool, by printing.
poolOf :: PlayerId.PlayerId -> GameState.GameState -> Map.Map PrintingId.PrintingId Natural.Natural
poolOf pid gs = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))

-- n fresh objects of one printing in a player's library: what CR 730.2 moves into
-- the subgame, and the one main-game zone that does move. Which END of the
-- library they land at is not a dial the case below turns -- CR 730.2 shuffles
-- each library as the subgame starts, and the sizing argument there holds for
-- either order.
stockLibrary :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
stockLibrary printing n pid gs0 =
  List.foldl' (\gs _ -> snd (S.addLibraryCard printing pid gs)) gs0 (replicate n ())

-- CR 122.1a: the +1/+1 counters on one object.
plusOneCounters :: ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
plusOneCounters oid gs =
  maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)

-- alice's Hanweir Battlements and Hanweir Garrison put onto `base` and melded
-- into Hanweir, the Writhing Township (CR 701.42a), answering the township's id
-- -- Nothing if the meld did not happen. Duplicated from Parun.SetupSpec's
-- `meldedBoard` rather than hoisted into Parun.Support, which rebuilds every
-- spec in the tree; the five Mountains pay the melding ability.
meldedBoard :: GameState.GameState -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, GameState.GameState)
meldedBoard base battlements garrison mountain =
  let (battlementsId, g1) = S.addPermanent battlements S.alice base
      (_, g2) = S.addPermanent garrison S.alice g1
      board =
        (S.landsFor mountain S.alice 5 g2)
          { GameState.turn =
              (GameState.turn (S.landsFor mountain S.alice 5 g2))
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      townshipName = CardName.MkCardName (Text.pack "Hanweir, the Writhing Township")
   in case Projection.abilitiesOf battlementsId board of
        [_, _, melding] ->
          let after = S.runPure (sparing battlementsId) board (do Activate.activateAbility S.alice battlementsId melding; Stack.resolveTop)
              township = filter (\oid -> fmap S.nameOf (Game.cardOf oid after) == Just townshipName) (Game.zoneMembers Zone.Battlefield S.alice after)
           in (Maybe.listToMaybe township, after)
        _ -> (Nothing, board)

-- Pays the melding ability without tapping Hanweir Battlements for mana, whose
-- {T} the ability still needs (CR 107.5). Duplicated from Parun.SetupSpec.
sparing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sparing oid p = case p of
  Prompt.ChooseManaSource _ _ _ candidates _ -> List.find (/= oid) (NonEmpty.toList candidates)
  _ -> S.identityAnswer p

-- alice's Extrapolate the Impossible on a three-seat board with two Swamps,
-- and a pool drawn by index from Sign in Blood, Psychic Miasma and Hoarding
-- Dragon -- interned in that order, so the prompt offers them so.
extrapolateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [(Int, Natural.Natural)] -> m (GameState.GameState, ObjectId.ObjectId, (Printing.Printing, Printing.Printing, Printing.Printing))
extrapolateBoard s registry picks = do
  swamp <- S.printingOf s registry "Swamp"
  spell <- S.printingOf s registry "Extrapolate the Impossible"
  signInBlood <- S.printingOf s registry "Sign in Blood"
  miasma <- S.printingOf s registry "Psychic Miasma"
  dragon <- S.printingOf s registry "Hoarding Dragon"
  let printings = [signInBlood, miasma, dragon]
      (board, spellId) = S.handOne spell (S.landsFor swamp S.alice 2 S.threePlayerGame)
  pure (stockPool [(printings !! i, n) | (i, n) <- picks] board, spellId, (signInBlood, miasma, dragon))

-- The id a printing was interned under, if it was.
printingIdIn :: Printing.Printing -> GameState.GameState -> Maybe PrintingId.PrintingId
printingIdIn printing gs = List.find (\printingId -> Game.printingOf printingId gs == Just printing) (Map.keys (poolOf S.alice gs))

-- Extrapolate the Impossible's answers: the "may" taken, alice's reveal answered
-- with `named`, `chooser` named as the opponent who chooses, and the chosen seat
-- taking the card offered LAST where any other seat would take the first -- so
-- the head the engine falls back to proves nothing.
extrapolating :: [OutsideCard.OutsideCard] -> PlayerId.PlayerId -> Prompt.Prompt r -> r
extrapolating named chooser p = case p of
  Prompt.ChooseOpponent {} -> chooser
  Prompt.ChoosePlayer {} -> chooser
  Prompt.ChooseFromOutsideTheGame _ who candidates _ _
    | who == S.alice -> named
    | who == S.carol -> [fst (NonEmpty.last candidates)]
    | otherwise -> [fst (NonEmpty.head candidates)]
  _ -> exercising p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Event (CR 400.11)" $ do
  -- CR 606.3 / 400.11b: the wish as a loyalty ability, its filter a creature.
  -- CR 400.11a: Wish opens alice's sideboard, so each priority she is asked at
  -- stands its printings in as objects. A PAIR differing in how many printings
  -- it holds, one or three: she plays a Plains from her hand, and the id bob
  -- sees it by is the same in both.
  Spec.it s "CR 400.11a gameplay: the id bob sees a land alice plays after Wish by does not count her sideboard's printings" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    wish <- S.printingOf s registry "Wish"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    let playing sideboard =
          let (board, wishId) = wishBoard mountain wish sideboard
              (plainsId, withPlains) = S.addHandCard plains S.alice board
              before =
                withPlains
                  { GameState.turn =
                      (GameState.turn withPlains)
                        { TurnRegisters.activePlayer = S.alice,
                          TurnRegisters.phase = Phase.PrecombatMain,
                          TurnRegisters.priority = Just S.alice
                        }
                  }
              wished = resolveWish exercising wishId before
              answer :: Prompt.Prompt r -> State.State [Action.Action] r
              answer p = case p of
                Prompt.ChooseAction _ _ actions -> Action.Play plainsId Nothing <$ State.put actions
                _ -> pure (S.identityAnswer p)
              ((_, played), offered) = State.runState (Engine.runGame answer wished (Engine.priorityDecision LoopTrail.empty)) []
           in (fmap ObjectView.object (PlayerView.Type.battlefield (PlayerView.viewFor S.bob played)), length [() | Action.Play {} <- offered])
        (one, offeredOne) = playing [(island, 1)]
        (three, offeredThree) = playing [(island, 1), (forest, 1), (swamp, 1)]
    Spec.assertEqWith s "CR 400.11a: bob sees the same ids on either board" one three
    Spec.assertEqWith s "the Plains entered" (length one) 5
    Spec.assertEqWith s "her Plains and each sideboard land were offered" (offeredOne, offeredThree) (2, 4)

  Spec.it s "CR 400.11b Vivien, Arkbow Ranger's -5 puts a creature card from outside the game into her hand" $ do
    mountain <- S.printingOf s registry "Mountain"
    vivien <- S.printingOf s registry "Vivien, Arkbow Ranger"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (vivienId, placed) = S.addPermanent vivien S.alice (S.landsInPlay mountain 1)
        board =
          Game.overTurn
            ( \registers ->
                registers
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice
                  }
            )
            (stockPool [(signInBlood, 1), (bears, 1)] (S.addCounter CounterKind.Loyalty 5 vivienId placed))

    case Projection.abilitiesOf vivienId board of
      [_, _, wishing] -> do
        let resolved = S.runPure exercising board (do Activate.activateAbility S.alice vivienId wishing; Stack.resolveTop)
        Spec.assertEqWith s "the creature card reached her hand" (printingsIn Zone.Hand S.alice resolved) [bears]
        Spec.assertEqWith s "and the sorcery stays outside the game" (Map.elems (poolOf S.alice resolved)) [1]
      other -> Spec.assertEqWith s "Vivien has three loyalty abilities" (length other) 3
  -- North Wind Avatar ({2}{U}{U}{R} 5/5 flier, "When this creature enters, if you
  -- cast it, you may put a card you own from outside the game into your hand.")
  -- is the wish as an enters trigger behind CR 603.4's intervening "if".
  --
  -- A PAIR of boards differing in ONE thing, whether the Avatar was cast: the
  -- same pool, the same answerer taking the "may", and the same enters event.
  Spec.it s "CR 603.4 gameplay: North Wind Avatar wishes when it was cast, and not when it was only put onto the battlefield" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    avatar <- S.printingOf s registry "North Wind Avatar"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let lands = S.landsFor mountain S.alice 1 (S.landsInPlay island 4)
        stocked = stockLibrary island 3 S.bob . stockLibrary island 3 S.alice . stockPool [(signInBlood, 1)]
        atPriority gs =
          gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        castBoard = atPriority (stocked (snd (S.addHandCard avatar S.alice lands)))
        putBoard = atPriority (stocked (snd (S.entersWithTrigger avatar S.alice lands)))
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          _ -> S.castAnswer p
        run gs = snd (Engine.runGamePure answer gs Engine.priorityLoop)
        afterCast = run castBoard
        afterPut = run putBoard
    Spec.assertEqWith s "CR 400.11b: cast, its trigger put the card she owns outside the game into her hand" (printingsIn Zone.Hand S.alice afterCast) [signInBlood]
    Spec.assertEqWith s "CR 603.4: put onto the battlefield uncast, it wished for nothing" (printingsIn Zone.Hand S.alice afterPut) []
    Spec.assertEqWith s "and the pool is spent only the first way" (Map.size (poolOf S.alice afterCast), Map.size (poolOf S.alice afterPut)) (0, 1)
    Spec.assertEqWith s "setup: the Avatar is on the battlefield both ways" (printingsIn Zone.Battlefield S.alice afterCast List.\\ [island, island, island, island, mountain], printingsIn Zone.Battlefield S.alice afterPut List.\\ [island, island, island, island, mountain]) ([avatar], [avatar])
  -- CR 103.2a: the sideboard is set aside before the game, which is the one path
  -- every case above skips by writing Player.outsideTheGame directly.
  Spec.it s "CR 103.2a a deck's sideboard is recorded on its player and minted into no zone" $ do
    signInBlood <- S.printingOf s registry "Sign in Blood"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let deck =
          Deck.MkDeck
            { Deck.cards = Map.empty,
              Deck.commander = Map.empty,
              Deck.vanguard = Nothing,
              Deck.dungeons = Set.empty,
              Deck.sideboard = Map.fromList [(signInBlood, 2), (dragon, 1)],
              Deck.conspiracies = Map.empty,
              Deck.sideDecks = Map.empty,
              Deck.stickerSheets = Seq.empty,
              Deck.draftNotes = Map.empty,
              Deck.exiledBeforeGame = Map.empty
            }
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        heldBy pid = traverse (\(i, n) -> fmap (\p -> (p, n)) (Game.printingOf i after)) (Map.toList (poolOf pid after))
    Spec.assertEqWith s "alice set aside three cards, counted per printing" (fmap List.sort (heldBy S.alice)) (Just (List.sort [(signInBlood, 2), (dragon, 1)]))
    Spec.assertEqWith s "bob set aside none" (poolOf S.bob after) Map.empty
    -- CR 400.11a / 400.11: the sideboard is outside the game, and outside the game
    -- is not a zone -- so setup mints no object for any of it.
    Spec.assertEqWith s "and no object was minted for any of them" (Map.size (GameState.objects after)) 0
  -- CR 400.11c and CR 115.10a: what the filter CANNOT see. Filter.IsBound asks
  -- whether the candidate is an object the resolution bound, and a card outside
  -- the game is never one -- rule 400.11c keeps every spell and ability from
  -- affecting it, and nothing mints an object for it until `bringInto` does. So
  -- the bare Filter.contextFor `eligible` builds is honest: handing it the
  -- resolution's slots could not change an answer here. Parun.CardSpec's
  -- "CR 400.11c no card asks
  -- IsBound in a wish's filter" is what keeps a card out of the position.
  --
  -- The pair differs in exactly one thing: the same board, the same bound slot,
  -- the same card-type conjunct, and the atom present or absent.
  Spec.it s "CR 400.11c a wish's filter cannot see what the resolution bound" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (board, wishId) = wishBoard mountain wish [(signInBlood, 1)]
        slot = SlotName.MkSlotName (Text.pack "target")
        -- The binding a resolution would have made, present on the board before
        -- either filter runs. It names the wish's own object because WHICH object
        -- it names cannot matter: no candidate outside the game is any of them.
        bind o = o {Object.bindings = Map.insert slot (Binding.toObject wishId) (Object.bindings o)}
        gs = board {GameState.objects = Map.adjust bind wishId (GameState.objects board)}
        bringing predicate = snd (Engine.runGamePure exercising gs (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True Set.empty Nothing) wishId S.alice))
        atom = Filter.And [Filter.HasCardType CardType.Sorcery, Filter.IsBound slot]
    Spec.assertEqWith s "the slot names an object before either filter runs" (fmap Object.bindings (Game.lookupObject wishId gs)) (Just (Map.singleton slot (Binding.toObject wishId)))
    -- THE BEHAVIOUR: "a sorcery that IS the bound object" admits nothing, so the
    -- exclusion a card would write -- Not of the same atom -- is vacuously true.
    Spec.assertEqWith s "the wish brings in nothing" (printingsIn Zone.Hand S.alice (bringing atom)) [wish]
    Spec.assertEqWith s "where the same wish without the atom brings the sorcery in" (List.sort (printingsIn Zone.Hand S.alice (bringing (Filter.HasCardType CardType.Sorcery)))) (List.sort [wish, signInBlood])
    -- And the offer itself, one step nearer the atom: the atom is False for a
    -- candidate with no identity, so the conjunction admits nobody.
    Spec.assertEqWith s "nothing was even offered" (length (Event.eligible atom wishId S.alice gs)) 0
    Spec.assertEqWith s "where the sorcery filter offers the one card" (length (Event.eligible (Filter.HasCardType CardType.Sorcery) wishId S.alice gs)) 1
  -- CR 730.4: a subgame's own view of the main game. Alice's creature sits
  -- on the PARENT's battlefield; Setup.subgameStateFrom moves it into
  -- GameState.outsideObjects (CR 730.4, "all objects in the main game ... are
  -- considered outside the subgame"), and a wish cast IN the subgame can reach
  -- it just as it reaches the sideboard pool. Called at the unit level --
  -- `Event.bringInto` directly -- rather than through a printed wish,
  -- since Living Wish's own casting is not this unit's concern.
  Spec.it s "CR 730.4/730.4a a wish cast in a subgame reaches a main-game creature" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bearId, parent) = S.addPermanent bear S.alice (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True Set.empty Nothing) bearId S.alice))
    Spec.assertEqWith s "CR 730.4/400.11c: the main-game creature arrives in her subgame hand" (printingsIn Zone.Hand S.alice after) [bear]
    Spec.assertEqWith s "CR 730.4a: the crossing is recorded for the outer frame to apply" (Foldable.toList (GameState.broughtIn after)) [bearId]
    Spec.assertEqWith s "and it is no longer offered" (Map.member bearId (GameState.outsideObjects after)) False
  -- The discrimination OutsideCard exists for: BOTH of CR 400.11c's sources on
  -- offer at once, and the answer names the pool one, not the main-game one.
  Spec.it s "CR 400.11c/730.4: the pool and the main game are offered together, and the answer picks the pool card" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let (bearId, parent0) = S.addPermanent bear S.alice (Setup.emptyGame S.bothPlayers)
        (dragonId, parent1) = Game.intern dragon parent0
        stock p = p {Player.outsideTheGame = Map.singleton dragonId 1}
        parent = parent1 {GameState.players = Map.adjust stock S.alice (GameState.players parent1)}
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        offered = Event.eligible predicate bearId S.alice sub
        isPool candidate = case candidate of
          OutsideCard.InPool _ -> True
          OutsideCard.InAnotherGame _ -> False
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ candidates _ _ -> [Maybe.fromMaybe (fst (NonEmpty.head candidates)) (List.find isPool (fmap fst (NonEmpty.toList candidates)))]
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure answer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True Set.empty Nothing) bearId S.alice))
    Spec.assertEqWith s "the pool and the main game were both on offer" (length offered) 2
    Spec.assertEqWith s "the pool card is what arrived" (printingsIn Zone.Hand S.alice after) [dragon]
    Spec.assertEqWith s "the main-game creature is untouched: the answer did not take it" (Map.member bearId (GameState.outsideObjects after)) True
    Spec.assertEqWith s "and the pool entry was spent" (poolOf S.alice after) Map.empty
  -- CR 108.3b: the reach outside the game is scoped to the acting player's OWN
  -- cards. Bob's creature sits in the same outsideObjects map; alice's wish must
  -- not see it.
  Spec.it s "CR 108.3b a main-game creature owned by another player is not offered" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bobsBearId, parent) = S.addPermanent bear S.bob (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True Set.empty Nothing) bobsBearId S.alice))
    Spec.assertEqWith s "alice's hand stays empty: bob's creature is not hers to reach" (printingsIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "and bob's card is untouched" (Map.member bobsBearId (GameState.outsideObjects after)) True
  -- CR 708.2 over CR 730.4: what a subgame's wish sees of a FACE-DOWN main-game
  -- permanent. alice casts Soul Summons ({1}{W} sorcery, "Manifest the top card
  -- of your library") in the main game over Sign in Blood, so the permanent
  -- standing outside the subgame is a card whose PRINTED face and whose CR 708.2a
  -- face-down characteristics disagree about every card type it has: a sorcery
  -- underneath, a 2/2 creature with no name on the table.
  --
  -- TWO WISHES over the SAME board, which is what makes the reading falsifiable
  -- in both directions. Living Wish's creature-or-land filter must reach the
  -- manifest and did not before; Burning Wish's sorcery filter must NOT reach it
  -- and did before. The Soul Summons in alice's main-game graveyard is the
  -- control the second leg needs -- a FACE-UP sorcery outside the subgame, so
  -- "the sorcery wish brought nothing in" cannot pass for the right answer, and
  -- the answerer below asks for the manifest by id and settles for the graveyard
  -- card only because the manifest is not on offer.
  --
  -- The two Plains are lands outside the subgame, so the creature-or-land leg
  -- offers three candidates for one pick and cannot short-circuit. The Goblin
  -- Piker under Sign in Blood keeps CR 104.3c off the board and leaves the
  -- library non-empty for CR 730.2 to move.
  Spec.it s "CR 708.2/730.4 a manifested main-game sorcery is offered to a subgame's wish as a creature and not as a sorcery" $ do
    summons <- S.printingOf s registry "Soul Summons"
    plains <- S.printingOf s registry "Plains"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (g1, summonsId) = S.handOne summons (S.landsInPlay plains 2)
        (_, g2) = S.addLibraryCard piker S.alice g1
        (_, g3) = S.addLibraryCard signInBlood S.alice g2
        parent = S.runPure S.identityAnswer g3 (S.cast S.alice summonsId >> Stack.resolveTop)
        faceDownIds = filter (\oid -> maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid parent)) (Set.toList (GameState.battlefield parent))
        manifestedId = Maybe.fromMaybe summonsId (Maybe.listToMaybe faceDownIds)
        sub = Setup.subgameStateFrom S.alice parent
        creatureOrLand = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        -- Pinned by id, never by searching for a legal option: an answerer that
        -- picked whatever the filter offered would repair the assertion under a
        -- mutation. It falls back to the head only where the manifest is absent,
        -- which is the whole of what the sorcery leg asserts.
        preferManifest :: Prompt.Prompt r -> r
        preferManifest p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ candidates _ _ -> [Maybe.fromMaybe (fst (NonEmpty.head candidates)) (List.find (== OutsideCard.InAnotherGame manifestedId) (fmap fst (NonEmpty.toList candidates)))]
          _ -> S.identityAnswer p
        wishing predicate = snd (Engine.runGamePure preferManifest sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True Set.empty Nothing) manifestedId S.alice))
        offeredOuter predicate = Maybe.mapMaybe (\card -> case card of OutsideCard.InAnotherGame oid -> Just oid; _ -> Nothing) (Event.eligible predicate manifestedId S.alice sub)
    -- CR 708.2a's 2/2 creature is what the creature-or-land wish reaches, and CR
    -- 708.9's reveal is what arrives: the card itself, not the 2/2.
    Spec.assertEqWith s "CR 708.2a/730.4: the creature-or-land wish takes the manifest, and CR 708.9 hands it over as the sorcery card underneath" (printingsIn Zone.Hand S.alice (wishing creatureOrLand)) [signInBlood]
    -- The other arm of the same rule: "no characteristics other than those
    -- listed" leaves the manifest no card type but creature, so the sorcery wish
    -- has to settle for the face-up sorcery in the main-game graveyard.
    Spec.assertEqWith s "CR 708.2: the sorcery wish cannot reach the manifest and takes the face-up main-game sorcery instead" (printingsIn Zone.Hand S.alice (wishing (Filter.HasCardType CardType.Sorcery))) [summons]
    -- Preconditions, after the behaviour: the fixture really did manifest a
    -- sorcery, and the offer really did have a choice in it.
    Spec.assertEqWith s "the manifest is face down on the main-game battlefield" (length faceDownIds) 1
    Spec.assertEqWith s "and the card under it is Sign in Blood" (Game.printingOfObject manifestedId parent) (Just signInBlood)
    Spec.assertEqWith s "the creature-or-land wish was offered the manifest beside alice's two main-game Plains" (length (offeredOuter creatureOrLand), elem manifestedId (offeredOuter creatureOrLand)) (3, True)
    Spec.assertEqWith s "and the sorcery wish was offered the graveyard card alone" (elem manifestedId (offeredOuter (Filter.HasCardType CardType.Sorcery)), length (offeredOuter (Filter.HasCardType CardType.Sorcery))) (False, 1)
  -- CR 713.5c over CR 730.4: a melded main-game permanent is ONE object outside
  -- the subgame (CR 701.42a), offered by its combined face. Granted (Fae of
  -- Wishes' Adventure, "You may reveal a noncreature card you own from outside
  -- the game and put it into your hand.") is the one printed wish whose filter
  -- tells the combined face from the two front faces: Hanweir, the Writhing
  -- Township is a creature, while Hanweir Battlements is a land.
  --
  -- A PAIR differing in the meld alone, cast and resolved inside the subgame.
  -- Unmelded, Granted takes the Battlements; melded, the township is not on
  -- offer and no Hanweir card arrives. The answerer pins the Hanweir object by
  -- id and falls back to the head of the offer, which holds alice's main-game
  -- Mountains on both legs.
  Spec.it s "CR 713.5c/730.4 Granted cannot see a melded Hanweir, whose combined face is a creature, and takes an unmelded Battlements" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    fae <- S.printingOf s registry "Fae of Wishes"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        (battlementsId, u1) = S.addPermanent battlements S.alice (Setup.emptyGame S.bothPlayers)
        (_, u2) = S.addPermanent garrison S.alice u1
        unmelded = S.landsFor mountain S.alice 5 u2
        hanweir = Maybe.catMaybes [townshipId, Just battlementsId]
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Granted's printed "may" (CR 608.2d), taken.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find (\card -> any (\oid -> card == OutsideCard.InAnotherGame oid) hanweir) (fmap fst (NonEmpty.toList offered)))]
          _ -> S.identityAnswer p
        granting parent =
          let (faeId, sub1) = S.addHandCard fae S.alice (S.landsFor island S.alice 4 (Setup.subgameStateFrom S.alice parent))
              sub2 =
                sub1
                  { GameState.turn =
                      (GameState.turn sub1)
                        { TurnRegisters.phase = Phase.PrecombatMain,
                          TurnRegisters.activePlayer = S.alice,
                          TurnRegisters.priority = Just S.alice
                        }
                  }
              granted = CardName.MkCardName (Text.pack "Granted")
           in (faeId, S.runPure answer sub2 (Cast.castSpell S.manaPerformer S.alice faeId granted Facing.FaceUp >> Stack.resolveTop))
        hanweirIn gs = filter (`elem` [battlements, garrison]) (printingsIn Zone.Hand S.alice gs)
        (_, afterMelded) = granting melded
        (_, afterUnmelded) = granting unmelded
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 713.5c: the melded township is a creature, so Granted brings no Hanweir card in" (hanweirIn afterMelded) []
    Spec.assertEqWith s "CR 730.4: the unmelded Battlements is a noncreature card, and Granted takes it" (hanweirIn afterUnmelded) [battlements]
    -- What each leg rests on.
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "Granted resolved on the melded leg, taking a Mountain instead" (printingsIn Zone.Hand S.alice afterMelded) [mountain]
    Spec.assertEqWith s "CR 730.4: the township stays outside the subgame" (fmap (`Map.member` GameState.outsideObjects afterMelded) townshipId) (Just True)
  -- CR 713.8 over CR 730.4: what a wish takes of a melded permanent is the
  -- permanent, and the permanent leaving the battlefield is "two cards into
  -- the appropriate zone" -- here her subgame hand. Living Wish's creature
  -- filter admits the township's combined face.
  Spec.it s "CR 713.8/730.4 a wish cast in a subgame takes a melded main-game permanent, and both its cards arrive" $ do
    mountain <- S.printingOf s registry "Mountain"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        sub = Setup.subgameStateFrom S.alice melded
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        creature = Filter.HasCardType CardType.Creature
        after = snd (Engine.runGamePure exercising sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand creature True Set.empty Nothing) township S.alice))
    Spec.assertEqWith s "CR 713.8: both of Hanweir's cards arrive in her subgame hand" (List.sort (fmap S.nameOf (Maybe.mapMaybe (`Game.cardOf` after) (Game.zoneMembers Zone.Hand S.alice after)))) (List.sort [S.nameOf (Printing.card battlements), S.nameOf (Printing.card garrison)])
    Spec.assertEqWith s "CR 713.8e: one permanent crossed, under the township's id" (Foldable.toList (GameState.broughtIn after)) [township]
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "CR 730.4: it was the one creature outside the subgame" (fmap (\oid -> [OutsideCard.InAnotherGame oid]) townshipId) (Just (Event.eligible creature township S.alice sub))
  -- CR 713.8a: "if a melded permanent is put into its owner's graveyard or
  -- library, that player may arrange the two cards in any order" -- and a wish
  -- naming the top of her library (The Raven's Warning's chapter III) is that.
  -- A PAIR of answers to Prompt.OrderComponentCards over the same board, which
  -- must leave different cards on top.
  Spec.it s "CR 713.8a a melded permanent a subgame puts on top of her library is arranged by her, and the answer decides the top card" $ do
    mountain <- S.printingOf s registry "Mountain"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        sub = Setup.subgameStateFrom S.alice melded
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        creature = Filter.HasCardType CardType.Creature
        arranging :: ([Natural.Natural] -> [Natural.Natural]) -> Prompt.Prompt r -> r
        arranging order p = case p of
          Prompt.OrderComponentCards _ _ _ offered -> order (List.genericTake (length offered) [0 ..])
          _ -> exercising p
        topWith order =
          let after = snd (Engine.runGamePure (arranging order) sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.LibraryTop creature False Set.empty Nothing) township S.alice))
              library = Game.zoneMembers Zone.Library S.alice after
           in (fmap S.nameOf (Maybe.listToMaybe library >>= (`Game.cardOf` after)), length library)
        (asOffered, offeredCount) = topWith id
        (reversedOrder, reversedCount) = topWith reverse
    Spec.assertBool s (asOffered /= reversedOrder) "CR 713.8a: the two arrangements leave different cards on top"
    Spec.assertEqWith s "and each top card is one of Hanweir's" (all (`elem` [Just (S.nameOf (Printing.card battlements)), Just (S.nameOf (Printing.card garrison))]) [asOffered, reversedOrder]) True
    Spec.assertEqWith s "CR 713.8: both cards arrive on each leg" (offeredCount, reversedCount) (2, 2)
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
  -- CR 731.3 over CR 730.4: a merged permanent rides the same door. Cubwarden
  -- merged over alice's Goblin Piker is one object outside the subgame, offered
  -- by its topmost component (CR 731.2a), and both cards arrive. Hand-built
  -- through Object.source rather than cast, since the merge itself is
  -- Parun.MutateSpec's subject. Two more boards put a token on top, which CR
  -- 731.2d makes a token and so no card a wish can bring in, and a token
  -- underneath, which is no card to bring. The Piker is one alice won from bob
  -- (CR 108.3), so each card arriving with its own identity is what lets the
  -- subgame's ownership report name it and not the Cubwarden.
  Spec.it s "CR 731.3/730.4 a wish cast in a subgame takes a merged main-game permanent, and both its cards arrive" $ do
    cubwarden <- S.printingOf s registry "Cubwarden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (mergedId, g1) = S.addPermanent cubwarden S.alice (Setup.emptyGame S.bothPlayers)
        (pikerPrinting, g2) = Game.intern piker g1
        cubwardenPrinting = case fmap Object.source (Game.lookupObject mergedId g2) of
          Just (Source.OfCard printingId) -> printingId
          _ -> pikerPrinting
        withSource source gs = gs {GameState.objects = Map.adjust (\o -> o {Object.source = source}) mergedId (GameState.objects gs)}
        cubwardenIdentity = Game.lookupObject mergedId g2 >>= Object.identity
        pikerIdentity = Just (CardIdentity.MkCardIdentity 99 S.bob)
        merged = withSource (Source.OfMerge (fmap (\c -> ShownComponent.MkShownComponent c Nothing Facing.FaceUp) (MergeComponent.OfCard (ComponentCard.MkComponentCard cubwardenPrinting cubwardenIdentity Nothing) NonEmpty.:| [MergeComponent.OfCard (ComponentCard.MkComponentCard pikerPrinting pikerIdentity Nothing)]))) g2
        tokenTopped = withSource (Source.OfMerge (fmap (\c -> ShownComponent.MkShownComponent c Nothing Facing.FaceUp) (MergeComponent.OfToken pikerPrinting NonEmpty.:| [MergeComponent.OfCard (ComponentCard.MkComponentCard cubwardenPrinting Nothing Nothing)]))) g2
        overToken = withSource (Source.OfMerge (fmap (\c -> ShownComponent.MkShownComponent c Nothing Facing.FaceUp) (MergeComponent.OfCard (ComponentCard.MkComponentCard cubwardenPrinting Nothing Nothing) NonEmpty.:| [MergeComponent.OfToken pikerPrinting]))) g2
        creature = Filter.HasCardType CardType.Creature
        wishing parent = snd (Engine.runGamePure exercising (Setup.subgameStateFrom S.alice parent) (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand creature True Set.empty Nothing) mergedId S.alice))
    let reported = [(fmap (S.nameOf . Printing.card) (Game.printingOfObject oid (wishing merged)), change) | c <- Foldable.toList (Ante.ownershipChanges (wishing merged)), let oid = OwnershipChange.object c; change = (OwnershipChange.startingOwner c, OwnershipChange.owner c)]
    Spec.assertEqWith s "CR 108.3/730.4a: each card keeps its own identity, so only the Piker is bob's to begin" reported [(Just (S.nameOf (Printing.card piker)), (S.bob, S.alice))]
    Spec.assertEqWith s "CR 731.3: both of the merged permanent's cards arrive in her subgame hand" (List.sort (printingsIn Zone.Hand S.alice (wishing merged))) (List.sort [cubwarden, piker])
    Spec.assertEqWith s "CR 731.2d: a token on top makes it a token, and no card is brought in" (printingsIn Zone.Hand S.alice (wishing tokenTopped)) []
    Spec.assertEqWith s "CR 731.3/111.7: Cubwarden over a token brings the one card, the token being none" (printingsIn Zone.Hand S.alice (wishing overToken)) [cubwarden]
    Spec.assertEqWith s "the merged board's entry is offered by its topmost component" (fmap OutsideObject.printing (Map.lookup mergedId (GameState.outsideObjects (Setup.subgameStateFrom S.alice merged)))) (Just cubwardenPrinting)
  -- CR 713.8 / 730.4a / 730.5 at gameplay level: alice casts Shahrazad over a
  -- melded Hanweir, the Writhing Township, and inside the subgame Living Wish
  -- takes the township. One permanent leaves the main game (CR 713.8e), and
  -- its two cards are what the subgame hands back to her main-game library.
  --
  -- The sizing is the Death Wish scenario's
  -- (outside-the-game/cr-604-2-730-4a-gameplay-death-wish-takes-titania-s-song-out-of-the-main-game-and-its-effect.json): one wish and eight Forests, so she
  -- holds the wish by turn 5 with three lands down, and bob decks on turn 6.
  -- Her main-game Plains and Mountains are lands outside the subgame too, so
  -- the offer has a choice in it; the answerer pins the township by id.
  Spec.it s "CR 713.8/730.4a gameplay: Living Wish takes a melded Hanweir out of the main game, and both its cards come back to her library" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    livingWish <- S.printingOf s registry "Living Wish"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        g1 = S.landsFor plains S.alice 2 melded
        g2 = stockLibrary mountain 9 S.bob (stockLibrary forest 8 S.alice (stockLibrary livingWish 1 S.alice g1))
        (_shahrazadId, g3) = S.addHandCard shahrazad S.alice g2
        before =
          g3
            { GameState.turn =
                (GameState.turn g3)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Living Wish's printed "may" (CR 608.2d), taken.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find (== OutsideCard.InAnotherGame township) (fmap fst (NonEmpty.toList offered)))]
          -- CR 730.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
        leftTheGame = [LeftTheGame.object l | GameEvent.LeftTheGame l <- fmap LoggedEvent.event (Foldable.toList (TurnRegisters.events (GameState.turn after)))]
        hanweirIn zone = filter (`elem` [battlements, garrison]) (printingsIn zone S.alice after)
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 713.8/730.5: both of Hanweir's cards come back to her main-game library" (List.sort (hanweirIn Zone.Library)) (List.sort [battlements, garrison])
    Spec.assertEqWith s "CR 713.8e/730.4a: one permanent left the main game, the township" leftTheGame [township]
    -- What the claims rest on.
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "the township started on the main-game battlefield" (Set.member township (GameState.battlefield before)) True
    Spec.assertEqWith s "CR 730.4a: it is gone from the main game" (Map.member township (GameState.objects after)) False
    Spec.assertEqWith s "and neither card is on her battlefield" (hanweirIn Zone.Battlefield) []
    Spec.assertEqWith s "CR 730.1b: alice won the subgame, so only bob paid" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
  -- CR 730.4a's wider half: main-game abilities "trigger on objects leaving a
  -- main-game ZONE", not only the battlefield. alice's Spirit Mascot ({R}{W}
  -- creature, "Whenever one or more cards leave your graveyard, put a +1/+1
  -- counter on this creature.") watches her main-game graveyard, which holds a
  -- Sign in Blood. Inside the subgame Burning Wish takes a sorcery she owns from
  -- outside the game, and CR 730.4 offers it two: the Sign in Blood, and the
  -- resolving Shahrazad on the main game's stack.
  --
  -- A PAIR differing in the answer alone. Taking the graveyard card is a card
  -- leaving her graveyard, and the Mascot gets its counter once the main game
  -- resumes; taking Shahrazad is a card leaving the STACK, and it gets none.
  --
  -- The sizing is the Shahrazad-taking scenario's, which this board's second
  -- leg repeats:
  -- outside-the-game/cr-730-5-gameplay-a-wish-that-takes-the-resolving-shahrazad-itself-still-finishes-resolving.json.
  Spec.it s "CR 730.4a gameplay: Burning Wish takes a card out of a main-game graveyard and Spirit Mascot sees it leave" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    mascot <- S.printingOf s registry "Spirit Mascot"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let g0 = Setup.emptyGame S.bothPlayers
        (mascotId, g1) = S.addPermanent mascot S.alice g0
        (signId, g2) = S.addGraveyardCard signInBlood S.alice g1
        g3 = S.landsFor plains S.alice 2 g2
        g4 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g3))
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before =
          g5
            { GameState.turn =
                (GameState.turn g5)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        -- Pinned by id: the graveyard card, or the one other main-game sorcery
        -- offered, which is the resolving Shahrazad.
        playWith takeGraveyardCard =
          let wanted candidate = case candidate of
                OutsideCard.InAnotherGame oid -> (oid == signId) == takeGraveyardCard
                OutsideCard.InPool _ -> False
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                -- Burning Wish's printed "may" (CR 608.2d), taken.
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
                  [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find wanted (fmap fst (NonEmpty.toList offered)))]
                -- CR 730.2's roll, answered so the turn count above is the one played.
                Prompt.RandomFirstPlayer _ -> S.alice
                _ -> S.castAnswer p
           in snd (Engine.runGamePure answer before Engine.priorityLoop)
        tookGraveyardCard = playWith True
        tookShahrazad = playWith False
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 730.4a: a card left alice's main-game graveyard, so the Mascot got its counter" (plusOneCounters mascotId tookGraveyardCard) 1
    Spec.assertEqWith s "CR 730.4a: a card left the main game's stack instead, so it got none" (plusOneCounters mascotId tookShahrazad) 0
    -- What each leg took, which is the pair's one difference.
    Spec.assertEqWith s "the first leg's wish took the Sign in Blood out of her graveyard" (Map.member signId (GameState.objects tookGraveyardCard), printingsIn Zone.Graveyard S.alice tookGraveyardCard) (False, [shahrazad])
    Spec.assertEqWith s "the second leg's wish took Shahrazad, leaving the Sign in Blood where it was" (Map.member signId (GameState.objects tookShahrazad), length (filter (== shahrazad) (printingsIn Zone.Library S.alice tookShahrazad))) (True, 1)
    Spec.assertEqWith s "the Mascot starts with no counters" (plusOneCounters mascotId before) 0
    Spec.assertEqWith s "CR 730.1b: alice won the subgame on both legs, so only bob paid" (fmap (\gs -> (S.lifeOf S.alice gs, S.lifeOf S.bob gs)) [tookGraveyardCard, tookShahrazad]) [(Just 20, Just 10), (Just 20, Just 10)]

  -- CR 730.5 again, against the two readers that looked the resolving spell up
  -- by id: Synthetic Subgame Tithe ({W}{W} sorcery: players play a subgame,
  -- then roll a six-sided die, you gain life equal to the result, then 1 life
  -- for each player who didn't win). A synthetic because no printed subgame
  -- card reads an amount it bound or counts "each player except the winner";
  -- issue 2493 records the Scryfall query. Burning Wish inside the subgame takes
  -- the resolving Tithe, so both gains are read off a spell whose object is
  -- gone: the die's 4 through Quantity.InSlot, and bob's 1 through
  -- Count.playersFor's EachPlayerExcept arm.
  --
  -- A PAIR differing in the wish's "may" alone: declined, the Tithe stays on
  -- the stack and the same gains are read off a live object. The sizing is the
  -- Shahrazad-taking scenario's, named in the Spirit Mascot case above.
  Spec.it s "CR 730.5 gameplay: a subgame spell whose card a wish took still reads the die it rolled and counts who didn't win" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    tithe <- S.printingOf s registry "Synthetic Subgame Tithe"
    burningWish <- S.printingOf s registry "Burning Wish"
    let g1 = S.landsFor plains S.alice 2 (Setup.emptyGame S.bothPlayers)
        g2 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g1))
        (_titheId, g3) = S.addHandCard tithe S.alice g2
        before =
          g3
            { GameState.turn =
                (GameState.turn g3)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        playWith decision =
          let answer :: Prompt.Prompt r -> r
              answer p = case p of
                -- Burning Wish's printed "may" (CR 608.2d): the pair's one difference.
                Prompt.ChooseOptional {} -> decision
                -- CR 706.2's result, distinct from every other number here.
                Prompt.RollDie _ -> 4
                -- CR 730.2's roll, answered so the turn count above is the one played.
                Prompt.RandomFirstPlayer _ -> S.alice
                _ -> S.castAnswer p
           in snd (Engine.runGamePure answer before Engine.priorityLoop)
        taken = playWith OptionalDecision.Exercises
        left = playWith OptionalDecision.Declines
    -- The gameplay-level claims, first: 20 + 4 for the die + 1 for bob.
    Spec.assertEqWith s "CR 730.5: with the Tithe's card taken, alice still gains the die's 4 and 1 for bob" (S.lifeOf S.alice taken) (Just 25)
    Spec.assertEqWith s "the same gains off a Tithe nothing took" (S.lifeOf S.alice left) (Just 25)
    -- What the pair rests on.
    Spec.assertEqWith s "CR 730.4/730.5: the wish took the Tithe, which came back to alice's main-game library" (length (filter (== tithe) (printingsIn Zone.Library S.alice taken)), length (filter (== tithe) (printingsIn Zone.Graveyard S.alice taken))) (1, 0)
    Spec.assertEqWith s "the declined wish left it to finish into her graveyard" (length (filter (== tithe) (printingsIn Zone.Graveyard S.alice left))) 1
    Spec.assertEqWith s "bob lost nothing either way" (S.lifeOf S.bob taken, S.lifeOf S.bob left) (Just 20, Just 20)

  -- CR 603.12 in CR 730.5's resumed resolution: Synthetic Subgame Levy ({W}{W}
  -- sorcery: players play a subgame, then each opponent loses life equal to the
  -- number of cards in your hand; when they do, you gain 3 life). A synthetic
  -- because no printed subgame card has a reflexive. Burning Wish takes the
  -- resolving Levy, so the loss's tally is filed in GameState.detachedBindings,
  -- and a tally of zero there must not arm the reflexive
  -- (Resolve.Common.happenedBetween).
  --
  -- A PAIR differing in alice's main-game hand alone: a Plains, so bob loses 1
  -- and the reflexive arms, or empty, so he loses nothing and it must not. The
  -- sizing is the Subgame Tithe case above's.
  Spec.it s "CR 603.12 / 730.5 gameplay: a zero tally a wish-taken spell files detached arms no reflexive" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    levy <- S.printingOf s registry "Synthetic Subgame Levy"
    burningWish <- S.printingOf s registry "Burning Wish"
    let playWith handCards =
          let g1 = S.landsFor plains S.alice 2 (Setup.emptyGame S.bothPlayers)
              g2 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g1))
              g3 = List.foldl' (\gs _ -> snd (S.addHandCard plains S.alice gs)) g2 (replicate handCards ())
              (_levyId, g4) = S.addHandCard levy S.alice g3
              before =
                g4
                  { GameState.turn =
                      (GameState.turn g4)
                        { TurnRegisters.activePlayer = S.alice,
                          TurnRegisters.phase = Phase.PrecombatMain,
                          TurnRegisters.priority = Just S.alice
                        }
                  }
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                -- Burning Wish's printed "may" (CR 608.2d), taken.
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                -- CR 730.2's roll, answered so the turn count above is the one played.
                Prompt.RandomFirstPlayer _ -> S.alice
                _ -> S.castAnswer p
           in snd (Engine.runGamePure answer before Engine.priorityLoop)
        lost = playWith (1 :: Int)
        none = playWith 0
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 603.12: bob lost 1, so alice gained the reflexive's 3" (S.lifeOf S.alice lost, S.lifeOf S.bob lost) (Just 23, Just 19)
    Spec.assertEqWith s "CR 603.12: bob lost nothing, so the reflexive never armed" (S.lifeOf S.alice none, S.lifeOf S.bob none) (Just 20, Just 20)
    -- What the pair rests on.
    Spec.assertEqWith s "CR 730.4/730.5: on both legs the wish took the Levy, which came back to alice's main-game library" (fmap (length . filter (== levy) . printingsIn Zone.Library S.alice) [lost, none]) [1, 1]
  -- CR 614.6 / 400.11c: Ring of Ma'rûf ({5} Artifact, "{5}, {T}, Exile this
  -- artifact: The next time you would draw a card this turn, instead put a card
  -- you own from outside the game into your hand."). The CONTROL of the
  -- outside-the-game scenario whose draw it replaces: the ability never
  -- activated, so the only difference is the floating row.
  Spec.it s "CR 121.1 with no row installed the same board draws off her library" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (unarmed, _) = ringBoard plains ring piker signInBlood False
        after = S.runPure S.identityAnswer unarmed (Event.drawCard Settle.none S.alice)
    Spec.assertEqWith s "CR 121.1 she drew the library card" (printingsIn Zone.Hand S.alice after) [piker]
    Spec.assertEqWith s "which came off her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 400.11b and nothing was taken out of the pool" (Map.size (poolOf S.alice after)) 1
  -- CR 616.1 / 616.1e: Synthetic Wishful Djinn ({3}{U}{U} Creature -- Djinn 3/3,
  -- "If you would draw a card, instead put a card you own from outside the game
  -- with power less than this creature's power into your hand"), synthetic: no
  -- printing replaces a draw with a wish that names its source -- Scryfall
  -- o:"outside the game" o:"would draw", 2026-10-09, Ring of Ma'rûf alone.
  --
  -- Two Djinns, one carrying a +1/+1 counter, both apply to her draw; once one
  -- applies the draw is gone (CR 616.1f), so which she picks decides the card.
  -- The pool holds one Hill Giant (power 3): the 4-power Djinn reaches it and
  -- the 3-power one reaches nothing. Each run names one Djinn, so whichever the
  -- engine would take unasked, one of the two runs disagrees with it.
  Spec.it s "CR 616.1 two Djinns of different power are a choice, and the answer decides what arrives" $ do
    plains <- S.printingOf s registry "Plains"
    djinn <- S.printingOf s registry "Synthetic Wishful Djinn"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let (board, pumped, plain) = djinnBoard plains djinn piker giant
        drawn preferred = S.runPure (preferringRow preferred) board (Event.drawCard Settle.none S.alice)
        viaPumped = drawn pumped
        viaPlain = drawn plain
    Spec.assertEqWith s "setup: the Djinns' powers are four and three" (Projection.powerOf pumped board, Projection.powerOf plain board) (Just 4, Just 3)
    -- THE BEHAVIOUR, ahead of every proxy.
    Spec.assertEqWith s "CR 616.1 through the bigger Djinn the Hill Giant reached her hand" (printingsIn Zone.Hand S.alice viaPumped) [giant]
    Spec.assertEqWith s "CR 616.1 through the smaller Djinn nothing did" (printingsIn Zone.Hand S.alice viaPlain) []
    Spec.assertEqWith s "CR 400.11b the Giant left the pool only the first way" (Map.size (poolOf S.alice viaPumped), Map.size (poolOf S.alice viaPlain)) (0, 1)
    Spec.assertEqWith s "CR 614.6 both ways the draw was replaced" (length (Game.zoneMembers Zone.Library S.alice viaPumped), length (Game.zoneMembers Zone.Library S.alice viaPlain)) (2, 2)
    Spec.assertBool s (wasAskedToReplace (answersFor (preferringRow pumped) board (Event.drawCard Settle.none S.alice))) "a ChooseReplacement was raised"
  -- The other side of the classification: two Rings of Ma'rûf, both activated,
  -- install two rows alike in `effect` whose filter (`And []`) reads nothing of
  -- either Ring, so either one brings in the same card and there is nothing to
  -- ask.
  Spec.it s "CR 616.1 two Rings' wishes are not a choice" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (armed, _) = ringBoard plains ring piker signInBlood True
        (secondId, g1) = S.addPermanent ring S.alice (S.landsFor plains S.alice 5 armed)
        twice = case Face.activatedAbilities (S.combinedFace ring) of
          ability : _ -> S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice secondId ability >> Stack.resolveTop)
          [] -> g1
    Spec.assertEqWith s "setup: both Rings were exiled, so both rows stand" (Set.member secondId (GameState.battlefield twice), length (StoredEffects.replacements (GameState.stored twice))) (False, 2)
    Spec.assertBool s (not (wasAskedToReplace (answersFor S.identityAnswer twice (Event.drawCard Settle.none S.alice)))) "no ChooseReplacement was raised"
    Spec.assertEqWith s "CR 400.11c and the card she owns outside the game reached her hand" (printingsIn Zone.Hand S.alice (S.runPure S.identityAnswer twice (Event.drawCard Settle.none S.alice))) [signInBlood]
  -- The two Rings' rows again, each re-aimed at a filter reading the source's
  -- COLOURS (Filter.Shares Color Source) and re-sourced onto a red Goblin
  -- Piker and a blue Coral Merfolk. A direct board: no card writes
  -- that atom into a wish. The pool holds one Hill Giant (red): through the
  -- Piker's row it arrives, through the Merfolk's nothing does, so treating the
  -- rows as alike would decide her draw for her.
  Spec.it s "CR 616.1 two wishes sharing a colour with differently coloured sources are a choice" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    merfolk <- S.printingOf s registry "Coral Merfolk"
    let (armed, _) = ringBoard plains ring piker giant True
        (secondId, g1) = S.addPermanent ring S.alice (S.landsFor plains S.alice 5 armed)
        twice = case Face.activatedAbilities (S.combinedFace ring) of
          ability : _ -> S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice secondId ability >> Stack.resolveTop)
          [] -> g1
        (red, g2) = S.addPermanent piker S.alice twice
        (blue, g3) = S.addPermanent merfolk S.alice g2
        sharing row = case ActiveReplacement.effect row of
          ReplacementEffect.DrawR (DrawR.MkDrawR w e (DrawRewrite.FromOutsideTheGame p)) ->
            row {ActiveReplacement.effect = ReplacementEffect.DrawR (DrawR.MkDrawR w e (DrawRewrite.FromOutsideTheGame p {FromOutsideTheGame.filter = Filter.Shares Trait.Color Anchor.Source}))}
          _ -> row
        board = g3 {GameState.stored = (GameState.stored g3) {StoredEffects.replacements = zipWith (\src row -> (sharing row) {ActiveReplacement.source = src}) [red, blue] (StoredEffects.replacements (GameState.stored g3))}}
        drawn preferred = S.runPure (preferringRow preferred) board (Event.drawCard Settle.none S.alice)
    Spec.assertEqWith s "setup: two rows stand, sourced on the Piker and the Merfolk" (fmap ActiveReplacement.source (StoredEffects.replacements (GameState.stored board))) [red, blue]
    -- THE BEHAVIOUR, ahead of every proxy.
    Spec.assertEqWith s "CR 616.1 through the red Piker's row the Hill Giant reached her hand" (printingsIn Zone.Hand S.alice (drawn red)) [giant]
    Spec.assertEqWith s "CR 616.1 through the blue Merfolk's row nothing did" (printingsIn Zone.Hand S.alice (drawn blue)) []
    Spec.assertBool s (wasAskedToReplace (answersFor (preferringRow red) board (Event.drawCard Settle.none S.alice))) "a ChooseReplacement was raised"
  -- CR 407.3: "when not playing for ante ... these cards can't be brought into
  -- the game from outside the game". Contract from Below is a sorcery, so
  -- Burning Wish's filter admits it; the pair differs only in the setting.
  Spec.it s "CR 407.3 Burning Wish cannot bring in an ante card when not playing for ante" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    contract <- S.printingOf s registry "Contract from Below"
    let (gs, wishId) = wishBoard mountain wish [(contract, 1)]
        forAnte = gs {GameState.match = (GameState.match gs) {MatchScope.settings = (MatchScope.settings (GameState.match gs)) {GameSettings.ante = True}}}
    Spec.assertEqWith s "CR 407.3 not playing for ante, her hand gets nothing" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId gs)) []
    Spec.assertEqWith s "playing for ante, the same wish brings it in" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId forAnte)) [contract]
  -- CR 108.5: a nontraditional card "remains outside the game". Death Wish
  -- admits any card, so each of a plane, a scheme, a vanguard, an Attraction and
  -- a Contraption is offered by nothing but the bar; Hill Giant is the control.
  Spec.it s "CR 108.5 Death Wish cannot bring in a nontraditional card" $ do
    swamp <- S.printingOf s registry "Swamp"
    wish <- S.printingOf s registry "Death Wish"
    giant <- S.printingOf s registry "Hill Giant"
    outsiders <- traverse (S.printingOf s registry) ["Agyrem", "All in Good Time", "Barrin", "Bumper Cars", "Arms Depot"]
    let fetched card = let (gs, wishId) = wishBoard swamp wish [(card, 1)] in resolveWish exercising wishId gs
    Monad.forM_ outsiders $ \card -> do
      Spec.assertEqWith s ("CR 108.5 her hand gets no " <> show (S.nameOf (Printing.card card))) (printingsIn Zone.Hand S.alice (fetched card)) []
      Spec.assertEqWith s "CR 108.5 and it remains outside the game" (Map.elems (poolOf S.alice (fetched card))) [1]
    Spec.assertEqWith s "the same wish brings a traditional card in" (printingsIn Zone.Hand S.alice (fetched giant)) [giant]
  -- CR 903.11: an ordinary effect brings nothing into a Commander game. The pair
  -- differs only in alice's designated commander; Zirda, the creature in her
  -- pool, is what Living Wish admits.
  Spec.it s "CR 903.11 Living Wish brings nothing into a Commander game" $ do
    forest <- S.printingOf s registry "Forest"
    wish <- S.printingOf s registry "Living Wish"
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let (board, wishId) = S.handOne wish (S.landsInPlay forest 4)
        commanded = S.runPure S.identityAnswer board (Setup.createDeck S.alice ((Deck.fromCards Map.empty) {Deck.commander = Map.singleton shimatsu 1}))
        withZirda = stockPool [(zirda, 1)]
    Spec.assertEqWith s "CR 903.11: in a Commander game her hand gets nothing" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId (withZirda commanded))) []
    Spec.assertEqWith s "with no commander the same wish brings Zirda in" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId (withZirda board))) [zirda]
  -- CR 730.2c / 903.11: a subgame of a Commander game is one, so its wish
  -- reaches no main-game card either. docs/decisions.md, "Whether a subgame of a
  -- Commander game is one".
  Spec.it s "CR 730.2c/903.11 a wish in a subgame of a Commander game reaches no main-game creature" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let (bearId, plain) = S.addPermanent bear S.alice (Setup.emptyGame S.bothPlayers)
        commanded = S.runPure S.identityAnswer plain (Setup.createDeck S.alice ((Deck.fromCards Map.empty) {Deck.commander = Map.singleton shimatsu 1}))
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        offered parent = Event.eligible predicate bearId S.alice (Setup.subgameStateFrom S.alice parent)
    Spec.assertEqWith s "CR 903.11: under a commander nothing is offered" (offered commanded) []
    Spec.assertEqWith s "CR 730.4: with none the main-game creature is" (offered plain) [OutsideCard.InAnotherGame bearId]
  -- CR 407.3 / 730.4: only the nine ante cards remove a card from the ante, and
  -- a subgame wish is not one of them. alice's main-game ante holds a Sign in
  -- Blood; inside a Shahrazad subgame, Burning Wish asks for it in vain. The
  -- game is played for ante, so the subgame antes one card from each library
  -- too: each library holds one Mountain more than the CR 730.4a case's, and
  -- the subgame's random ante is pinned away from the Burning Wish.
  Spec.it s "CR 407.3/730.4 gameplay: a subgame's Burning Wish cannot take a main-game ante card" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let g0 = Setup.gameWith GameSettings.plain {GameSettings.ante = True} S.bothPlayers
        (signId, g1) = S.addObjectIn Zone.Ante signInBlood S.alice g0
        g2 = S.landsFor plains S.alice 2 g1
        (wishLibraryId, g3) = S.addLibraryCard burningWish S.alice g2
        g4 = stockLibrary mountain 10 S.bob (stockLibrary mountain 9 S.alice g3)
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before =
          g5
            { GameState.turn =
                (GameState.turn g5)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        wanted candidate = case candidate of
          OutsideCard.InAnotherGame oid -> oid == signId
          OutsideCard.InPool _ -> False
        -- Records whether the Sign in Blood was ever on offer, and asks for it.
        recordOffers :: Prompt.Prompt r -> State.State Bool r
        recordOffers p = case p of
          Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> do
            State.modify' (\seen -> seen || any (wanted . fst) (NonEmpty.toList offered))
            pure [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find wanted (fmap fst (NonEmpty.toList offered)))]
          Prompt.RandomFirstPlayer _ -> pure S.alice
          -- CR 407.2 in the subgame: never ante the wish.
          Prompt.RandomObject offered -> pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find (/= wishLibraryId) (NonEmpty.toList offered)))
          _ -> pure (S.castAnswer p)
        ((_, after), offeredSign) = State.runState (Engine.runGame recordOffers before Engine.priorityLoop) False
    Spec.assertEqWith s "CR 407.3 the main-game ante card was never offered" offeredSign False
    Spec.assertEqWith s "CR 407.3 and it is still in alice's main-game ante" (Set.member signId (GameState.ante after), fmap Object.zone (Game.lookupObject signId after)) (True, Just Zone.Ante)
    Spec.assertEqWith s "CR 730.1a the subgame did not decide the main game, so nobody decked out" (GameState.result after) Nothing
  -- CR 108.3 / 730.4a / 730.5: an ownership change survives a card's trip
  -- through a subgame. In the main game alice's Tempest Efreet trades
  -- ownership with bob's one card in hand, a Sign in Blood (CR 407.3), which
  -- goes to her hand. Then she casts Shahrazad, and inside the subgame (the
  -- CR 407.3/730.4 case's sizing) Burning Wish takes that Sign in Blood out of
  -- her main-game hand. CR 730.5 sends it home to her main-game library, and
  -- the ownership report still names bob as the player who began the game
  -- with it, beside the Efreet that went the other way and the subgame ante
  -- card alice won (CR 407.2).
  Spec.it s "CR 108.3/730.4a gameplay: a card whose owner changed keeps its history through a subgame wish" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    efreet <- S.printingOf s registry "Tempest Efreet"
    let g0 = Setup.gameWith GameSettings.plain {GameSettings.ante = True} S.bothPlayers
        (efreetId, g1) = S.addPermanent efreet S.alice g0
        (_, g2) = S.addHandCard signInBlood S.bob (S.landsFor plains S.alice 2 g1)
        (wishLibraryId, g3) = S.addLibraryCard burningWish S.alice g2
        g4 = stockLibrary mountain 10 S.bob (stockLibrary mountain 9 S.alice g3)
        (shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        board =
          g5
            { GameState.turn =
                (GameState.turn g5)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        -- bob declines the Efreet's 10 life (CR 608.2d), so the trade happens.
        trading :: Prompt.Prompt r -> r
        trading p = case p of
          Prompt.ChooseOptional {} -> OptionalDecision.Declines
          _ -> S.identityAnswer p
        traded = case Activatable.abilitiesFor efreetId board of
          [ability] -> S.runPure trading board (Activate.activateAbility S.alice efreetId ability >> Stack.resolveTop)
          _ -> board
        taken = filter (/= shahrazadId) (Game.zoneMembers Zone.Hand S.alice traded)
        wanted candidate = case candidate of
          OutsideCard.InAnotherGame oid -> List.elem oid taken
          OutsideCard.InPool _ -> False
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find wanted (fmap fst (NonEmpty.toList offered)))]
          Prompt.RandomFirstPlayer _ -> S.alice
          -- CR 407.2 in the subgame: never ante the wish.
          Prompt.RandomObject offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (/= wishLibraryId) (NonEmpty.toList offered))
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer traded Engine.priorityLoop)
    let report = Ante.ownershipChanges after
        reportFor printing = [(OwnershipChange.startingOwner c, OwnershipChange.owner c) | c <- Foldable.toList report, Game.printingOfObject (OwnershipChange.object c) after == Just printing]
    Spec.assertEqWith s "CR 108.3 the report names bob as the Sign in Blood's first owner and alice as its owner" (reportFor signInBlood) [(S.bob, S.alice)]
    Spec.assertEqWith s "and the Efreet the other way" (reportFor efreet) [(S.alice, S.bob)]
    Spec.assertEqWith s "CR 407.2 and alice, who won the subgame, took bob's subgame ante card" (reportFor mountain) [(S.bob, S.alice)]
    Spec.assertEqWith s "CR 407.3 the Efreet traded bob's Sign in Blood into alice's hand" (printingsIn Zone.Hand S.alice traded) [shahrazad, signInBlood]
    Spec.assertEqWith s "CR 730.4a/730.5 the wish took it, and it came home to her main-game library" (printingsIn Zone.Hand S.alice after, length (filter (== signInBlood) (printingsIn Zone.Library S.alice after))) ([], 1)
  Spec.it s "CR 407.3/730.4 a main-game ante card is not outside the subgame; the same card in exile is" $ do
    sign <- S.printingOf s registry "Sign in Blood"
    let parent = Setup.gameWith GameSettings.plain {GameSettings.ante = True} S.bothPlayers
        outsideOf zone = let (oid, placed) = S.addObjectIn zone sign S.alice parent in Map.member oid (GameState.outsideObjects (Setup.subgameStateFrom S.alice placed))
    Spec.assertEqWith s "CR 407.3 the ante card is not outside the subgame" (outsideOf Zone.Ante) False
    Spec.assertEqWith s "CR 730.4 the same card in exile is" (outsideOf Zone.Exile) True
  -- CR 702.26b/730.4: alice's Breezekeeper phased out in her untap step, and
  -- in her main phase she casts Shahrazad. Inside the subgame Living Wish ("a
  -- creature or land card") is not offered the Breezekeeper: a phased-out
  -- permanent is treated as though it does not exist, CR 730.4 does not name
  -- one, and the wish's ruling of 2007-09-16 says it can't acquire one. The
  -- answerer takes the Breezekeeper if offered, so the reading the rule rejects
  -- reddens every assertion below.
  --
  -- The sizing is the Death Wish scenario's: one {1}{G} wish and eight Forests for
  -- alice, castable on her turn 3, and bob decks on turn 6.
  Spec.it s "CR 702.26b/730.4 gameplay: a subgame's Living Wish is not offered a phased-out Breezekeeper" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    livingWish <- S.printingOf s registry "Living Wish"
    breezekeeper <- S.printingOf s registry "Breezekeeper"
    let (keeperId, g1) = S.addPermanent breezekeeper S.alice (Setup.emptyGame S.bothPlayers)
        g2 = Phasing.phaseOut (PhasedOut.Directly S.alice) keeperId g1
        g3 = S.landsFor plains S.alice 2 g2
        g4 = stockLibrary mountain 9 S.bob (stockLibrary forest 8 S.alice (stockLibrary livingWish 1 S.alice g3))
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before =
          g5
            { GameState.turn =
                (GameState.turn g5)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        isKeeper candidate = case candidate of
          OutsideCard.InAnotherGame oid -> oid == keeperId
          OutsideCard.InPool _ -> False
        -- Records whether the Breezekeeper was ever on offer, and asks for it.
        recordOffers :: Prompt.Prompt r -> State.State (Bool, Bool) r
        recordOffers p = case p of
          Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> do
            State.modify' (\(_, seen) -> (True, seen || any (isKeeper . fst) (NonEmpty.toList offered)))
            pure [Maybe.fromMaybe (fst (NonEmpty.head offered)) (List.find isKeeper (fmap fst (NonEmpty.toList offered)))]
          Prompt.RandomFirstPlayer _ -> pure S.alice
          _ -> pure (S.castAnswer p)
        ((_, after), (wished, offeredKeeper)) = State.runState (Engine.runGame recordOffers before Engine.priorityLoop) (False, False)
    Spec.assertEqWith s "setup: the Breezekeeper is phased out" (Phasing.isPhasedOut keeperId before) True
    Spec.assertEqWith s "the subgame's Living Wish resolved and asked" wished True
    Spec.assertEqWith s "CR 702.26b the phased-out Breezekeeper was never offered" offeredKeeper False
    Spec.assertEqWith s "CR 702.26b and it is still alice's, phased out in the main game" (fmap Object.owner (Game.lookupObject keeperId after), Phasing.phasedOutStatus keeperId after) (Just S.alice, Just (PhasedOut.Directly S.alice))
    Spec.assertEqWith s "CR 730.1a the subgame did not decide the main game" (GameState.result after) Nothing
  -- CR 400.11b: a destination that is not the hand. The Raven's Warning
  -- ({1}{W}{U} Enchantment -- Saga; name, cost, type line and Oracle text checked
  -- against api.scryfall.com 2026-09-16, paper printing `khm`) is the only
  -- printing that says one -- Scryfall o:"outside the game" -is:digital
  -- -is:funny, 2026-09-16, twenty-one hits, of which the other non-hand
  -- destination is Research's "shuffle up to four cards you own from outside the
  -- game into your library", which the Research group below drives.
  --
  -- Chapter III alone is what this case drives: the Saga is placed with two lore
  -- counters, so CR 715.3c's turn-based action puts the third on and the final
  -- chapter is the one trigger on the stack.
  --
  -- ONE card already in her library, so the top is a different place from the
  -- bottom; TWO cards in the pool, so which one arrives is a choice she is asked
  -- and this answerer pins by naming the last offered. Every printing distinct.
  Spec.it s "CR 400.11b The Raven's Warning puts the card it found on top of her library and not into her hand" $ do
    ravens <- S.printingOf s registry "The Raven's Warning"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    psychicMiasma <- S.printingOf s registry "Psychic Miasma"
    let (sagaId, placed) = S.addPermanent ravens S.alice (Setup.emptyGame S.bothPlayers)
        stocked = stockLibrary dragon 1 S.alice placed
        -- Interned in LIST ORDER, wishBoard's reason: the ids ascend with the
        -- list, so Psychic Miasma is what the prompt offers last.
        intern printing (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, 1)], gs2)
        (entries, interned) = List.foldl' (flip intern) ([], stocked) [signInBlood, psychicMiasma]
        stock p = p {Player.outsideTheGame = Map.fromList entries}
        withPool = interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}
        ready =
          Game.overTurn
            ( \registers ->
                registers
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice
                  }
            )
            (S.addCounter CounterKind.Lore 2 sagaId withPool)

        advanced = S.runPure S.identityAnswer ready (Engine.runTurnBasedActions Phase.PrecombatMain)
        answeringLast :: Prompt.Prompt r -> r
        answeringLast p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> [List.last (fmap fst (foldr (:) [] offered))]
          _ -> exercising p
        settled = S.runPure answeringLast advanced Engine.settleForPriority
        resolved = S.runPure answeringLast settled Stack.resolveTop
    -- THE BEHAVIOUR, ahead of every proxy, and one assertion for both readings the
    -- destination carries: the ZONE, which the wish cycle's hand would fail, and
    -- the END, which LibraryPosition.defaultValue's bottom would fail.
    Spec.assertEqWith s "CR 400.11b/401.2 the card she named is on TOP of her library, above the card already there" (printingsIn Zone.Library S.alice resolved) [psychicMiasma, dragon]
    Spec.assertEqWith s "CR 400.11b and nothing reached her hand" (printingsIn Zone.Hand S.alice resolved) []
    Spec.assertEqWith s "CR 400.11b and only the copy she named left the pool" (Map.size (poolOf S.alice resolved)) 1
    -- The fixture's own preconditions, AFTER the behaviour so neither can absorb a
    -- mutation the library is what observes.
    Spec.assertEqWith s "setup: CR 715.3c's turn-based action put the third lore counter on" (S.counterOf CounterKind.Lore sagaId advanced) 3
    Spec.assertEqWith s "setup: her library held the one card and no more before chapter III resolved" (printingsIn Zone.Library S.alice settled) [dragon]
  -- A count other than one. Research ({G}{U} Instant, the left half of Research
  -- // Development: "Shuffle up to four cards you own from outside the game into
  -- your library."; read off card_faces at api.scryfall.com 2026-09-27) on
  -- researchBoard, answered by position in the offer. Two copies of Sign in
  -- Blood are named, so one answer spends a printing twice (CR 100.4a); the
  -- shuffle REVERSES what it is offered, so the order the library comes back in
  -- says the shuffle happened after the cards arrived.
  Spec.it s "CR 400.11c Research shuffles the cards she chose, two copies of one printing among them, into her library" $ do
    (gs, researchId, (signInBlood, _, dragon, _), before) <- researchBoard s registry
    let resolved = S.runPure (researching [0, 0, 2]) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 701.24a the three cards she named are in her library, which was shuffled after they arrived" (printingsIn Zone.Library S.alice resolved) (reverse before <> [signInBlood, signInBlood, dragon])
    Spec.assertEqWith s "CR 400.11b both copies and the Dragon left the pool, and the other two cards did not" (Map.elems (poolOf S.alice resolved)) [1, 1]
    Spec.assertEqWith s "and nothing reached her hand" (printingsIn Zone.Hand S.alice resolved) []
  -- "Up to" reaches none, and CR 701.24d shuffles the library all the same: the
  -- same board, answered with nothing.
  Spec.it s "CR 701.24d Research bringing in nothing still shuffles her library" $ do
    (gs, researchId, _, before) <- researchBoard s registry
    let resolved = S.runPure (researching []) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 701.24d her library holds what it did, shuffled" (printingsIn Zone.Library S.alice resolved) (reverse before)
    Spec.assertEqWith s "CR 400.11b and the pool is untouched" (Map.elems (poolOf S.alice resolved)) [2, 1, 1, 1]
  -- The count is a ceiling on the answer: five named, the first four arrive and
  -- the fifth card stays out (Event.settleOutside).
  Spec.it s "CR 400.11c Research brings in no more than four however many she names" $ do
    (gs, researchId, (signInBlood, miasma, dragon, _), before) <- researchBoard s registry
    let resolved = S.runPure (researching [0, 0, 1, 2, 3]) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 400.11c four cards arrived, in the order named, and the shuffle followed" (printingsIn Zone.Library S.alice resolved) (reverse before <> [signInBlood, signInBlood, miasma, dragon])
    Spec.assertEqWith s "and the fifth is still outside the game" (Map.elems (poolOf S.alice resolved)) [1]
  -- CR 201.2b / 608.2d: Extrapolate the Impossible ({1}{B} sorcery, "You may
  -- reveal exactly two cards you own with different names from outside the
  -- game. An opponent chooses one of them. You put that card into your hand.";
  -- api.scryfall.com 2026-11-06, whose ruling keeps the other card outside the
  -- game) on extrapolateBoard: three seats, so "an opponent" is a choice.
  Spec.it s "CR 608.2d Extrapolate the Impossible brings in the card the opponent she named chose, and the other stays outside the game" $ do
    (gs, spellId, (signInBlood, miasma, _)) <- extrapolateBoard s registry [(0, 1), (1, 1)]
    let resolved = resolveWish (extrapolating [] S.carol) spellId gs
    Spec.assertEqWith s "CR 608.2d carol chose the card offered last, which is the one in her hand" (printingsIn Zone.Hand S.alice resolved) [miasma]
    Spec.assertEqWith s "and the card carol did not choose is still outside the game" (Map.keys (Map.filter (> 0) (poolOf S.alice resolved))) (Maybe.mapMaybe (`printingIdIn` gs) [signInBlood])
  -- The same board, bob named: bob takes the card offered first, so which seat
  -- chose is what decides the card.
  Spec.it s "CR 608.2d Extrapolate the Impossible asks the opponent she names, not another" $ do
    (gs, spellId, (signInBlood, _, _)) <- extrapolateBoard s registry [(0, 1), (1, 1)]
    let resolved = resolveWish (extrapolating [] S.bob) spellId gs
    Spec.assertEqWith s "CR 608.2d bob chose the card offered first" (printingsIn Zone.Hand S.alice resolved) [signInBlood]
  -- CR 201.2b: two copies share a name, so naming one card twice reveals it and
  -- the next card with another name, of which carol takes the last.
  Spec.it s "CR 201.2b Extrapolate the Impossible cannot reveal two copies of one card" $ do
    (gs, spellId, (signInBlood, miasma, _)) <- extrapolateBoard s registry [(0, 2), (1, 1), (2, 1)]
    let named = Maybe.mapMaybe (`printingIdIn` gs) [signInBlood, signInBlood]
        resolved = resolveWish (extrapolating (fmap OutsideCard.InPool named) S.carol) spellId gs
    Spec.assertEqWith s "CR 201.2b the second card revealed was Psychic Miasma, which carol chose" (printingsIn Zone.Hand S.alice resolved) [miasma]
    Spec.assertEqWith s "and both copies of Sign in Blood stay outside the game" (Map.elems (poolOf S.alice resolved)) [2, 1]
  -- CR 609.3, Search.exactly's reading: a pool with one name reveals that card,
  -- and the opponent's choice of one of one is no choice.
  Spec.it s "CR 609.3 Extrapolate the Impossible with one name outside the game brings that card in" $ do
    (gs, spellId, (signInBlood, _, _)) <- extrapolateBoard s registry [(0, 2)]
    let resolved = resolveWish (extrapolating [] S.carol) spellId gs
    Spec.assertEqWith s "CR 609.3 one Sign in Blood reached her hand" (printingsIn Zone.Hand S.alice resolved) [signInBlood]
    Spec.assertEqWith s "and the other copy stays outside the game" (Map.elems (poolOf S.alice resolved)) [1]
    let choseOutside r = case r of
          Response.ChoseFromOutsideTheGame _ -> True
          _ -> False
        asked = answersFor (extrapolating [] S.carol) gs (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertEqWith s "and one name is no choice: nobody was asked which cards" (length (filter choseOutside asked)) 0
  -- Opposition Agent's "they exile each card they find" reaches a card found
  -- outside the game too: bob's Agent, and alice searching her library and
  -- outside the game. Without the Agent the Fog reaches her hand (the
  -- outside-the-game scenarios on Invasion of Arcavios).
  Spec.it s "CR 701.23j an opponent's Opposition Agent exiles the card found outside the game, linked to the Agent" $ do
    board <- arcaviosBoard s registry
    agentCard <- S.printingOf s registry "Opposition Agent"
    let (agentId, withAgent) = S.addPermanent agentCard S.bob (arcaviosState board)
        resolved = resolveArcavios (arcaviosAnswer [SearchPlace.InZone Zone.Library, SearchPlace.OutsideTheGame]) board {arcaviosState = withAgent}
        exiled = Game.zoneMembers Zone.Exile S.alice resolved
    Spec.assertEqWith s "the Fog from outside the game is in her exile" (printingsIn Zone.Exile S.alice resolved) [arcaviosFog board]
    Spec.assertEqWith s "and her hand is empty" (printingsIn Zone.Hand S.alice resolved) []
    Spec.assertEqWith s "CR 607.1c the exiled Fog is linked to the Agent" (fmap (\oid -> fmap ExileLink.source (Map.lookup oid (GameState.exiledWith resolved))) exiled) [Just agentId]
  -- CR 701.23j with a reveal for later clauses: Turtles Forever ({3}{W}
  -- instant, "Search your library and/or outside the game for exactly four
  -- legendary creature cards you own with different names, then reveal those
  -- cards. An opponent chooses two of them. Put the chosen cards into your hand
  -- and shuffle the rest into your library."; api.scryfall.com 2026-11-12) on
  -- turtlesBoard, bob taking the two cards the library did not give.
  Spec.it s "CR 701.23j Turtles Forever reveals cards found outside the game, and the opponent's two go to her hand" $ do
    board <- turtlesBoard s registry
    let resolved = resolveWish (turtling True (turtlesFound board)) (turtlesSpell board) (turtlesState board)
    Spec.assertEqWith s "CR 608.2c bob chose the two cards from outside the game, which are in her hand" (namesIn Zone.Hand S.alice resolved) (turtleNames ["Ambassador Laquatus", "Angus Mackenzie"])
    Spec.assertEqWith s "and the library's two finds stay in her library" (namesIn Zone.Library S.alice resolved) (turtleNames ["Anafenza, the Foremost", "Arcanis the Omnipotent", "Plains"])
    Spec.assertEqWith s "CR 201.2b the Arcanis outside the game shares a name with the library's, and stays out" (Map.toList (poolOf S.alice resolved)) (fmap (\printingId -> (printingId, 1)) (Maybe.maybeToList (printingIdIn (turtlesArcanis board) (turtlesState board))))
  -- Search.exactly over both halves: declining everything out there, the two
  -- cards with other names are found anyway, so bob's two are still them.
  Spec.it s "CR 101.1 Turtles Forever's exactly four counts the cards outside the game" $ do
    board <- turtlesBoard s registry
    let resolved = resolveWish (turtling False (turtlesFound board)) (turtlesSpell board) (turtlesState board)
    Spec.assertEqWith s "CR 101.1 four were found, and bob's two from outside the game are in her hand" (namesIn Zone.Hand S.alice resolved) (turtleNames ["Ambassador Laquatus", "Angus Mackenzie"])
    Spec.assertEqWith s "and the pool keeps only the Arcanis" (Map.elems (poolOf S.alice resolved)) [1]
    let searched r = case r of
          Response.Searched _ -> True
          _ -> False
        asked = answersFor (turtling False (turtlesFound board)) (turtlesState board) (S.cast S.alice (turtlesSpell board) >> Stack.resolveTop)
    Spec.assertEqWith s "CR 701.23j and her library's two, which the cards out there complete, were asked once" (length (filter searched asked)) 1
  -- CR 400.11: a card the search revealed outside the game stays out there
  -- until the later clause brings it in, so the move to her hand is no move
  -- from her library. Volatile Rift ({R/G}{R/G} Enchantment, "Vivid --
  -- Whenever a creature card is put into your hand from your library, it
  -- perpetually gets +X/+0, where X is the number of colors among permanents
  -- you control."; data/cards/, Oracle checked against Scryfall 2026-09-27)
  -- beside turtlesBoard, bob taking the library's Anafenza and one card from
  -- out there.
  Spec.it s "CR 400.11 Turtles Forever's card from outside the game reaches her hand from no library" $ do
    board <- turtlesBoard s registry
    rift <- S.printingOf s registry "Volatile Rift"
    let withRift = snd (S.addPermanent rift S.alice (turtlesState board))
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseCardFromAmong _ _ _ pool | Just anafenza <- List.find (`elem` take 1 (turtlesFound board)) (NonEmpty.toList pool) -> anafenza
          _ -> turtling True (turtlesFound board) p
        resolved = resolveWish answer (turtlesSpell board) withRift
        settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
        resolveAll gs = if null (TurnRegisters.stack (GameState.turn gs)) then gs else resolveAll (S.runPure S.identityAnswer gs Stack.resolveTop)
        after = resolveAll settled
        handPT name gs = [S.powerToughnessOf oid gs | oid <- Game.zoneMembers Zone.Hand S.alice gs, fmap S.nameOf (Game.cardOf oid gs) == Just name]
    Spec.assertEqWith s "CR 608.2c bob chose Anafenza and Ambassador Laquatus" (namesIn Zone.Hand S.alice resolved) (turtleNames ["Ambassador Laquatus", "Anafenza, the Foremost"])
    Spec.assertEqWith s "CR 603.2 Volatile Rift triggered once, for the card from her library" (length (TurnRegisters.stack (GameState.turn settled))) 1
    Spec.assertEqWith s "and Anafenza perpetually got +2/+0 for the Rift's two colors" (handPT (CardName.MkCardName (Text.pack "Anafenza, the Foremost")) after) [Just (6, 4)]
    Spec.assertEqWith s "CR 400.11 while Ambassador Laquatus, brought in from outside the game, did not" (handPT (CardName.MkCardName (Text.pack "Ambassador Laquatus")) after) [Just (1, 3)]

-- alice with a Forest and an Island, Research // Development in hand -- {G}{U}
-- pays Research and nothing pays Development --
-- Goblin Piker and Cancel in her library, and five cards over four printings
-- outside the game: one more card than "up to four" takes, and two copies of
-- Sign in Blood. The pool printings are interned in list order, so the offer
-- lists them Sign in Blood, Psychic Miasma, Hoarding Dragon, Lightning Bolt.
-- Returns the pool's four printings and the library before the cast, top first.
researchBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, (Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing), [Printing.Printing])
researchBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  research <- S.printingOf s registry "Research"
  piker <- S.printingOf s registry "Goblin Piker"
  cancel <- S.printingOf s registry "Cancel"
  signInBlood <- S.printingOf s registry "Sign in Blood"
  miasma <- S.printingOf s registry "Psychic Miasma"
  dragon <- S.printingOf s registry "Hoarding Dragon"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))
      (board, researchId) = S.handOne research lands
      (_, withPiker) = S.addLibraryCard piker S.alice board
      (_, stocked) = S.addLibraryCard cancel S.alice withPiker
      intern (printing, n) (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, n)], gs2)
      (entries, interned) = List.foldl' (flip intern) ([], stocked) [(signInBlood, 2), (miasma, 1), (dragon, 1), (bolt, 1)]
      stock p = p {Player.outsideTheGame = Map.fromList entries}
      ready = interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}
  pure (ready, researchId, (signInBlood, miasma, dragon, bolt), printingsIn Zone.Library S.alice ready)

-- alice casts one half of a split card by name (CR 709.3), paying from her lands.
castHalf :: String -> ObjectId.ObjectId -> Game ()
castHalf half oid = Cast.castSpell S.manaPerformer S.alice oid (CardName.MkCardName (Text.pack half)) Facing.FaceUp

-- Research's two answers: the cards it brings in, named by POSITION in the offer
-- so one position may be named twice, and a shuffle that reverses whatever it
-- is offered, which is what makes one visible.
researching :: [Int] -> Prompt.Prompt r -> r
researching picks p = case p of
  Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> Maybe.mapMaybe (\i -> Maybe.listToMaybe (drop i (fmap fst (NonEmpty.toList offered)))) picks
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

-- Invasion of Arcavios's board: alice with five Islands and the Siege in hand;
-- two Islands in her library; a Sign in Blood in her graveyard; and an Island
-- and a Fog outside the game, the Island interned first so an offer the filter
-- did not narrow would lead with it.
data ArcaviosBoard = MkArcaviosBoard
  { arcaviosState :: GameState.GameState,
    arcaviosSpell :: ObjectId.ObjectId,
    arcaviosFog :: Printing.Printing
  }

arcaviosBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m ArcaviosBoard
arcaviosBoard s registry = do
  island <- S.printingOf s registry "Island"
  invasion <- S.printingOf s registry "Invasion of Arcavios"
  signInBlood <- S.printingOf s registry "Sign in Blood"
  fog <- S.printingOf s registry "Fog"
  let (board, spellId) = S.handOne invasion (S.landsInPlay island 5)
      withIslands = stockLibrary island 2 S.alice board
      buried = snd (S.addGraveyardCard signInBlood S.alice withIslands)
  pure (MkArcaviosBoard (stockPool [(island, 1), (fog, 1)] buried) spellId fog)

-- Cast the Siege, resolve it, and resolve its enters trigger.
resolveArcavios :: (forall r. Prompt.Prompt r -> r) -> ArcaviosBoard -> GameState.GameState
resolveArcavios answer board =
  let entered = S.runPure answer (arcaviosState board) (S.cast S.alice (arcaviosSpell board) >> Stack.resolveTop)
      settled = S.runPure answer entered Engine.settleForPriority
   in S.runPure answer settled Stack.resolveTop

-- The places she takes, finding nothing in them, and everything she is offered
-- from outside the game taken.
arcaviosAnswer :: [SearchPlace.SearchPlace] -> Prompt.Prompt r -> r
arcaviosAnswer places p = case p of
  Prompt.ChooseSearchZones {} -> Set.fromList places
  Prompt.Search {} -> []
  Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> fmap fst (NonEmpty.toList offered)
  _ -> S.identityAnswer p

-- Turtles Forever's board: alice with four Plains and the instant in hand;
-- Anafenza, the Foremost, Arcanis the Omnipotent and a Plains in her library;
-- and Arcanis the Omnipotent, Ambassador Laquatus and Angus Mackenzie outside
-- the game. The library interns Arcanis first, so the offer out there leads
-- with the copy whose name the library's find has taken.
data TurtlesBoard = MkTurtlesBoard
  { turtlesState :: GameState.GameState,
    turtlesSpell :: ObjectId.ObjectId,
    -- | The library's Anafenza and Arcanis.
    turtlesFound :: [ObjectId.ObjectId],
    turtlesArcanis :: Printing.Printing
  }

turtlesBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m TurtlesBoard
turtlesBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  turtles <- S.printingOf s registry "Turtles Forever"
  anafenza <- S.printingOf s registry "Anafenza, the Foremost"
  arcanis <- S.printingOf s registry "Arcanis the Omnipotent"
  laquatus <- S.printingOf s registry "Ambassador Laquatus"
  angus <- S.printingOf s registry "Angus Mackenzie"
  let (board, spellId) = S.handOne turtles (S.landsInPlay plains 4)
      withPlains = snd (S.addLibraryCard plains S.alice board)
      (arcanisId, withArcanis) = S.addLibraryCard arcanis S.alice withPlains
      (anafenzaId, stocked) = S.addLibraryCard anafenza S.alice withArcanis
  pure (MkTurtlesBoard (stockPool [(arcanis, 1), (laquatus, 1), (angus, 1)] stocked) spellId [anafenzaId, arcanisId] arcanis)

-- Turtles Forever's answers: both places, every library match, everything out
-- there or nothing, and bob taking what is offered beside the library's finds.
turtling :: Bool -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
turtling takeOutside fromLibrary p = case p of
  Prompt.ChooseSearchZones _ _ places -> places
  Prompt.Search _ _ matches _ -> matches
  Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> if takeOutside then fmap fst (NonEmpty.toList offered) else []
  Prompt.ChooseCardFromAmong _ _ _ pool -> Maybe.fromMaybe (NonEmpty.head pool) (List.find (`notElem` fromLibrary) (NonEmpty.toList pool))
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

-- The names of the cards in a zone, sorted.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs = List.sort (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))

turtleNames :: [String] -> [CardName.CardName]
turtleNames = List.sort . fmap (CardName.MkCardName . Text.pack)

-- alice with two Synthetic Wishful Djinns, the first carrying a +1/+1 counter,
-- two library cards of one printing and one card of another outside the game.
-- Answers the board and the two Djinns' ids, the bigger first.
djinnBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
djinnBoard plains djinn stock outside =
  let (pumped, g1) = S.addPermanent djinn S.alice (S.landsInPlay plains 1)
      (plain, g2) = S.addPermanent djinn S.alice g1
      g3 = stockLibrary stock 2 S.alice (S.addCounter CounterKind.PlusOnePlusOne 1 pumped g2)
      (outsideId, g4) = Game.intern outside g3
      pool p = p {Player.outsideTheGame = Map.singleton outsideId 1}
   in (g4 {GameState.players = Map.adjust pool S.alice (GameState.players g4)}, pumped, plain)

-- CR 616.1: name the replacement whose source is `preferred`, filtering the
-- offered entries rather than building one; every other prompt takes the
-- default.
preferringRow :: ObjectId.ObjectId -> Prompt.Prompt r -> r
preferringRow preferred p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- alice with five untapped lands, a Ring of Ma'rûf on the battlefield, two
-- library cards of one printing and one card of another outside the game, with
-- the Ring's ability activated and resolved when `arm` is True and untouched when
-- it is False. One builder for both legs, so the two boards differ in that alone.
--
-- Five lands for the {5}, with the {T} and the exile beside it; CR 113.6m is what
-- keeps that ability working on the battlefield alone. The ObjectId returned is
-- the Ring's, so a case can read what the cost did to it.
ringBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (GameState.GameState, ObjectId.ObjectId)
ringBoard plains ring stock outside arm =
  let (ringId, g1) = S.addPermanent ring S.alice (S.landsInPlay plains 5)
      g2 = stockLibrary stock 2 S.alice g1
      (outsideId, g3) = Game.intern outside g2
      pool p = p {Player.outsideTheGame = Map.singleton outsideId 1}
      g4 = g3 {GameState.players = Map.adjust pool S.alice (GameState.players g3)}
      final = case Face.activatedAbilities (S.combinedFace ring) of
        ability : _ | arm -> S.runPure S.identityAnswer g4 (Activate.activateAbility S.alice ringId ability >> Stack.resolveTop)
        _ -> g4
   in (final, ringId)
