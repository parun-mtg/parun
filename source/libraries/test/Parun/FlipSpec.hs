{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 710 flip cards end to end: Parun.Types.Layout's Flip arm and the two
-- Parun.Engine.Card functions that read it (CR 710.2's normal view through
-- `combined`, CR 710.1b/710.1c's alternative view through `flippedFace`),
-- Parun.Types.Object's flipped status (CR 110.5) with the substitution
-- Parun.Engine.Game.resolveFaceFor and `namesFor` make of it, the writer
-- Parun.Engine.Game.flipPermanent with its `flipsOver` gate, the Effect.Flip arm
-- of Parun.Engine.Resolve, and CR 710.4's clearing on departure through
-- Parun.Types.Object.newIncarnation.
--
-- Every case runs against the printed Akki Lavarunner // Tok-Tok, Volcano Born,
-- which is CR 710.2's own worked Example: a {3}{R} 1/1 Goblin Warrior with haste
-- and "whenever this creature deals damage to an opponent, flip it", whose
-- alternative half is the legendary 2/2 Goblin Shaman Tok-Tok, Volcano Born with
-- protection from red. It was picked because the rule prints it, and because the
-- two halves disagree about every characteristic CR 710.2 swaps while agreeing
-- about the two CR 710.1c withholds.
--
-- CR 603.2's recipient half -- Akki's trigger names an OPPONENT -- is
-- data/scenarios/flip/'s Harm's Way and Soul's Fire scenarios.
--
-- CR 710.5's alternative name needs nothing here: Prompt.ChooseCardName offers
-- no candidate list, so a player may already choose it -- Parun.AdventureSpec
-- says the same of CR 716.5.
--
-- CR 710.2's flip in play, CR 707.2 / 707.3's copies of it and CR 707.9b's
-- exceptions on a copy that flips are data/scenarios/flip/'s too. CR 110.5c's
-- flipped permanent that copies a Room is the Mirrorweave case last.
--
-- CR 731.2h's merged permanent containing a flip card is Parun.MutateSpec's, this
-- card being the pool's only flip printing and Cubwarden what merges with it.
module Parun.FlipSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Filter as Filter
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Board as Board
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Protection as Protection
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RoomHalf as RoomHalf
import qualified Parun.Types.Staged as Staged
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.Zone as Zone

akkiName, tokTokName :: CardName.CardName
akkiName = CardName.MkCardName (Text.pack "Akki Lavarunner")
tokTokName = CardName.MkCardName (Text.pack "Tok-Tok, Volcano Born")

-- Everything the two halves disagree about, read through the projection, as one
-- tuple: names, power and toughness, subtypes, the legendary supertype, and
-- protection from red. Asserted whole so a case names the half rather than five
-- independent facts, and so a change that moved only one of them cannot pass.
halfReadings ::
  ObjectId.ObjectId ->
  GameState.GameState ->
  (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set Supertype.Supertype, Bool)
halfReadings oid gs =
  ( Projection.namesOf oid gs,
    S.powerToughnessOf oid gs,
    Projection.subtypesOf oid gs,
    Projection.supertypesOf oid gs,
    Projection.hasKeyword (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.Type.HasColor Color.Red, Protection.spares = Nothing}) oid gs
  )

normalHalf, alternativeHalf :: (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set Supertype.Supertype, Bool)
normalHalf = (Set.singleton akkiName, Just (1, 1), Set.fromList [Subtype.Goblin, Subtype.Warrior], Set.empty, False)
alternativeHalf = (Set.singleton tokTokName, Just (2, 2), Set.fromList [Subtype.Goblin, Subtype.Shaman], Set.singleton Supertype.Legendary, True)

-- The two characteristics CR 710.1c withholds from the swap, read as a pair:
-- mana value and colour.
costReadings :: ObjectId.ObjectId -> GameState.GameState -> (Maybe Integer, Set.Set Color.Color)
costReadings oid gs = (PC.manaValue (Projection.project oid gs), Projection.colorsOf oid gs)

-- CR 710.2's Example asks two effects about the card, and both go through one
-- predicate: "search your library for a legendary card" and "legendary creatures
-- get +2/+2" each evaluate the supertype the object HAS. Asked here as the
-- filter atom a card's own text compiles to, against the projection's view.
--
-- Read through the filter rather than through a card, because no printing in
-- data/cards/ says either of the Example's two sentences: grepping the corpus
-- for HasSupertype (2026-09-07) returns Kellan Joins Up's enters TRIGGER,
-- Cleopatra, Exiled Pharaoh's target pool, and Undercity's and Synthetic Tidal
-- Waste's nonbasic-land clauses -- a legendary-filtered library search and a
-- static pump over legendary creatures are both absent. A card printing either
-- would refute the claim, not the rule; Cleopatra's "up to two other target
-- legendary creatures" is the nearest the pool comes, and it reaches this same
-- predicate.
isLegendary :: ObjectId.ObjectId -> GameState.GameState -> Bool
isLegendary oid gs =
  Filter.matches
    (Filter.contextIn gs (Just S.alice) Nothing)
    (Projection.viewOfObject oid gs)
    (Filter.Type.HasSupertype Supertype.Legendary)

-- alice's Akki alone against bob, with combat about to start. Nothing else is on
-- the battlefield, so the only thing that can deal bob combat damage is the
-- creature this unit is about.
akkiDuel :: Board.Board
akkiDuel = S.duel S.beginningOfCombat [S.settled "akki" "Akki Lavarunner"] []

-- Akki attacks bob, unblocked, and the whole combat phase runs -- so CR 510.2's
-- damage, CR 603.3's trigger placement and CR 608's resolution all happen inside
-- the engine rather than being poked in.
attackScript :: Seq.Seq Timed.Timed
attackScript = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "akki"])]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Flip" $ do
  -- CR 710.2, first sentence: "in every zone other than the battlefield, and
  -- also on the battlefield before the permanent flips, a flip card has only the
  -- normal characteristics of the card."
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's other
  -- two-frame layout would take: a combined view would name this permanent
  -- "Akki Lavarunner//Tok-Tok, Volcano Born", make it legendary, and give it
  -- protection from red while it is still Akki.
  Spec.it s "CR 710.2 an unflipped permanent shows its normal half and only that" $ do
    akki <- S.printingOf s registry "Akki Lavarunner"
    let (oid, gs) = S.addPermanent akki S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "every reader sees Akki Lavarunner" (halfReadings oid gs) normalHalf
    Spec.assertEqWith s "CR 710.1c: {3}{R} is mana value 4 and red" (costReadings oid gs) (Just 4, Set.singleton Color.Red)
  -- CR 710.2's Example, the search half: "an effect that says 'search your
  -- library for a legendary card' can't find this flip card." A card in a
  -- library is in a zone other than the battlefield, so rule 710.2's first
  -- sentence leaves it the normal half's type line -- which prints no supertype.
  Spec.it s "CR 710.2 the flip card in a library is not legendary" $ do
    akki <- S.printingOf s registry "Akki Lavarunner"
    let (oid, gs) = S.addObjectIn Zone.Library akki S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (not (isLegendary oid gs)) "a legendary search must not find the flip card"
    Spec.assertEqWith s "and it is named for its normal half" (Projection.namesOf oid gs) (Set.singleton akkiName)
  -- CR 710.4: "if a flipped permanent leaves the battlefield, it retains no
  -- memory of its status." The flipped permanent is put into its owner's hand
  -- and then back onto the battlefield; CR 400.7's new object arrives unflipped,
  -- so every reader is Akki's again.
  Spec.it s "CR 710.4 a flipped permanent that leaves the battlefield comes back unflipped" $ do
    built <- S.buildBoardOrFail s registry akkiDuel
    case Map.lookup (Label.MkLabel (Text.pack "akki")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the Akki alias"
      Just oid -> do
        (_, flipped) <- S.runScriptOrFail s attackScript built S.combatGame
        Spec.assertEqWith s "flipped first" (halfReadings oid flipped) alternativeHalf
        let (inHand, bounced) = S.runPureWith S.identityAnswer flipped (Event.changeZoneReturning (pure ()) oid Zone.Hand)
        case Foldable.toList inHand of
          [handId] -> do
            let (back, returned) = S.runPureWith S.identityAnswer bounced (Event.changeZoneReturning (pure ()) handId Zone.Battlefield)
            case Foldable.toList back of
              [returnedId] -> do
                Spec.assertEqWith s "CR 710.4: the returning permanent is Akki Lavarunner again" (halfReadings returnedId returned) normalHalf
                Spec.assertEqWith s "and its status is unflipped" (fmap Object.flipped (Game.lookupObject returnedId returned)) (Just False)
                Spec.assertBool s (not (isLegendary returnedId returned)) "so a legendary pump no longer reaches it"
              other -> Spec.assertFailure s ("expected one permanent back, got " <> show (length other))
          other -> Spec.assertFailure s ("expected one card in hand, got " <> show (length other))
  -- CR 707.3 / 110.5c over CR 709.5: a flipped Tok-Tok that Mirrorweave makes a
  -- copy of an Opalescence-animated Roaring Furnace // Steaming Sauna has the
  -- Room's halves against its OWN doors, both shut -- a nameless 0/0 Room its
  -- +1/+1 counter keeps alive as a 1/1. Its flipped status has no effect: the
  -- printed flip face would leave it a legendary 3/3 Tok-Tok.
  Spec.it s "CR 110.5c a flipped Tok-Tok Mirrorweave makes a copy of an animated Room is that Room" $ do
    akki <- S.printingOf s registry "Akki Lavarunner"
    room <- S.printingOf s registry "Roaring Furnace"
    opalescence <- S.printingOf s registry "Opalescence"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    island <- S.printingOf s registry "Island"
    let (_, withOpalescence) = S.addPermanent opalescence S.alice (S.landsInPlay island 4)
        (akkiId, withAkki) = S.addPermanent akki S.alice withOpalescence
        flipped = S.addCounter CounterKind.PlusOnePlusOne 1 akkiId (Game.withFlipped True akkiId withAkki)
        (roomId, withRoom) = S.addPermanent room S.alice flipped
        opened = withRoom {GameState.objects = Map.adjust (\o -> o {Object.unlockedHalves = Set.singleton RoomHalf.LeftHalf}) roomId (GameState.objects withRoom)}
        (staged, weaveId) = S.handOne mirrorweave opened
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just roomId) . Recipient.objectOf) . snd) sets
          _ -> S.identityAnswer p
        woven = S.runPure answer staged (S.cast S.alice weaveId >> Stack.resolveTop >> Engine.settleForPriority)
    -- THE gameplay assertion: the Room's characteristics, not Tok-Tok's.
    Spec.assertEqWith
      s
      "CR 709.5: the flipped Akki is a nameless, nonlegendary 1/1 Room"
      (Projection.namesOf akkiId woven, S.powerToughnessOf akkiId woven, isLegendary akkiId woven)
      (Set.empty, Just (1, 1), False)
    -- The proxies, after it: the status survived, and the copied Room is the
    -- 2/2 Roaring Furnace its open door makes it.
    Spec.assertEqWith s "CR 110.5c: the status is kept" (fmap Object.flipped (Game.lookupObject akkiId woven)) (Just True)
    Spec.assertEqWith
      s
      "control: before Mirrorweave it was a 3/3 Tok-Tok, and the Room a 2/2 Roaring Furnace"
      (Projection.namesOf akkiId opened, S.powerToughnessOf akkiId opened, Projection.namesOf roomId woven, S.powerToughnessOf roomId woven)
      (Set.singleton tokTokName, Just (3, 3), Set.singleton (CardName.MkCardName (Text.pack "Roaring Furnace")), Just (2, 2))
