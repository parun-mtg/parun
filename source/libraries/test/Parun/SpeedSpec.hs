{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Parun.Engine.Speed (CR 702.179, "start your engines!"), the CR 704.5aa
-- arm Parun.Engine.Sba runs from it, Parun.Types.Player's speed field,
-- Parun.Engine.Quantity's Speed arm (CR 702.179e/702.179f), CR 702.178a's max
-- speed gate -- Parun.Types.ActivatedAbility's condition, applied by
-- Parun.Engine.Projection.abilitiesGiven -- and CR 702.178b's zone clause, applied
-- by Parun.Engine.Activatable.zoneAbilitiesOf. Also Parun.Types.Effect's
-- DecreaseSpeed arm and its Parun.Types.SpeedDecrease payload, and the two
-- references Spikeshell Harrier reaches "that opponent" through --
-- Parun.Types.PlayerRef's HolderOfBound and Parun.Types.Filter's
-- IsControllerOfBound.
--
-- Gameplay-level throughout. Muraganda Raceway is the fixture for everything on
-- the battlefield: a Land printing "Start your engines!", "{T}: Add {C}" and "Max
-- speed — {T}: Add {C}{C}", so one card supplies both halves -- the resource and
-- something whose presence depends on it. maxSpeedZoneSpec needs a second
-- printing, because rule 702.178b is only observable off the battlefield.
--
-- Every case about how speed CHANGES gets there through the rules: started by the
-- state-based action, raised by an opponent losing life to a Sign in Blood or a
-- Lightning Bolt cast through the stack. The `atSpeed` helper at the foot of this
-- module writes a speed directly, and only cases about what READS speed use it --
-- rule 702.179d admits one increase a turn, so a board at 4 is four turns of
-- setup that would prove nothing the increase cases have not already proved.
--
-- cardDecreaseSpec is the group whose subject is not Muraganda Raceway. It
-- proves that a CARD's printed instruction reaches the speed opcode, through
-- Spikeshell Harrier, and its own header carries what is particular to it.
module Parun.SpeedSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Event.Trigger as Trigger
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Speed as Speed
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.AbilityTriggered as AbilityTriggered
import qualified Parun.Types.Action as A
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LifeChange as LifeChange
import qualified Parun.Types.Mana as Mana.Type
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PendingTrigger as PendingTrigger
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- This player's speed, as Player.speed holds it -- Nothing for a player who has
-- none (CR 702.179b), which is the state CR 704.5aa looks for and is NOT the same
-- as Just 0.
speedOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe (Maybe Natural.Natural)
speedOf pid gs = fmap Player.speed (Map.lookup pid (GameState.players gs))

-- Aim every target slot at Bob, so Sign in Blood's shared slot makes BOB the
-- player who loses life. Alice is the caster, so this is an OPPONENT losing life
-- -- CR 702.179d's event, and the whole point of the card being aimed rather than
-- left to whichever player a set happens to offer first.
atBob :: Prompt.Prompt r -> r
atBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- Cast the spell, resolve it, and settle -- which runs the state-based actions
-- (CR 704.3) and places whatever triggered (CR 603.3b). Then resolve whatever the
-- settle put on the stack and settle again, so an inherent speed trigger has both
-- happened and finished.
castResolveSettle :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castResolveSettle answer pid spellId gs =
  let cast = S.runPure answer gs (S.cast pid spellId)
      resolved = S.runPure answer cast (Stack.resolveTop >> Engine.settleForPriority)
   in S.runPure answer resolved (Stack.resolveTop >> Engine.settleForPriority)

-- How many mana ended up in Alice's pool after tapping this source, with every
-- prompt answered by `answer`.
pooledBy :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Int
pooledBy answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (S.tapForMana oid)) of
  Mana.Type.MkMana units -> length units

-- Answer Prompt.ChooseManaYield with the LAST candidate. The discriminator for
-- the max speed case: a Raceway at max speed offers two yields, {C} and {C}{C},
-- and the {C}{C} one is the ability CR 702.178a granted. Answering with the first
-- would pass whether or not the grant happened.
biggestYield :: Prompt.Prompt r -> r
biggestYield p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> last (foldr (:) [] candidates)
  _ -> S.identityAnswer p

-- Alice at speed `n`, with three untapped Swamps, one Loxodon Surveyor in her
-- graveyard and a stocked library, holding priority. Returns the graveyard card's
-- id.
--
-- The library is stocked because the ability DRAWS: an empty one would set CR
-- 704.5b's flag and lose Alice the game at the next settle, which would pass or
-- fail these cases for a reason that has nothing to do with rule 702.178.
--
-- Three Swamps because the activation cost is {3}; the Surveyor's own {2}{G} is
-- never paid, since it is in the graveyard rather than being cast.
surveyorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural.Natural -> (ObjectId.ObjectId, GameState.GameState)
surveyorBoard surveyor swamp filler n =
  let base = S.landsInPlay swamp 3
      (gyId, withCard) = S.addGraveyardCard surveyor S.alice base
      stocked = foldr (\_ g -> snd (S.addLibraryCard filler S.alice g)) withCard [1 .. (3 :: Int)]
   in ( gyId,
        atSpeed
          n
          S.alice
          ( stocked
              { GameState.turn =
                  (GameState.turn stocked)
                    { TurnRegisters.priority = Just S.alice
                    }
              }
          )
      )

-- Is this action an activation of that object's ability? Pinned to the OBJECT,
-- since the cases below are about which zone an ability is offered from.
isActivateOf :: ObjectId.ObjectId -> A.Action -> Bool
isActivateOf oid action = case action of
  A.Activate o _ -> o == oid
  A.Pass -> False
  A.Play {} -> False
  A.Cast {} -> False
  A.TurnFaceUp {} -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.PerformAsASorcery _ -> False
  A.TurnConspiracyFaceUp _ -> False
  A.Plot {} -> False
  A.Foretell _ -> False
  A.Suspend _ -> False
  A.PutCompanionIntoHand -> False
  A.RollPlanarDie -> False
  A.RerollShownDie _ _ -> False
  A.Ignore _ _ -> False
  A.EndEffect {} -> False
  A.MakeLaterPayment {} -> False
  A.PayLifeForMana _ _ -> False
  A.ActivateManaAbility _ -> False

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Speed" $ do
  maxSpeedSpec s registry
  maxSpeedZoneSpec s registry
  increaseSpec s registry
  cardDecreaseSpec s registry

-- CR 702.178b: "if an ability granted by a max speed ability states which zones
-- it functions from, the max speed ability that grants that ability functions
-- from those zones."
--
-- Loxodon Surveyor {2}{G} Creature -- Elephant Scout 3/3, "Start your engines!"
-- and "Max speed — {3}, Exile this card from your graveyard: Draw a card"
-- (checked against Scryfall; Aetherdrift prints five of these, one per colour).
-- The granted ability states its zone the way CR 113.6m does it -- through a cost
-- that moves the card out of the graveyard -- so the max speed ability that
-- grants it functions in the graveyard too, and Alice's speed is asked about a
-- card that is nowhere near the battlefield.
--
-- Both directions are proved, because only the pair discriminates: at speed 4 the
-- ability is offered from the graveyard, at speed 3 it is not. A test that only
-- ever asked the first would pass for an engine that ignored the condition
-- entirely.
maxSpeedZoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
maxSpeedZoneSpec s registry = Spec.describe s "MaxSpeedZone" $ do
  Spec.it s "CR 702.178b at max speed the Surveyor's ability is offered from the graveyard" $ do
    surveyor <- S.printingOf s registry "Loxodon Surveyor"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, gs) = surveyorBoard surveyor swamp piker 4
    Spec.assertEqWith s "the card really is in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice gs) [gyId]
    Spec.assertEqWith s "and it offers one ability from there" (length (Activatable.abilitiesFor gyId gs)) 1
    Spec.assertEqWith s "CR 702.178b and the card HAS the ability there (the projection's own roster)" (length (Projection.abilitiesOf gyId gs)) 1
    Spec.assertBool s (any (isActivateOf gyId) (Action.legalActions S.alice gs)) "and the activation is a legal action"
  -- The same gate above 4, which is the second half of parun's ruling on CR
  -- 702.179e (Parun.Engine.Speed.maxSpeed carries it): a player at 5 has max
  -- speed, so a "Max speed --" card written as AtLeast 4 still functions. The
  -- Surveyor's own gate, not the Raceway's, since each card carries its own copy
  -- of CR 702.178a's comparison and only its own case can catch it drifting back
  -- to Exactly. atSpeed rather than an effect, this group's posture throughout;
  -- speed/cr-702-178a-past-4-the-raceway-keeps-its-max-speed-ability.json
  -- drives the same ruling through Gomif, Fast Racer.
  Spec.it s "CR 702.178a past max speed the Surveyor's ability is still offered" $ do
    surveyor <- S.printingOf s registry "Loxodon Surveyor"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, gs) = surveyorBoard surveyor swamp piker 5
    Spec.assertBool s (any (isActivateOf gyId) (Action.legalActions S.alice gs)) "the activation is a legal action at speed 5"
    Spec.assertEqWith s "and one ability is offered from the graveyard" (length (Activatable.abilitiesFor gyId gs)) 1
  -- The other direction, and the whole reason the gate is re-asked outside the
  -- battlefield: one less speed and the same graveyard card offers nothing. The
  -- board is identical in every other respect -- same three Swamps, same library,
  -- same priority -- so the speed is the only thing that can account for it.
  Spec.it s "CR 702.178a below max speed the same graveyard card offers nothing" $ do
    surveyor <- S.printingOf s registry "Loxodon Surveyor"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, gs) = surveyorBoard surveyor swamp piker 3
    Spec.assertEqWith s "the card is still in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice gs) [gyId]
    Spec.assertEqWith s "but it offers no ability" (Activatable.abilitiesFor gyId gs) []
    Spec.assertEqWith s "nor has one there" (Projection.abilitiesOf gyId gs) []
    Spec.assertBool s (not (any (isActivateOf gyId) (Action.legalActions S.alice gs))) "and no activation is offered"
  -- CR 113.6m's "functions ONLY in that zone", from the other side: the very same
  -- printing on the BATTLEFIELD at max speed offers nothing. The projection still
  -- hands the ability out -- CR 702.178a's condition is true, and the card's
  -- ruling has the max speed ability function in the graveyard "too"
  -- (AbilityZone.grantFunctionsIn) -- and the activation is withheld all the
  -- same, because a cost that exiles this card from the graveyard cannot be paid
  -- by a card that is not in one.
  --
  -- The falsifier for a test that passed because the ability worked on the
  -- battlefield anyway.
  Spec.it s "CR 113.6m the same card on the battlefield offers the ability to nobody" $ do
    surveyor <- S.printingOf s registry "Loxodon Surveyor"
    swamp <- S.printingOf s registry "Swamp"
    let (bfId, board) = S.addPermanent surveyor S.alice (S.landsInPlay swamp 3)
        gs =
          atSpeed
            4
            S.alice
            ( board
                { GameState.turn =
                    (GameState.turn board)
                      { TurnRegisters.priority = Just S.alice
                      }
                }
            )
    Spec.assertEqWith s "the projection does hand it out" (length (Projection.abilitiesOf bfId gs)) 1
    Spec.assertBool s (not (any (isActivateOf bfId) (Action.legalActions S.alice gs))) "but no activation is offered"

maxSpeedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
maxSpeedSpec s registry = Spec.describe s "MaxSpeed" $ do
  -- CR 702.178a: "as long as your speed is 4, this object has '[Ability]'". At
  -- speed 1 the Raceway has ONE activated ability, its printed "{T}: Add {C}".
  Spec.it s "CR 702.178a below max speed the Raceway has only its unconditional mana ability" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, board) = S.addPermanent raceway S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.settleSba board
    Spec.assertEqWith s "speed 1" (speedOf S.alice gs) (Just (Just 1))
    Spec.assertEqWith s "one activated ability" (length (Projection.abilitiesOf racewayId gs)) 1
    Spec.assertEqWith s "and tapping it makes one mana" (pooledBy biggestYield racewayId gs) 1
  -- CR 702.179f: a player who has NO speed at all reads as speed 0 for anything
  -- that asks. Settling is deliberately skipped, which is the only way to see a
  -- controller of a Raceway with no speed.
  --
  -- TWO assertions, because the gate alone cannot say this. An unanswered
  -- quantity and a 0 both fail "exactly 4" -- Condition.holds collapses the
  -- undeterminable case to False -- so the ability count below rules out a
  -- stand-in of 4 and nothing finer. Speed.speedOf is the reading CR 702.179f
  -- actually fixes, and Just 0 against the field's Nothing is what tells the two
  -- apart.
  Spec.it s "CR 702.179f a controller with no speed reads as 0, not as unanswered" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, gs) = S.addPermanent raceway S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "the field holds no speed at all (CR 702.179b)" (speedOf S.alice gs) (Just Nothing)
    Spec.assertEqWith s "but every reader sees 0 (CR 702.179f)" (Speed.speedOf S.alice gs) (Just 0)
    Spec.assertEqWith s "so the max speed ability is absent" (length (Projection.abilitiesOf racewayId gs)) 1
  -- CR 702.178a's other side, and the case the whole gate exists for: at speed 4
  -- the object HAS the granted ability, and tapping it makes two mana.
  Spec.it s "CR 702.178a at max speed the Raceway gains its second mana ability" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, board) = S.addPermanent raceway S.alice (Setup.emptyGame S.bothPlayers)
        gs = atSpeed 4 S.alice (S.settleSba board)
    Spec.assertEqWith s "two activated abilities" (length (Projection.abilitiesOf racewayId gs)) 2
    Spec.assertEqWith s "and the granted one makes two mana" (pooledBy biggestYield racewayId gs) 2
  -- Burnout Bashtronaut's "Max speed — This creature has double strike": a
  -- keyword the max speed ability grants its own object, asked at 4 and at 3.
  Spec.it s "CR 702.178a at max speed Burnout Bashtronaut has double strike, one short it does not" $ do
    bashtronaut <- S.printingOf s registry "Burnout Bashtronaut"
    let (botId, board) = S.addPermanent bashtronaut S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.settleSba board
    Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike botId (atSpeed 4 S.alice settled)) "double strike at speed 4"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.DoubleStrike botId (atSpeed 3 S.alice settled))) "no double strike at speed 3"
  -- The layer system is asked BEFORE CR 702.178a's gate, so a Raceway whose rules
  -- text CR 305.7 stripped has no ability left to grant however fast its
  -- controller is. The falsifier for a gate that added the ability rather than
  -- letting one through.
  Spec.it s "CR 305.7 the strip beats the grant even at max speed" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (racewayId, board) = S.addPermanent raceway S.alice (Setup.emptyGame S.bothPlayers)
        (_, moonlit) = S.addPermanent bloodMoon S.bob board
        gs = atSpeed 4 S.alice moonlit
    Spec.assertEqWith s "no activated abilities at all" (Projection.abilitiesOf racewayId gs) []

increaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
increaseSpec s registry = Spec.describe s "Increase" $ do
  -- CR 603.2: the ability triggers as the loss happens, when Bob is still an
  -- opponent in the game; CR 704.5a's departure before the CR 117.5 gather does
  -- not take it back. Three seats, so the game goes on without him.
  Spec.it s "CR 603.2 an opponent the loss knocked out still raises your speed" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, withRaceway) = S.addPermanent raceway S.alice (S.landsFor swamp S.alice 2 S.threePlayerGame)
        stock pid g = foldr (\_ g2 -> snd (S.addLibraryCard piker pid g2)) g [1 .. (4 :: Int)]
        (gs0, spellId) = S.handOne signInBlood (stock S.carol (stock S.bob (stock S.alice withRaceway)))
        gs = atSpeed 1 S.alice gs0 {GameState.players = Map.adjust (\p -> p {Player.life = 2}) S.bob (GameState.players gs0)}
        after = castResolveSettle atBob S.alice spellId gs
    Spec.assertBool s (S.bob `notElem` Game.stillPlaying after) "bob left the game"
    Spec.assertEqWith s "and the game goes on" (GameState.result after) Nothing
    Spec.assertEqWith s "alice's speed rose from 1 to 2" (speedOf S.alice after) (Just (Just 2))
  -- The same rider on an ability GAINED after the turn's first matching event.
  -- Bob loses life to a Lightning Bolt while alice has no speed, so CR 702.179d's
  -- ability does not exist yet; she then plays Muraganda Raceway, CR 702.179a
  -- starts her speed at 1, and a second Bolt is the ability's first triggering
  -- though not the turn's first opponent losing life: the rider is per ability
  -- (Cursed Wombat's ruling of 2024-06-07).
  Spec.it s "CR 702.179d an ability gained after the turn's first life loss still triggers once" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (racewayId, withRaceway) = S.addHandCard raceway S.alice (S.landsInPlay mountain 2)
        (firstBolt, withFirst) = S.addHandCard lightningBolt S.alice withRaceway
        (gs0, secondBolt) = S.handOne lightningBolt withFirst
        early = castResolveSettle atBob S.alice firstBolt gs0
        engines = S.runPure S.identityAnswer early (Cast.playLand Settle.none False S.alice racewayId Nothing >> Engine.settleForPriority)
        late = castResolveSettle atBob S.alice secondBolt engines
    Spec.assertEqWith s "the second Bolt raised alice's speed from 1 to 2" (speedOf S.alice late) (Just (Just 2))
    Spec.assertEqWith s "bob lost six life over the two Bolts" (S.lifeOf S.bob late) (fmap (subtract 6) (S.lifeOf S.bob gs0))
    Spec.assertEqWith s "setup: alice had no speed when bob first lost life" (speedOf S.alice early) (Just Nothing)
    Spec.assertEqWith s "setup: the Raceway started her speed at 1" (speedOf S.alice engines) (Just (Just 1))
  -- CR 603.2c's "one or more opponents lose life", through the shared matcher
  -- (Event.Trigger.inherentTriggers): two losses in ONE group -- one combat
  -- damage step -- are one trigger event, and a later group in the same scan is
  -- another. The rider then spends one of the two, so speed rises once. The
  -- gather is read directly because the rider masks the batching at gameplay
  -- level.
  Spec.it s "CR 603.2c two losses in one group are one trigger event, a later group another, and the rider keeps one" $ do
    let logged =
          S.withGroupedEvents
            [ [GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 2 True), GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 1 True)],
              [GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 3 True)]
            ]
            (atSpeed 1 S.alice (Setup.emptyGame S.bothPlayers))
        after = resolveTrigger S.identityAnswer logged
    Spec.assertEqWith s "alice is the active player" (TurnRegisters.activePlayer (GameState.turn logged)) S.alice
    Spec.assertEqWith s "CR 702.179d alice's speed rose once" (speedOf S.alice after) (Just (Just 2))
    Spec.assertEqWith s "one sourceless trigger was spent" (inherentTriggersSpent after) [S.alice]
    Spec.assertEqWith s "the gather fired once per group, not once per loss" (fmap PendingTrigger.controller (Trigger.inherentTriggers (fmap (\(pid, ability) -> (TriggerSource.Sourceless, pid, ability)) (Speed.abilities logged)) (Event.unscannedGrouped logged) logged)) [S.alice, S.alice]

-- CR 702.179 in the one direction rule 702.179 never legislates: a REDUCTION.
-- CR 702.179c covers being "instructed to increase", CR 702.179b makes speed
-- something a player has only once a rule or effect sets it, and nothing in the
-- rule bounds a speed from below -- so both the reduction and its floor are the
-- CARD's, and the card is Spikeshell Harrier ({4}{U} Artifact Creature -- Robot
-- Turtle 4/4, Aetherdrift, checked against Scryfall): "When this creature enters,
-- return target creature or Vehicle an opponent controls to its owner's hand. If
-- that opponent's speed is greater than each other player's speed, reduce that
-- opponent's speed by 1. This effect can't reduce their speed below 1."
--
-- THREE SEATS throughout, and not for the usual reason. "Each OTHER player" is
-- read against the BOUNCED PERMANENT'S CONTROLLER rather than against the
-- Harrier's, so two seats would collapse "the other players" onto the one player
-- who cast it and could not tell the printed comparison from "greater than
-- yours". Every board below therefore gives all three seats DISTINCT speeds where
-- it can, with a seat above Bob and a seat below him across the group, and one
-- board ties two seats at the top -- which is the only way to tell CR's "greater
-- than" from "at least".
--
-- The speeds are written with `atSpeed` rather than climbed through rule 702.179d,
-- which increaseSpec above already proves: one increase per player per turn could
-- not reach three different seats at three different speeds inside one turn.
--
-- NOT PROVEN HERE, and stated because it cannot be: the opcode leaves a player
-- with NO speed (CR 702.179b) with none rather than giving them one. This card
-- cannot reach that branch -- CR 702.179f reads such a player at 0, and 0 is
-- never strictly greater than every other player's reading, so the gate is always
-- false for them. That arm is a regression fence rather than a proved behaviour.
cardDecreaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cardDecreaseSpec s registry = Spec.describe s "CardDecrease" $ do
  -- The whole card, on a board where the gate holds. Bob is strictly the fastest,
  -- so his speed drops by exactly 1 -- and 4 -> 3 with Alice at 2 and Carol at 1
  -- leaves every seat distinct both before and after, so no other reading of the
  -- sentence produces this board.
  Spec.it s "CR 702.179 the fastest player's speed drops by one" $ do
    harrier <- S.printingOf s registry "Spikeshell Harrier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, board) = harrierBoard harrier piker
        gs = atSpeed 4 S.bob (atSpeed 2 S.alice (atSpeed 1 S.carol board))
        after = resolveTrigger (bounceAt victimId) gs
    -- CR 608.2h is what the second sentence rests on: the first has already moved
    -- the permanent to a hidden zone, so "that opponent" is answered from its last
    -- known information rather than from a controller it no longer has (CR 108.4).
    Spec.assertEqWith s "the permanent was returned to bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertBool s (not (Set.member victimId (GameState.battlefield after))) "and it is off the battlefield"
    Spec.assertEqWith s "bob's 4 became 3" (speedOf S.bob after) (Just (Just 3))
    Spec.assertEqWith s "alice's speed did not move" (speedOf S.alice after) (Just (Just 2))
    Spec.assertEqWith s "nor carol's" (speedOf S.carol after) (Just (Just 1))
  -- "GREATER THAN each other player's speed", strictly. Bob and Carol tie at the
  -- top, so Bob is not greater than each other player and nothing happens. The one
  -- board that separates the printed comparison from "at least as fast as".
  Spec.it s "CR 702.179 a tie at the top is not greater than each other player" $ do
    harrier <- S.printingOf s registry "Spikeshell Harrier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, board) = harrierBoard harrier piker
        gs = atSpeed 3 S.bob (atSpeed 1 S.alice (atSpeed 3 S.carol board))
        after = resolveTrigger (bounceAt victimId) gs
    Spec.assertEqWith s "the permanent was still returned" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertEqWith s "but bob's speed stayed at 3" (speedOf S.bob after) (Just (Just 3))
    Spec.assertEqWith s "and carol's at 3" (speedOf S.carol after) (Just (Just 3))
    Spec.assertEqWith s "and alice's at 1" (speedOf S.alice after) (Just (Just 1))
  -- The comparison is against EVERY other player, not against the Harrier's
  -- controller. Bob is faster than Alice, who cast it, and slower than Carol, who
  -- did nothing -- so a reading that only looked at "you" would lower his speed
  -- here and the printed one does not.
  Spec.it s "CR 702.179 a third seat above the targeted player stops the reduction" $ do
    harrier <- S.printingOf s registry "Spikeshell Harrier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, board) = harrierBoard harrier piker
        gs = atSpeed 3 S.bob (atSpeed 1 S.alice (atSpeed 4 S.carol board))
        after = resolveTrigger (bounceAt victimId) gs
    Spec.assertEqWith s "the permanent was still returned" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertEqWith s "bob's speed stayed at 3" (speedOf S.bob after) (Just (Just 3))
    Spec.assertEqWith s "carol's at 4" (speedOf S.carol after) (Just (Just 4))
    Spec.assertEqWith s "and alice's at 1" (speedOf S.alice after) (Just (Just 1))
  -- "THIS EFFECT CAN'T REDUCE THEIR SPEED BELOW 1", the card's own sentence and no
  -- rule of rule 702.179's. Bob at 1 against two seats with no speed at all passes
  -- the gate -- CR 702.179f reads them at 0 -- so the reduction happens and the
  -- floor is the only thing that stops it landing on 0.
  Spec.it s "the printed floor stops the reduction at 1" $ do
    harrier <- S.printingOf s registry "Spikeshell Harrier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, board) = harrierBoard harrier piker
        gs = atSpeed 1 S.bob board
        after = resolveTrigger (bounceAt victimId) gs
    Spec.assertEqWith s "alice has no speed at all (CR 702.179b)" (speedOf S.alice gs) (Just Nothing)
    Spec.assertEqWith s "nor carol" (speedOf S.carol gs) (Just Nothing)
    Spec.assertEqWith s "bob is left at the floor rather than dropped to 0" (speedOf S.bob after) (Just (Just 1))
    Spec.assertEqWith s "and the two speedless seats are still speedless" (speedOf S.carol after) (Just Nothing)
  -- The floor's PAIR, differing from the case above in exactly one thing: Bob's
  -- starting speed. At 2 the same gate holds against the same two speedless seats
  -- and the reduction lands, which is what makes the case above a clamp rather
  -- than a gate that never fired.
  Spec.it s "one above the floor, the same board reduces" $ do
    harrier <- S.printingOf s registry "Spikeshell Harrier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, board) = harrierBoard harrier piker
        gs = atSpeed 2 S.bob board
        after = resolveTrigger (bounceAt victimId) gs
    Spec.assertEqWith s "bob's 2 became 1" (speedOf S.bob after) (Just (Just 1))
    Spec.assertEqWith s "and alice still has no speed -- a decrease creates none" (speedOf S.alice after) (Just Nothing)

-- Alice's Spikeshell Harrier entering with its CR 603.6a trigger pending, over a
-- three-seat board whose only other permanent is Bob's Goblin Piker -- the one
-- legal target, so the answerer below cannot pick a different one and rescue an
-- assertion. Carol controls nothing: she is here to be a third SPEED, which is
-- all the printed comparison asks of her.
harrierBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
harrierBoard harrier piker =
  let (victimId, board) = S.addPermanent piker S.bob S.threePlayerGame
      (_, gs) = S.entersWithTrigger harrier S.alice board
   in (victimId, gs)

-- Aim the trigger's one target slot at this permanent, PINNED rather than
-- searched: an answerer that picked whatever was legal would find the same
-- permanent again after a mutation and keep the case green.
bounceAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bounceAt victimId p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject victimId))) sets
  _ -> S.identityAnswer p

-- Place the pending enters trigger (CR 603.3b) and resolve it. No settle
-- afterwards: nothing on these boards has a state-based action or a further
-- trigger owed, and the assertions are about the resolution itself.
resolveTrigger :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveTrigger answer gs =
  let placed = S.runPure answer gs (Engine.placePendingTriggers [])
   in S.runPure answer placed Stack.resolveTop

-- CR 702.179d's "only once each turn" allowance, read the way
-- Event.withinTriggerLimit spends it: the players whose SOURCELESS ability is in
-- this turn's CR 603.3b log.
inherentTriggersSpent :: GameState.GameState -> [PlayerId.PlayerId]
inherentTriggersSpent gs =
  Maybe.mapMaybe
    ( \event -> case event of
        GameEvent.AbilityTriggered record
          | AbilityTriggered.source record == TriggerSource.Sourceless ->
              Just (AbilityTriggered.controller record)
        _ -> Nothing
    )
    (S.eventsOf gs)

-- Put this player at exactly this speed, bypassing CR 702.179d's climb. Every
-- case that uses it is about what READS speed (CR 702.178a's gate, CR 702.179e's
-- max speed), not about how speed got there -- which increaseSpec proves through
-- the rules.
atSpeed :: Natural.Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
atSpeed n pid gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.speed = Just n}) pid (GameState.players gs)}
