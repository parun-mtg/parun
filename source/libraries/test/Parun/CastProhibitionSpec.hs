{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Parun.Engine.PlayerEffect over effects that forbid casting (CR 601.3): Silence
-- and its conditional forms, Liliana, Null Chamber, Runed Halo, The Stasis
-- Coffin, Conjurer's Ban.
-- Split out of Parun.PlayerEffectSpec, which keeps the machinery.
module Parun.CastProhibitionSpec where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Interpreter as Interpreter
import Parun.PlayerEffectSpec (isCast, silenceAfter)
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Action as Action.Type
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.VariableChoice as VariableChoice
import qualified Parun.Types.Zone as Zone

-- Silence {W} Instant: "Your opponents can't cast spells this turn."
silenceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
silenceSpec s registry =
  Spec.describe s "Silence" $ do
    Spec.it s "before Silence resolves, bob may cast his creature" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, pikerId, _, before, _) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.bob before)) "offered"

-- CR 601.2c: the three-seat Cease-Fire board. alice has three Plains and the
-- Cease-Fire; all three seats have two Mountains, a Goblin Piker in hand and two
-- Plains in their library, and carol also holds a Lightning Bolt. The seats are
-- stocked IDENTICALLY on the creature axis on purpose -- the only thing that
-- differs between bob and carol afterwards is which of them the spell targeted --
-- and carol's Bolt is the second axis: same seat, same mana, a noncreature card.
--
-- THREE seats because two collapse "target player" onto "the opponent", which is
-- exactly the reading under test. carol is the far seat.
--
-- The libraries are stocked because the card draws: an empty one would lose alice
-- the game to CR 104.3c before the assertions ran, and the equal stock is what
-- makes the draw's own control (bob's library) honest.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
ceaseFireBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ceaseFireBoard plains ceaseFire mountain piker lightningBolt =
  let gs0 = Setup.emptyGame S.threePlayers
      repeatedly f n gs = List.foldl' (\g _ -> f g) gs [1 .. n :: Int]
      addLands printing pid = repeatedly (snd . S.addPermanent printing pid)
      stockLibrary pid = repeatedly (snd . S.addLibraryCard plains pid) (2 :: Int)
      gs1 = addLands plains S.alice (3 :: Int) gs0
      gs2 = addLands mountain S.alice (2 :: Int) gs1
      (ceaseFireId, gs3) = S.addHandCard ceaseFire S.alice gs2
      (alicesPiker, gs4) = S.addHandCard piker S.alice gs3
      gs5 = addLands mountain S.bob (2 :: Int) gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      gs7 = addLands mountain S.carol (2 :: Int) gs6
      (carolsPiker, gs8) = S.addHandCard piker S.carol gs7
      (carolsBolt, gs9) = S.addHandCard lightningBolt S.carol gs8
      stocked = stockLibrary S.carol (stockLibrary S.bob (stockLibrary S.alice gs9))
   in ( ceaseFireId,
        alicesPiker,
        bobsPiker,
        carolsPiker,
        carolsBolt,
        -- carol's own main phase, which is when the card is really cast: its
        -- effect lasts "this turn", so aiming it at the player whose turn it is
        -- is what makes it bite at all.
        stocked
          { GameState.turn =
              (GameState.turn stocked)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.carol,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- Every target prompt answers with CAROL -- pinned rather than searched, so a
-- broken bake cannot be repaired by an answerer that hunts for a legal option.
ceaseFireAtCarol :: Prompt.Prompt r -> r
ceaseFireAtCarol p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.carol))) sets
  _ -> S.identityAnswer p

-- alice casts Cease-Fire at carol and it resolves.
ceaseFireAfter :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
ceaseFireAfter plains ceaseFire mountain piker lightningBolt =
  let (ceaseFireId, alicesPiker, bobsPiker, carolsPiker, carolsBolt, before) = ceaseFireBoard plains ceaseFire mountain piker lightningBolt
      onStack = S.runPure ceaseFireAtCarol before (S.cast S.alice ceaseFireId)
      after = S.runPure ceaseFireAtCarol onStack Engine.priorityLoop
   in (alicesPiker, bobsPiker, carolsPiker, carolsBolt, before, after)

-- Is a cast of this card offered to this seat, on a board staged as that seat's
-- own main phase? CR 302.1 offers a creature spell only to the active player, so
-- the flip is what keeps the three seats comparable -- the posture the three-seat
-- Silence case above already takes, and nothing else about the board changes.
offersCast :: PlayerId.PlayerId -> ObjectId.ObjectId -> Printing.Printing -> GameState.GameState -> Bool
offersCast pid oid printing gs =
  elem
    (Action.Type.Cast oid (S.printingName printing) Facing.FaceUp)
    ( Action.legalActions
        pid
        ( gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.activePlayer = pid
                  }
            }
        )
    )

-- Cease-Fire {2}{W} Instant: "Target player can't cast creature spells this
-- turn. Draw a card." The first card in the pool to store a player effect on a
-- TARGETED seat.
ceaseFireSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ceaseFireSpec s registry =
  Spec.describe s "CeaseFire" $ do
    -- The positive control the scenarios' negatives are read against
    -- (data/scenarios/cast-prohibition, "CR 601.2c" and "CR 514.2"): before the
    -- spell resolves, every seat may cast its Goblin Piker.
    Spec.it s "before Cease-Fire resolves, all three seats may cast their creature" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (alicesPiker, bobsPiker, carolsPiker, _, before, _) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
      Spec.assertEqWith s "three seats" (length (GameState.seating before)) 3
      Spec.assertBool s (offersCast S.alice alicesPiker piker before) "alice could cast"
      Spec.assertBool s (offersCast S.bob bobsPiker piker before) "bob could cast"
      Spec.assertBool s (offersCast S.carol carolsPiker piker before) "carol could cast"

-- CR 611.2b's board, and the SWAMP is the only thing about it that varies. alice
-- has an Island (which pays for the spell) and, on the holding board, a Swamp
-- (which the condition counts); bob and carol each have two Mountains and a
-- Goblin Piker, so both opponents can genuinely cast before the spell resolves.
--
-- Paying and gating are deliberately split across two lands: with one land doing
-- both, "the condition holds" and "she had mana" would be the same fact, and the
-- never-starts case below could not hold mana equal while removing the Swamp.
--
-- The Swamp's id is S.noSource on the board that has no Swamp: the one case built
-- that way never names it, and there is nothing on the battlefield for it to
-- collide with.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
conditionalSilenceBoard :: Bool -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
conditionalSilenceBoard withSwamp island swamp hush mountain piker =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addPermanent island S.alice gs0
      (swampId, gs2) =
        if withSwamp
          then S.addPermanent swamp S.alice gs1
          else (S.noSource, gs1)
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (_, gs4) = S.addPermanent mountain S.bob gs3
      (_, gs5) = S.addPermanent mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addPermanent mountain S.carol gs6
      (_, gs8) = S.addPermanent mountain S.carol gs7
      (carolsPiker, gs9) = S.addHandCard piker S.carol gs8
   in ( hushId,
        swampId,
        bobsPiker,
        carolsPiker,
        gs9
          { GameState.turn =
              (GameState.turn gs9)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- Goblin Piker is a creature, so CR 302.1 offers it only to the ACTIVE player.
-- The board is alice's own main phase, so each opponent's cast is read off a copy
-- with activePlayer flipped to them and nothing else changed.
conditionalSilenceCasts :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
conditionalSilenceCasts who gs =
  filter
    isCast
    ( Action.legalActions
        who
        ( gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.activePlayer = who
                  }
            }
        )
    )

-- SYNTHETIC. "Synthetic Conditional Silence" {U} Instant: "For as long as you
-- control a Swamp, your opponents can't cast spells." CR 611.2b's duration on the
-- stored player-effect carrier (Parun.Types.ActivePlayerEffect), which no printed
-- card reaches: a "for as long as" effect that changes what a PLAYER may do is
-- printed as a static ability on a permanent, and that rides the other carrier
-- (Parun.Types.PlayerStaticAbility) -- Grand Abolisher, Rule of Law and Damping
-- Engine are all statics. Every printed spell or ability that stores a
-- player-axis effect states a TURN-relative duration instead (Silence, Blossoming
-- Calm, Hope of Ghirapur, Academic Probation). Nothing in CR 611.2b confines the
-- duration to one carrier, so the card is legitimate and only unprinted.
conditionalSilenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalSilenceSpec s registry =
  Spec.describe s "Synthetic Conditional Silence" $ do
    -- THE CONTROL TWIN: both opponents really could cast, so the refusals in
    -- data/scenarios/cast-prohibition's "CR 611.2b it is stored while the
    -- condition holds" are the prohibition and not an unaffordable Piker.
    Spec.it s "before it resolves, both opponents may cast" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, bobsPiker, carolsPiker, before) = conditionalSilenceBoard True island swamp hush mountain piker
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob before)) "bob is offered his Piker"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.carol before)) "and carol hers"

    -- CR 611.2b's first sentence: a duration that never STARTS means the effect
    -- does nothing at all. The board differs from the holding one by the Swamp
    -- alone -- the Island that pays for the spell is on both -- so this is the
    -- Nothing arm of Expiry.arm and not an unaffordable cast.
    Spec.it s "CR 611.2b with no Swamp the duration never starts and nothing is stored" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, bobsPiker, _, before) = conditionalSilenceBoard False island swamp hush mountain piker
          -- Stack.resolveTop and NOT the priority loop the other cases use. A
          -- settle runs Expiry.sweepConditional, which deletes an effect whose
          -- condition is already false -- so a loop cannot tell "the duration
          -- never started" from "it started and was swept an instant later", and
          -- an arm that stored the effect unconditionally would leave this case
          -- green. The bare resolution can tell them apart.
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice hushId)) Stack.resolveTop
          settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
      Spec.assertBool s (notElem hushId (TurnRegisters.stack (GameState.turn resolved))) "the spell really did resolve"
      Spec.assertEqWith s "nothing stored" (StoredEffects.playerEffects (GameState.stored resolved)) []
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "so bob may cast"

-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
matchesObjectBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
matchesObjectBoard lightningBolt piker =
  let base = Setup.emptyGame S.bothPlayers
      (bolt, withBolt) = S.spellOnStack lightningBolt S.alice base
      (pikerId, gs) = S.spellOnStack piker S.alice withBolt
   in (bolt, pikerId, gs)

-- The spell-match half of the cost-adjustment axis, now expressed as a Filter
-- over the PROJECTED view (CR 613.1d layer 4 for a card type, CR 613.1e layer 5
-- for a colour) rather than the retired SpellCriterion. A noncreature spell is
-- Filter.Not (Filter.HasCardType Creature); a coloured spell is Filter.HasColor.
matchesObjectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matchesObjectSpec s registry =
  Spec.describe s "matchesObject" $ do
    let noncreature = Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)

    Spec.it s "CR 613.1d Thalia's noncreature criterion admits an instant" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) noncreature bolt gs) "Lightning Bolt is a noncreature spell"

    Spec.it s "CR 613.1d a creature spell fails the noncreature criterion" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) noncreature pikerId gs)) "Goblin Piker is a creature spell"

    Spec.it s "CR 613.1e a colour criterion admits a matching-colour spell" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) (Filter.Type.HasColor Color.Red) bolt gs) "Lightning Bolt is red"

    Spec.it s "CR 613.1e a colour criterion rejects a non-matching colour" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) (Filter.Type.HasColor Color.Blue) bolt gs)) "Lightning Bolt is not blue"

-- Null Chamber {3}{W} World Enchantment: "As this enchantment enters, you and an
-- opponent each choose a card name other than a basic land card name. Spells
-- with the chosen names can't be cast and lands with the chosen names can't be
-- played."
--
-- alice has eight untapped Plains and four Mountains (mana is never the reason a
-- cast is unavailable, before or after the Chamber's own {3}{W} is paid) and the
-- Chamber in hand, in her own precombat main phase with an empty stack.
nullChamberBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
nullChamberBoard plains mountain nullChamber =
  let addMountain board _ = snd (S.addPermanent mountain S.alice board)
      lands = List.foldl' addMountain (S.landsInPlay plains 8) [1 .. 4 :: Int]
      (gs, oid) = S.handOne nullChamber lands
   in (oid, gs)

-- CR 201.4 answered PER CHOOSER, which is the whole of what makes Null Chamber
-- worth testing: `pick` is asked WHO is choosing, so a case can put the
-- controller's name and the opponent's on different cards. `opponent` settles
-- the card's other open choice -- which opponent is asked at all -- and is never
-- reached at two seats. Everything else is the shared interpreter.
chamberAnswer :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> Prompt.Prompt r -> r
chamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ _ _ -> pick chooser
  Prompt.ChooseOpponent {} -> opponent
  _ -> S.identityAnswer p

-- chamberAnswer, taking each name off a QUEUE rather than off a function of the
-- chooser. CR 201.4 refuses an illegal name and the same chooser is asked again,
-- so the case below needs an answerer whose second answer differs from its
-- first, which a pure Prompt r -> r cannot be -- State-threaded, as
-- Parun.CopySpec's countingAnswer is.
--
-- Over Asked, because Parun.Interpreter.policingCardNames wraps the primitive
-- seam (Parun.Engine.Engine.runGameAsked); nothing here reads the tag.
--
-- An exhausted queue answers `fallback` rather than looping or failing: a legal
-- name neither case expects, so drawing past the end reddens the chosenNames
-- assertion instead of hanging the re-ask.
queuedChamberAnswer ::
  (Monad m) =>
  PlayerId.PlayerId ->
  CardName.CardName ->
  Asked.Asked GameState.GameState r ->
  State.StateT [CardName.CardName] m r
queuedChamberAnswer opponent fallback asked = case Asked.prompt asked of
  Prompt.ChooseCardName {} -> do
    queue <- State.get
    case queue of
      [] -> pure fallback
      name : rest -> do
        State.put rest
        pure name
  p -> pure (chamberAnswer opponent (const fallback) p)

-- castChamber, with the name answers coming off `queue` through
-- Parun.Interpreter.policingCardNames -- the wrapper an interpreter installs over
-- its own answerer. Hands back the finished board and what the queue has LEFT,
-- which is how the case below counts the asks.
--
-- The registry is lifted into the answerer's own monad: Registry.Registry is
-- parameterized over the monad a lookup works in exactly so that a caller can.
policedChamber ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  CardName.CardName ->
  [CardName.CardName] ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  m (GameState.GameState, [CardName.CardName])
policedChamber s registry opponent fallback queue gs oid = do
  let lifted = Registry.hoist Trans.lift registry
      play = Engine.runGameAsked (Interpreter.policingCardNames lifted (Trans.lift . S.brokenCorpus s) (queuedChamberAnswer opponent fallback)) gs (S.cast S.alice oid >> Stack.resolveTop)
  ((_, after), left) <- State.runStateT play queue
  pure (after, left)

-- CR 201.4a's restriction as Null Chamber prints it: "other than a basic land
-- card name", which is a supertype and a card type together (CR 205.4a: a basic
-- land card is the one carrying both).
nonBasicLandName :: Filter.Type.Filter Keyword.Keyword
nonBasicLandName = Filter.Type.Not (Filter.Type.And [Filter.Type.HasSupertype Supertype.Basic, Filter.Type.HasCardType CardType.Land])

-- Goblin Piker's SLUG -- the second spelling Parun.Registry.named's haddock says
-- fetches the same card, Parun.Registry.slugFor mapping the card's own name onto
-- it. A slug is not what CR 201.4's reference holds, and the gap between the two
-- lookups is what Parun.Interpreter.legalCardName's exact face-name comparison
-- closes.
pikerSlug :: CardName.CardName
pikerSlug = CardName.MkCardName (Text.pack "goblin-piker")

-- Cast the Chamber and let it resolve, answering both name choices.
--
-- CAST rather than S.addPermanent, because the choice happens only on the entry
-- path (Event.runEntry): a Chamber placed straight onto the battlefield
-- has an empty chosenNames and prohibits nothing.
castChamber :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castChamber opponent pick gs oid =
  let answer :: Prompt.Prompt r -> r
      answer = chamberAnswer opponent pick
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- The one object that reached the battlefield between two states -- the Chamber
-- itself, whose id the cast never handed back (CR 400.7 mints a new one).
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
  [oid] -> Just oid
  _ -> Nothing

nullChamberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nullChamberSpec s registry =
  Spec.describe s "NullChamber" $ do
    -- CR 201.4's OTHER refusal, which data/scenarios/cast-prohibition's "CR
    -- 201.4 a name no card has" cannot reach: a string the registry answers
    -- to that is still not a card's name. The file registry
    -- looks up a slug, so "goblin-piker" fetches the Goblin Piker; rule 201.4
    -- asks for the name of a card, and Parun.Engine.Filter's HasChosenName
    -- compares names exactly, so admitting the slug would write a name into
    -- Object.chosenNames that prohibits nothing.
    --
    -- THE DISCRIMINATOR is the last assertion: without it this case passes for
    -- that scenario's reason -- a name the registry cannot resolve at all.
    Spec.it s "CR 201.4 a slug the registry answers to is not a card's name" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          queue = [pikerSlug, S.printingName piker, S.printingName cancel]
      fetched <- Registry.fetchCard registry pikerSlug
      (after, left) <- policedChamber s registry S.bob (S.printingName lightningBolt) queue board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          Spec.assertEqWith
            s
            "the card's own name, and not the slug that fetches it"
            (Map.findWithDefault Map.empty Acquisition.Own (Object.chosenNames chamber))
            (fmap Set.singleton (Map.fromList [(S.alice, S.printingName piker), (S.bob, S.printingName cancel)]))
          Spec.assertEqWith s "every queued answer was drawn, so alice was asked twice" left []
      Spec.assertBool s (either (const False) Maybe.isJust fetched) "the registry does answer to the slug"

    -- CR 709.3a / 709.3b: only the half being cast is asked about, so naming
    -- "Wax" stops Wax and leaves Wane castable. The prohibition reads the
    -- proposal's view (Filter.HasChosenName), where CR 709.4a's combined view in
    -- the hand would carry both names and stop both halves. A Forest, a Piker for
    -- Wax to target and the Chamber itself for Wane keep either half castable.
    Spec.it s "CR 709.3a naming one half of a split card stops that half and not the other" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      forest <- S.printingOf s registry "Forest"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      waxWane <- S.printingOf s registry "Wax"
      cancel <- S.printingOf s registry "Cancel"
      let wax = CardName.MkCardName (Text.pack "Wax")
          wane = CardName.MkCardName (Text.pack "Wane")
          (oid, board) = nullChamberBoard plains mountain nullChamber
          withSplit aliceName =
            let after = castChamber S.bob (\pid -> if pid == S.alice then aliceName else S.printingName cancel) board oid
                (_, withForest) = S.addPermanent forest S.alice after
                (_, withPiker) = S.addPermanent piker S.alice withForest
             in S.addHandCard waxWane S.alice withPiker
          (splitId, gs) = withSplit wax
          offered = Action.legalActions S.alice gs
          -- The control differs in alice's name alone: the Piker, which she is
          -- not holding.
          (controlId, control) = withSplit (S.printingName piker)
      Spec.assertBool s (notElem (Action.Type.Cast splitId wax Facing.FaceUp) offered) "the named Wax half is not offered"
      Spec.assertBool s (elem (Action.Type.Cast splitId wane Facing.FaceUp) offered) "the Wane half still is"
      Spec.assertBool s (elem (Action.Type.Cast controlId wax Facing.FaceUp) (Action.legalActions S.alice control)) "and Wax is offered when nobody named it"

-- Take every land play the board allows and stop. S.playLandAnswer plays a land
-- whenever one is offered and passes otherwise, so the loop halts exactly when
-- CR 305.2a's comparison refuses -- the whole gate, through the real priority
-- loop, rather than a direct call to Action.legalActions.
playEveryLand :: GameState.GameState -> GameState.GameState
playEveryLand gs = S.runPure S.playLandAnswer gs Engine.priorityLoop

-- CR 601.3b's board, shared by the two groups below it -- Vedalken Orrery's and
-- Sigarda's Aid's -- since what a permission is read off is the caller's `extra`.
--
-- One board, built twice. alice holds `hand` -- a creature card, so CR 302.1 and
-- CR 117.1a's second sentence give it the sorcery-speed window -- and a
-- Mountain, behind nine untapped Mountains so that mana is never the reason a
-- cast is unavailable. It is BOB's precombat main phase and the stack is empty,
-- so alice's own sorcery-speed window is shut. `extra` goes onto the battlefield
-- under alice, and is the only thing a pair of boards here ever differs by.
-- Returns the hand card, the ids of `extra` in the order given, and the board.
flashBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
flashBoard mountain hand extra =
  let (base, oid) = S.pikerInHand mountain hand 9 Phase.PrecombatMain
      withLand = snd (S.addHandCard mountain S.alice base)
      put (ids, g) printing = let (i, g1) = S.addPermanent printing S.alice g in (ids <> [i], g1)
      (extraIds, withExtra) = List.foldl' put ([], withLand) extra
   in ( oid,
        extraIds,
        withExtra
          { GameState.turn =
              (GameState.turn withExtra)
                { TurnRegisters.activePlayer = S.bob,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- The same board back on ALICE's turn, which is the control every refusal below
-- is measured against: it is what says the card in hand is affordable, offered
-- and unblocked by anything the permission under test is not responsible for.
flashOnOwnTurn :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
flashOnOwnTurn mountain hand extra =
  let (oid, _, board) = flashBoard mountain hand extra
   in ( oid,
        board
          { GameState.turn =
              (GameState.turn board)
                { TurnRegisters.activePlayer = S.alice
                }
          }
      )

-- CR 109.5's You scope from the other seat: it is ALICE's turn, BOB holds
-- priority with a Piker of his own, and both players have nine untapped
-- Mountains. `owner` is who controls the Orrery, and is the only thing that
-- varies.
orreryScopeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
orreryScopeBoard mountain piker orrery owner =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.bob g)) base [1 .. 9 :: Int]
      (bobsPiker, withBobsPiker) = S.addHandCard piker S.bob withBobsLands
      withOrrery = snd (S.addPermanent orrery owner withBobsPiker)
   in ( bobsPiker,
        withOrrery
          { GameState.turn =
              (GameState.turn withOrrery)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.bob
                }
          }
      )

-- CR 307.5's window, on the same axis and carried by something else entirely:
-- alice controls a Goblin Piker to equip, a Bonesplitter to equip it with
-- (data/cards/bonesplitter.json declares the equip ability SorcerySpeed) and
-- whatever `extra` names, with nine untapped Mountains for the {1}. `active` is
-- whose turn it is. Returns the Equipment.
equipBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
equipBoard mountain piker bonesplitter extra active =
  let base = S.landsInPlay mountain 9
      withPiker = snd (S.addPermanent piker S.alice base)
      (equipment, withEquipment) = S.addPermanent bonesplitter S.alice withPiker
      withExtra = List.foldl' (\g printing -> snd (S.addPermanent printing S.alice g)) withEquipment extra
   in ( equipment,
        withExtra
          { GameState.turn =
              (GameState.turn withExtra)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = active,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

isActivateOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivateOf oid action = case action of
  Action.Type.Activate o _ -> o == oid
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.TurnConspiracyFaceUp _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect {} -> False
  Action.Type.MakeLaterPayment {} -> False
  Action.Type.PayLifeForMana _ _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

isPlay :: Action.Type.Action -> Bool
isPlay action = case action of
  Action.Type.Play {} -> True
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.TurnConspiracyFaceUp _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect {} -> False
  Action.Type.MakeLaterPayment {} -> False
  Action.Type.PayLifeForMana _ _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

-- Runed Halo {W}{W} Enchantment: "As this enchantment enters, choose a card
-- name. You have protection from the chosen card name." The pool's one card that
-- gives a PLAYER protection from a CHOSEN NAME -- The Stasis Coffin below states
-- its quality on the card instead -- and so the producer of both halves this
-- group proves: CR 702.16b's targeting bar and CR 702.16c's enchanting bar, each
-- read off Parun.Engine.PlayerEffect.protectedFrom.
--
-- Curse of Vitality is the Aura on the other side, and it has to be an
-- enchant-PLAYER one (CR 702.5d): rule 702.16c's player half is the clause under
-- test, and an Aura that enchants a creature never reaches it.
--
-- THREE SEATS, and that is what makes each case discriminating rather than
-- vacuous: alice holds the Halo, bob holds the Curse, and carol holds nothing --
-- so "bob may not enchant alice" is told apart from "bob may not enchant
-- anybody" on one board, and from "the Curse is illegal" on one pass.
--
-- Every case is boards differing in ONE thing -- the chosen NAME, or which seat
-- the case asks about. A board where the Halo names Goblin Piker is the same
-- board in every other respect, which is what keeps the mana, the phase and the
-- Curse's own legality out of the answer.
runedHaloBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
runedHaloBoard plains halo curse =
  let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 2 S.threePlayerGame)
      (haloId, g1) = S.addHandCard halo S.alice lands
      (curseId, g2) = S.addHandCard curse S.bob g1
   in ( haloId,
        curseId,
        g2
          { GameState.turn =
              (GameState.turn g2)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- Cast the Halo and let it resolve, naming `name`.
--
-- CAST rather than S.addPermanent, castChamber's reason: the choice happens only
-- on the entry path (Event.runEntry), so a Halo placed straight onto the
-- battlefield has an empty chosenNames and protects from nothing.
castHalo :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castHalo name gs oid =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCardName {} -> name
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- castHalo through Parun.Interpreter.lookingUpCards over the suite's registry, so
-- Prompt.LookUpCard is answered the way an interpreter holding the reference
-- answers it. castHalo is the same cast with the reference never consulted.
lookedUpHalo :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> m GameState.GameState
lookedUpHalo s registry name gs oid = do
  (_, after) <- Engine.runGameAsked (Interpreter.lookingUpCards registry (S.brokenCorpus s) (namingAnswer name)) gs (S.cast S.alice oid >> Stack.resolveTop)
  pure after

-- Names `name` whenever a card name is asked for, and otherwise S.identityAnswer.
namingAnswer :: (Monad m) => CardName.CardName -> Asked.Asked GameState.GameState r -> m r
namingAnswer name asked = case Asked.prompt asked of
  Prompt.ChooseCardName {} -> pure name
  p -> pure (S.identityAnswer p)

-- A Hill Giant of bob's, carrying `kit` when there is one, attacks alice alone
-- off a board the Halo has already entered; the pair (before, after) comes back
-- for the life it cost. Combat damage and not a burn spell, since CR 702.16b
-- already stops a spell with the chosen name from targeting alice.
spyKitCombat :: Printing.Printing -> Maybe Printing.Printing -> GameState.GameState -> (GameState.GameState, GameState.GameState)
spyKitCombat giant kit haloed =
  let (giantId, withGiant) = S.addPermanent giant S.bob haloed
      equipped = case kit of
        Nothing -> withGiant
        Just k ->
          let (kitId, g) = S.addPermanent k S.bob withGiant
           in S.attachTo kitId (Recipient.ToObject giantId) g
      before =
        equipped
          { GameState.turn =
              (GameState.turn equipped)
                { TurnRegisters.activePlayer = S.bob,
                  TurnRegisters.priority = Just S.bob,
                  TurnRegisters.phase = Phase.Combat CombatStep.DeclareAttackers,
                  TurnRegisters.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]}
                }
          }
      fight = Combat.declareAttackers S.manaPerformer S.bob >> Combat.declareBlockers S.manaPerformer >> Damage.dealCombatDamage Settle.none
   in (before, S.settleSba (S.runPure (S.attackTo S.alice) before fight))

runedHaloSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runedHaloSpec s registry =
  Spec.describe s "RunedHalo" $ do
    -- CR 612.7 with CR 108.1: Spy Kit's host has every nonlegendary creature
    -- card's name in the Oracle card reference, not just the game's. The Halo
    -- names Goblin Piker, which is in no zone, so only Prompt.LookUpCard's answer
    -- puts it in reach; the attacker is a Hill Giant.
    --
    -- FOUR combats differing in one thing each: no Kit, a legendary creature
    -- card named instead (the filter, not "every name"), and the reference never
    -- consulted (the lookup is load-bearing).
    Spec.it s "CR 612.7 / 702.16e a Spy Kit host has the chosen name of a card in no zone, and its damage is prevented" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      jedit <- S.printingOf s registry "Jedit Ojanen"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      namedPiker <- lookedUpHalo s registry pikerName board haloId
      namedJedit <- lookedUpHalo s registry (S.printingName jedit) board haloId
      let (beforeKit, withKit) = spyKitCombat giant (Just kit) namedPiker
          (beforeBare, bare) = spyKitCombat giant Nothing namedPiker
          (beforeLegend, legend) = spyKitCombat giant (Just kit) namedJedit
          (beforeUnasked, unasked) = spyKitCombat giant (Just kit) (castHalo pikerName board haloId)
      Spec.assertEqWith s "the Kit's host is named Goblin Piker, so alice takes none of its 4" (S.lifeOf S.alice withKit) (S.lifeOf S.alice beforeKit)
      Spec.assertEqWith s "unequipped, the Giant is no Goblin Piker and deals its 3" (S.lifeOf S.alice bare) (fmap (subtract 3) (S.lifeOf S.alice beforeBare))
      Spec.assertEqWith s "a legendary creature card's name is not the host's, so alice takes 4" (S.lifeOf S.alice legend) (fmap (subtract 4) (S.lifeOf S.alice beforeLegend))
      Spec.assertEqWith s "with the reference never asked, alice takes 4" (S.lifeOf S.alice unasked) (fmap (subtract 4) (S.lifeOf S.alice beforeUnasked))
      Spec.assertBool s (not (Map.member pikerName (Game.referenceFaces board))) "no Goblin Piker card is anywhere in the game before the Halo names it"
    -- CR 108.1: the Halo's name looked up in a reference that cannot read its
    -- card files. The registry's failure goes to the host's handler, here
    -- Left, rather than reading as no card by that name; the control's
    -- registry answers no card instead, and the Halo resolves.
    Spec.it s "CR 108.1 a lookup the registry cannot answer goes to the host's handler" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          corpus = InvalidCorpus.MkInvalidCorpus "cards" ["cards/goblin-piker.json: unreadable"]
          answering found = Registry.MkRegistry {Registry.fetchCard = const (pure found), Registry.cards = pure (fmap (const []) found), Registry.printings = PrintingTable.empty, Registry.stickerSheets = Map.empty}
          naming reference = fmap snd (Engine.runGameAsked (Interpreter.lookingUpCards reference Left (namingAnswer (CardName.MkCardName (Text.pack "Goblin Piker")))) board (S.cast S.alice haloId >> Stack.resolveTop))
      case naming (answering (Left corpus)) of
        Left reached -> Spec.assertEqWith s "the registry's failure reaches the handler" reached corpus
        Right _ -> Spec.assertFailure s "the registry's failure read as no card"
      Spec.assertBool s (Either.isRight (naming (answering (Right Nothing)))) "a reference with no such card lets the Halo resolve"
      -- CR 201.4's judgement of a chosen name reads the same reference.
      let judging found = Interpreter.legalCardName (answering found) Left board S.alice (Filter.Type.And []) (CardName.MkCardName (Text.pack "Goblin Piker"))
      Spec.assertEqWith s "judging a name hands the failure to the handler too" (judging (Left corpus)) (Left corpus)
      Spec.assertEqWith s "and a reference with no such card refuses the name" (judging (Right Nothing)) (Right False)
    -- CR 612.7 / 206.3a: City in a Bottle's state trigger (CR 603.8) sweeps the
    -- other nontoken permanents with a name originally printed in Arabian Nights,
    -- and a Spy Kit host has every such nonlegendary creature card's name --
    -- Kird Ape's among them -- though no such card is in the game. Two boards
    -- differing in one thing: the reference answered by the suite's registry
    -- (Parun.Interpreter.lookingUpCards), and never consulted.
    Spec.it s "CR 612.7 / 206.3a a Spy Kit host has the Arabian Nights names of cards the game has never seen, and City in a Bottle sweeps it" $ do
      bottle <- S.printingOf s registry "City in a Bottle"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      let (bottleId, g1) = S.addPermanent bottle S.alice S.threePlayerGame
          (giantId, g2) = S.addPermanent giant S.alice g1
          (kitId, g3) = S.addPermanent kit S.alice g2
          board =
            Game.overTurn
              ( \registers ->
                  registers
                    { TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.priority = Just S.alice
                    }
              )
              (S.attachTo kitId (Recipient.ToObject giantId) g3)

          kirdApe = CardName.MkCardName (Text.pack "Kird Ape")
          unasked = S.runPure S.identityAnswer board Engine.priorityLoop
      (_, asked) <- Engine.runGameAsked (Interpreter.lookingUpCards registry (S.brokenCorpus s) (pure . S.identityAnswer . Asked.prompt)) board Engine.priorityLoop
      Spec.assertBool s (not (S.onBattlefield giantId asked)) "CR 603.8 the host, named Kird Ape among others, was sacrificed"
      Spec.assertBool s (S.onBattlefield giantId unasked) "with the reference never consulted it survives"
      Spec.assertBool s (S.onBattlefield bottleId asked && S.onBattlefield kitId asked) "the Bottle and the Kit themselves stay"
      Spec.assertBool s (not (Map.member kirdApe (Game.referenceFaces board))) "no Kird Ape card is anywhere in the game"
    -- CR 612.7 / 709.4a/c: a split card with a creature half is a nonlegendary
    -- creature card with two names, so a Spy Kit host has its instant half's
    -- name too. The split card sits in bob's hand, since a synthetic is no card
    -- of the registry's reference. Two boards differing in one thing: the split
    -- card in the game, and not.
    Spec.it s "CR 612.7 / 709.4a a Spy Kit host has both names of a split card with a creature half" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      split <- S.printingOf s registry "Synthetic Mirror Rite"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          (_, withSplit) = S.addHandCard split S.bob board
          rite = CardName.MkCardName (Text.pack "Synthetic Mirror Rite")
          (beforeSplit, withSplitAfter) = spyKitCombat giant (Just kit) (castHalo rite withSplit haloId)
          (beforeAbsent, absent) = spyKitCombat giant (Just kit) (castHalo rite board haloId)
      Spec.assertEqWith s "the host is named Synthetic Mirror Rite, so alice takes none of its 4" (S.lifeOf S.alice withSplitAfter) (S.lifeOf S.alice beforeSplit)
      Spec.assertEqWith s "with no such split card in the game, alice takes 4" (S.lifeOf S.alice absent) (fmap (subtract 4) (S.lifeOf S.alice beforeAbsent))

-- The Stasis Coffin {3} Legendary Artifact: "{2}, {T}, Exile The Stasis Coffin:
-- You gain protection from everything until your next turn." A card that gives
-- a PLAYER protection from a quality the CARD states
-- (PlayerEffect.HasProtectionFrom), where Runed Halo above states CR 201.4's
-- chosen name instead -- and CR 702.16j's "everything" is that quality written as
-- the empty conjunction.
--
-- The STORED carrier, which is the half Runed Halo cannot reach: the Coffin
-- exiles itself to pay for its own ability (CR 601.2h), so by the time any of the
-- three prohibitions is read the source is in exile and the quality can only have
-- come off the row.
--
-- Curse of Vitality is the enchant-PLAYER Aura on the other side of rules 702.16b
-- and 702.16c, as it is for Runed Halo; the Goblin Piker is rule 702.16e's
-- attacker.
--
-- THREE SEATS, for runedHaloBoard's reason: alice holds the Coffin, bob holds the
-- Curse and the Piker, and carol holds nothing, so "bob may not target alice" is
-- told apart from "bob may not target anybody".
--
-- alice's two Plains are exactly the ability's {2} and bob's three are exactly the
-- Curse's {2}{W}, so no case below can turn on an unaffordable cost.
stasisCoffinBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
stasisCoffinBoard plains coffin curse piker =
  let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 2 S.threePlayerGame)
      (coffinId, g1) = S.addPermanent coffin S.alice lands
      (curseId, g2) = S.addHandCard curse S.bob g1
      (pikerId, g3) = S.addPermanent piker S.bob g2
   in ( coffinId,
        curseId,
        pikerId,
        g3
          { GameState.turn =
              (GameState.turn g3)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- Activate the Coffin's one ability and let it resolve. The ability is read off
-- the PROJECTION rather than the printed face, Parun.AuraSpec's fortify cases'
-- spelling.
activateCoffin :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateCoffin coffinId gs = case Projection.abilitiesOf coffinId gs of
  [ability] -> S.runPure S.identityAnswer gs (Activate.activateAbility S.alice coffinId ability >> Stack.resolveTop)
  _ -> gs

stasisCoffinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stasisCoffinSpec s registry =
  Spec.describe s "TheStasisCoffin" $ do
    -- CR 702.16j with CR 702.16c's second sentence: "such Auras attached to the
    -- permanent or player with protection will be put into their owners'
    -- graveyards as a state-based action" (CR 704.5m), which
    -- Parun.Engine.Sba.fallsOff answers through
    -- Parun.Engine.PlayerEffect.protectedFrom.
    --
    -- The Curse is attached BEFORE the ability resolves, which is the order the
    -- rule is about.
    Spec.it s "CR 702.16j / 702.16c an Aura already enchanting alice is buried once she gains protection from everything" $ do
      plains <- S.printingOf s registry "Plains"
      coffin <- S.printingOf s registry "The Stasis Coffin"
      curse <- S.printingOf s registry "Curse of Vitality"
      piker <- S.printingOf s registry "Goblin Piker"
      let (coffinId, _, _, base) = stasisCoffinBoard plains coffin curse piker
          (aura, withAura) = S.addPermanent curse S.bob base
          cursed = S.attachTo aura (Recipient.ToPlayer S.alice) withAura
          after = S.settleSba (activateCoffin coffinId cursed)
      Spec.assertBool s (not (S.onBattlefield aura after)) "the Curse is off the battlefield after one pass"
      Spec.assertEqWith s "in its OWNER's graveyard, and bob owns it" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertBool s (S.onBattlefield aura (S.settleSba cursed)) "and without the ability it stays where it is"

-- City in a Bottle {2} Artifact -- second sentence: "Players can't cast spells
-- or play lands with a name originally printed in the Arabian Nights
-- expansion." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-09-06.)
--
-- CR 206.3a defines the name list, so both halves are one
-- Filter.HasNameOriginallyPrintedIn; what is new is the PLAY half. CR 305.1 makes playing a land a
-- special action that never uses the stack, so the cast-side prohibition beside
-- it reaches no land however its Filter reads -- which is why
-- PlayerEffect.CantPlayLands now carries a Filter of its own and
-- Action.playableLands hands prohibitsPlayingLand the object it narrows by.
--
-- Desert is the listed land and the Mountain is the falsifier: they differ only
-- in whether CR 206.3a names them, and a prohibition that narrowed nothing --
-- Damping Engine's reading -- would take the Mountain too. The pair of BOARDS
-- differing in one thing is the Bottle itself, which is what says the denial is
-- the card's rather than the fixture's.
--
-- bob is asked as well, because CR 611.1's "players" is every seat and not the
-- Bottle's controller alone.
cityInABottleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cityInABottleSpec s registry =
  Spec.describe s "CityInABottle" $ do
    Spec.it s "CR 305.1 a land whose name CR 206.3a lists can't be played, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      desert <- S.printingOf s registry "Desert"
      bottle <- S.printingOf s registry "City in a Bottle"
      let hands base =
            let (aliceDesert, g1) = S.addHandCard desert S.alice base
                (aliceMountain, g2) = S.addHandCard mountain S.alice g1
                (bobDesert, g3) = S.addHandCard desert S.bob g2
             in (aliceDesert, aliceMountain, bobDesert, g3)
          -- alice's own main phase with priority in hand, which is what makes
          -- the OFFER assertions below say something: CR 305.1's window is
          -- Action.legalActions', and outside it no land is offered at all.
          mainPhase base =
            base
              { GameState.turn =
                  (GameState.turn base)
                    { TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.priority = Just S.alice
                    }
              }
          (_, withBottle) = S.addPermanent bottle S.alice (mainPhase (S.landsInPlay plains 4))
          (aliceDesertId, aliceMountainId, bobDesertId, gs) = hands withBottle
          (freeDesertId, _, _, before) = hands (mainPhase (S.landsInPlay plains 4))
          playable = Action.playableLands S.alice gs
      Spec.assertBool s (notElem (aliceDesertId, Nothing) playable) "CR 305.1 alice's Desert, whose name CR 206.3a lists, is not playable"
      Spec.assertBool s (elem (aliceMountainId, Nothing) playable) "her Mountain, which that list does not name, still is"
      Spec.assertBool s (notElem (Action.Type.Play aliceDesertId Nothing) (Action.legalActions S.alice gs)) "and no Play is offered for the Desert"
      Spec.assertBool s (elem (Action.Type.Play aliceMountainId Nothing) (Action.legalActions S.alice gs)) "while the Mountain is offered"
      Spec.assertBool s (notElem (bobDesertId, Nothing) (Action.playableLands S.bob gs)) "CR 611.1 bob's Desert is stopped too, the sentence naming every seat"
      Spec.assertBool s (elem (freeDesertId, Nothing) (Action.playableLands S.alice before)) "and the same Desert is playable on the board the Bottle is missing from"

    -- The cast half on the same board, which the arm above cannot answer for: CR
    -- 305.1 keeps the two gates apart, so a Filter on either one alone would
    -- leave the other sentence unwritten. Asked through prohibitsCasting rather
    -- than through legalActions, so that the negative cannot pass for want of
    -- mana.
    Spec.it s "CR 601.3a a spell whose name CR 206.3a lists can't be cast, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      kirdApe <- S.printingOf s registry "Kird Ape"
      piker <- S.printingOf s registry "Goblin Piker"
      bottle <- S.printingOf s registry "City in a Bottle"
      let (_, withBottle) = S.addPermanent bottle S.alice (S.landsInPlay plains 4)
          (apeId, withApe) = S.addHandCard kirdApe S.alice withBottle
          (pikerId, gs) = S.addHandCard piker S.alice withApe
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice apeId VariableChoice.Announced gs) "CR 601.3a alice may not cast her Kird Ape, whose name CR 206.3a lists"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice pikerId VariableChoice.Announced gs)) "and her Goblin Piker, which that list does not name, is untouched"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.PlayerEffect" $ do
  nullChamberSpec s registry
  runedHaloSpec s registry
  stasisCoffinSpec s registry
  cityInABottleSpec s registry
  silenceSpec s registry
  ceaseFireSpec s registry
  conditionalSilenceSpec s registry
  matchesObjectSpec s registry
