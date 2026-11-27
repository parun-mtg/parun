{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Parun.Engine.Replacement over enters-the-battlefield replacements (CR 614.1c,
-- CR 614.12, CR 122.6): riot, unleash and bloodthirst, entering tapped or with
-- counters, and the as-enters choices from Agent's Toolkit to Squad Captain.
-- Split out of Parun.ReplacementSpec, which keeps the machinery.
module Parun.EntryReplacementSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.FaceDown as FaceDown
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Keyword as Keyword.Engine
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Int as Int
import Parun.PreventionSpec (answersFor, castAndResolve, counterBoard, countersOn, newestNamed, raceAnswer, theAbility, wasAskedToReplace)
import qualified Parun.Registry as Registry
import Parun.ReplacementSpec (controlledNamed, declineLastRiot, riotBoard, riotChoosing, wasAskedForRiot)
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.FaceDownReason as FaceDownReason
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.Response as Response
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.TurnUpProcedure as TurnUpProcedure

riotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
riotSpec s registry = Spec.describe s "Riot (CR 702.136)" $ do
  -- THE CHOICE IS THE ANSWERER'S. Both outcomes above are reachable only through
  -- a prompt, and the prompt is never elided: CR 702.136a's two halves are
  -- distinguishable on every board.
  Spec.it s "CR 702.136a the controller is asked, and the engine chooses nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The control that keeps the case above from passing for the wrong reason: a
  -- creature WITHOUT riot enters through the same funnel and is asked nothing.
  Spec.it s "CR 702.136a a creature without riot raises no riot prompt" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, held) = riotBoard mountain 3 forest 0 [pikerPrinting]
    case held of
      pikerCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice pikerCard >> Stack.resolveTop)
         in Spec.assertBool s (not (wasAskedForRiot asked)) "no ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136b's "each works separately" read at its sharpest: the rule's own
  -- consequence is that one instance may take the counter while the other takes
  -- haste, which no single application of riot can produce -- CR 702.136a's two
  -- halves are exclusive.
  Spec.it s "CR 702.136b one instance takes the counter and the other takes haste" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        (_, board) = S.addPermanent rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let play = S.cast S.alice goblinCard >> Stack.resolveTop
            script = declineLastRiot (answersFor answer board play)
            ((_, after), desync) = Replay.replay script board play
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                -- An exhausted or mismatched transcript would fall back to
                -- Replay.defaultAnswer, which would decide riot itself.
                Spec.assertBool s (Maybe.isNothing desync) "the transcript answered every prompt"
                Spec.assertEqWith s "the exercised instance's counter" (countersOn CounterKind.PlusOnePlusOne goblin after) 1
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste goblin after) "and the declined instance's haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"

-- alice controls three untapped Swamps on a THREE-SEAT board and holds one
-- Bloodrage Vampire, in her precombat main phase with priority; bob controls one
-- Ogre Sentry. Returns the state, the card in hand and the Sentry.
--
-- Three seats rather than riotBoard's two, and for rule 702.54a's word "an
-- opponent": a two-player board collapses "an opponent" onto the only other
-- player, so a reading that admitted the entry whenever ANY player was dealt
-- damage could not be told from one that admitted it only for an opponent's.
bloodthirstBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bloodthirstBoard swamp vampire sentry =
  let lands = S.landsFor swamp S.alice 3 S.threePlayerGame
      (bobsSentry, withSentry) = S.addPermanent sentry S.bob lands
      (held, gs) = S.addHandCard vampire S.alice withSentry
   in (readyForAlice gs, held, bobsSentry)

-- alice controls seven untapped Forests on a THREE-SEAT board and holds one
-- Petrified Wood-Kin ({6}{G} 3/3, "This spell can't be countered. / Bloodthirst X
-- / Protection from instants"), in her precombat main phase with priority; bob
-- controls one Ogre Sentry, the source every damage event in the group names.
-- Returns the state, the card in hand and the Sentry.
--
-- Three seats for bloodthirstBoard's reason and for one more: rule 702.54b's
-- "your opponents" is a SUM ACROSS PLAYERS, which two seats cannot tell from one
-- player's own total.
bloodthirstXBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bloodthirstXBoard forest woodKin sentry =
  let lands = S.landsFor forest S.alice 7 S.threePlayerGame
      (bobsSentry, withSentry) = S.addPermanent sentry S.bob lands
      (held, gs) = S.addHandCard woodKin S.alice withSentry
   in (readyForAlice gs, held, bobsSentry)

-- alice, in her precombat main phase, with priority. Applied by the board
-- fixtures around it.
readyForAlice :: GameState.GameState -> GameState.GameState
readyForAlice gs =
  gs
    { GameState.turn =
        (GameState.turn gs)
          { TurnRegisters.phase = Phase.PrecombatMain,
            TurnRegisters.activePlayer = S.alice,
            TurnRegisters.priority = Just S.alice
          }
    }

-- alice holds one card and controls exactly the lands named, untapped, in her
-- precombat main phase with priority. Returns the state and the card in hand.
--
-- EXACTLY the lands the cast needs, which is what pins the payment without
-- pinning an answer by index: a {3} or {5} generic cost against that many
-- untapped sources leaves the payer no colour to choose, so what was spent is
-- the board rather than the answerer.
sunburstBoard :: [(Printing.Printing, Int)] -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
sunburstBoard lands card =
  let base = List.foldl' (\acc (land, n) -> S.landsFor land S.alice n acc) (Setup.emptyGame S.bothPlayers) lands
      (held, gs) = S.addHandCard card S.alice base
   in (readyForAlice gs, held)

-- CR 702.44: sunburst, on Suntouched Myr ({3} 0/0 artifact creature, "Sunburst"
-- and nothing else) for rule 702.44a's creature half and on Clearwater Goblet
-- ({5} artifact, sunburst plus an upkeep ability counting its charge counters)
-- for the other. A minted CR 614.1c row whose count AND counter kind are both
-- read off the entering object, which is what makes it neither bloodthirst's
-- shape nor riot's.
--
-- ONE BOARD PER CASE, differing in nothing but which lands pay for the spell:
-- the same seat, the same card, the same cast. What the assertions tell apart is
-- how many counters of WHICH kind the permanent entered with.
--
-- Distinct numbers everywhere, so no two readings coincide: three lands of three
-- colours are 3 counters where three Plains are 1 (mana spent is not colours
-- spent), and the Goblet's five lands carry four colours (its charge counters
-- are 4, not 5 and not 0).
sunburstSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sunburstSpec s registry =
  let enters = castAndResolve S.aggressiveAnswer
      myrIn = newestNamed (CardName.MkCardName $ Text.pack "Suntouched Myr")
   in Spec.describe s "Sunburst (CR 702.44)" $ do
        -- CR 702.44b's "only if one or more colored mana was spent": the same
        -- three mana again, none of them coloured.
        Spec.it s "CR 702.44b colourless mana buys no counter at all" $ do
          fountain <- S.printingOf s registry "Radiant Fountain"
          myr <- S.printingOf s registry "Suntouched Myr"
          let (gs, held) = sunburstBoard [(fountain, 3)] myr
              after = enters gs held
          case myrIn after of
            Nothing -> Spec.assertFailure s "Suntouched Myr did not reach the battlefield"
            Just oid -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne oid after) 0
              Spec.assertEqWith s "power" (Projection.powerOf oid after) (Just 0)

-- CR 702.54: bloodthirst, on Bloodrage Vampire ({2}{B} 3/1 Vampire, "Bloodthirst
-- 1" and nothing else) for rule 702.54a's N, and on Petrified Wood-Kin ({6}{G}
-- 3/3, "Bloodthirst X") for rule 702.54b's X. The second minted entry replacement
-- whose own rule states a condition, after rule 702.145b's daybound, which is why
-- Parun.Engine.Replacement.admitsEntry now has two arms that are not `True` --
-- and rule 702.54b's X is not one of them, since that variant states none.
--
-- ONE BOARD PER FORM, and every case differs from its siblings in nothing but
-- what happened before the cast: the same three seats, the same lands, the same
-- card cast the same way. The one exception is the CR 616.1e pair below, which
-- differs in one permanent -- bob's Kismet -- and says so. The permanent ENTERS
-- in every case, so what the assertions tell apart is "entered with counters"
-- from "entered", never "entered" from "did not".
--
-- Distinct numbers everywhere, so no two readings coincide: bloodthirst 1 on a
-- printed 3/1 shows as a 4/2, and the damage amounts are 4 at bob, 5 at alice, 2
-- at bob's Sentry (which a 3/3 survives) and 6 of life paid. The X cases pick
-- theirs the same way, and each case says which readings its numbers separate.
--
-- The damage goes in through Damage.applyDamage, the funnel that records the
-- event, exactly as Parun.TriggerSpec's Furious Spinesplitter group does -- and
-- everything downstream of the record is the card's own, driven through a real
-- cast and a real resolution.
bloodthirstSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodthirstSpec s registry =
  let hit src target amount gs =
        S.runPure
          S.identityAnswer
          gs
          (Damage.applyDamage Settle.none [DamageEvent.MkDamageEvent src target amount 0 False False False 0 Nothing Nothing mempty False mempty mempty mempty mempty Nothing mempty mempty Nothing DamageKind.Noncombat])
      enters = castAndResolve S.aggressiveAnswer
      vampireIn = newestNamed (CardName.MkCardName $ Text.pack "Bloodrage Vampire")
      woodKinIn = newestNamed (CardName.MkCardName $ Text.pack "Petrified Wood-Kin")
   in Spec.describe s "Bloodthirst (CR 702.54)" $ do
        -- THE PAIR THAT MAKES THE CONDITION REAL. The board above with one damage
        -- event added and nothing else changed.
        Spec.it s "CR 702.54a an opponent was dealt damage, so it enters a 4/2" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.bob) 4 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne vamp after) 1
              -- Printed 3/1, so the counter shows in the projection (CR 613.4c,
              -- layer 7c).
              Spec.assertEqWith s "power" (Projection.powerOf vamp after) (Just 4)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf vamp after) (Just 2)
        -- CR 102.2 / 109.5: "an opponent" is a player other than the entering
        -- permanent's controller, so alice's own damage is not an opponent's.
        -- Unreachable on a two-seat board, which is why this group takes three.
        Spec.it s "CR 102.2 damage to the controller herself does not turn it on" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.alice) 5 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 120.3a names the PLAYER recipient; a creature bob controls is not bob.
        Spec.it s "CR 120.3a damage to an opponent's creature does not either" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              damaged = hit bobsSentry (Recipient.ToCreature bobsSentry) 2 gs0
              after = enters damaged held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
              Spec.assertBool s (Set.member bobsSentry (GameState.battlefield after)) "and the 3/3 Sentry survived the 2, so the board is otherwise the same"
        -- CR 119.4's life loss is not CR 120.1's damage, and the log records the
        -- two separately. Without this the condition could be reading
        -- GameEvent.LifeLost and pass every case above, since damage to a player
        -- files one of those too.
        Spec.it s "CR 119.4 life lost without damage is not damage" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstBoard swamp vampire sentry
              after = enters (S.runPure S.identityAnswer gs0 (Event.payLife Settle.none S.bob 6)) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 702.54c: "if an object has multiple instances of bloodthirst, each
        -- applies separately." Asserted at the mint rather than at gameplay level,
        -- because nothing in the pool prints or grants a second instance -- the
        -- same footing Parun.TriggerSpec's vanishing group states its rule 702.63c
        -- claim on.
        Spec.it s "CR 702.54c each instance is its own row" $
          Spec.assertEqWith
            s
            "bloodthirst 1 held twice mints two rows"
            (Keyword.Engine.mintedReplacementsFor (Keyword.Bloodthirst (Just 1)) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.Bloodthirst (Just 1)))))
        -- CR 702.54b: bloodthirst X on Petrified Wood-Kin. THE PAIR IS THIS CASE
        -- AND THE ONE BELOW, one board apart in nothing but what was dealt.
        --
        -- 3 at bob and 2 at carol, so every reading of the rule reaches a
        -- different number: rule 702.54b's sum is 5, rule 702.54a's tally of
        -- damaged opponents is 2, one seat's total is 3, and a lifetime count of
        -- damage events is 2. A printed 3/3 makes the projection 8/8, which no
        -- other reading produces either.
        Spec.it s "CR 702.54b X is the total damage its controller's opponents were dealt" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstXBoard forest woodKin sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.carol) 2 (hit bobsSentry (Recipient.ToPlayer S.bob) 3 gs0)
              after = enters damaged held
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> do
              Spec.assertEqWith s "five +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne kin after) 5
              -- Printed 3/3, so the counters show in the projection (CR 613.4c,
              -- layer 7c).
              Spec.assertEqWith s "power" (Projection.powerOf kin after) (Just 8)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf kin after) (Just 8)
        -- The DISCRIMINATING TWIN: the same board without Kismet, where rule
        -- 702.54b's row is alone in its bucket and there is nothing to ask.
        -- Without it "a prompt was raised" above would pass under a recorder that
        -- reported one on every board.
        Spec.it s "CR 616.1 the bloodthirst X row alone asks nobody" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstXBoard forest woodKin sentry
              cast = S.cast S.alice held >> Stack.resolveTop
          Spec.assertBool s (not (wasAskedToReplace (answersFor S.aggressiveAnswer gs0 cast))) "no ChooseReplacement was raised"
        -- CR 109.5: "your opponents" excludes the entering permanent's own
        -- controller. 7 at alice and 3 at bob, so a sum over every player would be
        -- 10 and a sum over the opponents is 3.
        Spec.it s "CR 109.5 damage to the controller herself is not in X" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstXBoard forest woodKin sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.bob) 3 (hit bobsSentry (Recipient.ToPlayer S.alice) 7 gs0)
              after = enters damaged held
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> Spec.assertEqWith s "three +1/+1 counters, not ten" (countersOn CounterKind.PlusOnePlusOne kin after) 3

-- CR 614.1c with CR 120.3a. Stuffy Doll, {5} Artifact Creature -- Construct 0/1,
-- whole text: "Indestructible / As this creature enters, choose a player. /
-- Whenever this creature is dealt damage, it deals that much damage to the chosen
-- player. / {T}: This creature deals 1 damage to itself." (oracle checked on
-- Scryfall)
--
-- The first card whose as-enters choice is a PLAYER, and the first whose payload
-- reads one back. Both halves are proved by ONE observable -- whose life total
-- moved -- so a stamp with no reader, and a reader with no stamp, both fail.
--
-- THREE SEATS, and the pair of boards differs in exactly one thing: WHOM the
-- ChoosePlayer answer names. Two seats would collapse "the chosen player" onto
-- the only opponent, and the assertion would pass under a hard-coded "an
-- opponent" as happily as under the field.
--
-- The amount is 3, which is neither the Doll's power (0) nor its toughness (1)
-- nor the {T} ability's 1, so a payload reading a characteristic instead of CR
-- 603.2's captured binding fails rather than passing by luck.
stuffyDollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stuffyDollSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settleAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- A noncombat event's own path, enrageSpec's: applyDamage records the
      -- DamageDealt entries, the settle puts what they triggered on the stack
      -- (CR 603.3), and the priority loop resolves it.
      dealing events gs = resolveAll (settleAll (S.runPure S.identityAnswer gs (Damage.applyDamage Settle.none events)))
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount 0 False False False 0 Nothing Nothing mempty False mempty mempty mempty mempty Nothing mempty mempty Nothing DamageKind.Noncombat
      lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g, S.lifeOf S.carol g)
      chosenOn oid g = Game.lookupObject oid g >>= Map.lookup PlayerChoice.ChosenPlayer . Map.findWithDefault Map.empty Acquisition.Own . Object.chosenPlayers
      -- alice casts the Doll off five Mountains on a three-seat board and answers
      -- CR 614.12a's choice with `who`. The Doll must be CAST: S.addPermanent puts
      -- an object straight onto the battlefield without running the entry loop,
      -- so it would choose nobody.
      --
      -- The answer is pinned to a PlayerId by identity rather than by an index
      -- into the offer: an answerer that searched the candidate list for a legal
      -- seat would find a different one after a mutation and repair the assertion.
      castDoll :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (GameState.GameState, Maybe ObjectId.ObjectId, [Response.Response])
      castDoll doll mountain who =
        let (withCard, oid) = S.handOne doll (S.landsFor mountain S.alice 5 S.threePlayerGame)
            step :: Prompt.Prompt r -> r
            step p = case p of
              Prompt.ChoosePlayer {} -> who
              _ -> S.identityAnswer p
            ((_, after), answers) = Replay.record step withCard (S.cast S.alice oid >> Stack.resolveTop)
            entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield withCard)) of
              o : _ -> Just o
              [] -> Nothing
         in (after, entered, answers)
   in Spec.describe s "Stuffy Doll (CR 614.1c)" $ do
        -- THE PROVING CASE, and a pair of boards differing only in the answer.
        Spec.it s "CR 614.1c the chosen player, and only they, take the damage" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let hit who = case castDoll doll mountain who of
                (gs, Just dollId, _) ->
                  let (pikerId, board) = S.addPermanent piker S.bob gs
                   in Just (board, dollId, dealing [noncombat pikerId dollId 3] board)
                _ -> Nothing
          case (hit S.bob, hit S.carol) of
            (Just (before, dollId, chosenBob), Just (_, _, chosenCarol)) -> do
              Spec.assertEqWith s "all three seats start at 20" (lives before) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "CR 120.3a bob was chosen, so bob loses 3" (lives chosenBob) (Just 20, Just 17, Just 20)
              -- The same board and the same amount, one different answer.
              Spec.assertEqWith s "carol chosen instead, so carol loses 3" (lives chosenCarol) (Just 20, Just 20, Just 17)
              -- CR 120.3e: the damage really landed on the Doll, so the two
              -- assertions above are this trigger and not bookkeeping.
              Spec.assertEqWith s "CR 120.3e and the 3 is marked on the Doll" (fmap Object.damage (Game.lookupObject dollId chosenBob)) (Just 3)
              -- CR 702.12b: 3 over a toughness of 1 is lethal, and indestructible
              -- keeps it on the battlefield anyway.
              Spec.assertBool s (Set.member dollId (GameState.battlefield chosenBob)) "CR 702.12b indestructible: still on the battlefield"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- CR 608.2h: the proving case's board with carol chosen, the Doll given
        -- -2/-2 and buried by CR 704.5f while its trigger waits. The chosen player
        -- is read off the last known information, so carol still loses 3.
        Spec.it s "CR 608.2h a Doll that has left still damages the chosen player" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          case castDoll doll mountain S.carol of
            (gs, Just dollId, _) -> do
              let (pikerId, board) = S.addPermanent piker S.bob gs
                  triggered = settleAll (S.runPure S.identityAnswer board (Damage.applyDamage Settle.none [noncombat pikerId dollId 3]))
                  minusTwo = Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-2)))
                  gone = settleAll (S.withEffect dollId minusTwo triggered)
                  after = resolveAll gone
              Spec.assertEqWith s "CR 608.2h carol was chosen, so carol loses 3" (lives after) (Just 20, Just 20, Just 17)
              Spec.assertEqWith s "and the trigger was on the stack as the Doll left" (length (TurnRegisters.stack (GameState.turn gone)), Set.member dollId (GameState.battlefield gone)) (1, False)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- The STAMP, asserted independently of the payload, so a JSON typo in the
        -- trigger cannot hide behind a green read-back -- and the other way round.
        Spec.it s "CR 614.12a the choice is made before the permanent enters" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.carol of
            (gs, Just dollId, answers) -> do
              Spec.assertEqWith s "CR 614.1c the Doll remembers carol" (chosenOn dollId gs) (Just S.carol)
              -- The SECOND INVARIANT, asserted directly: the engine did not pick a
              -- seat, it asked. Three seats make CR 102.1's offer three wide, so
              -- the prompt is a real question rather than an elided one.
              Spec.assertBool s (List.elem (Response.ChosePlayer S.carol) answers) "CR 614.12a the engine raised a ChoosePlayer prompt"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- CR 400.7: a NEW object forgets the choice. Object.newIncarnation is a
        -- record UPDATE, so omitting the field there compiles and silently carries
        -- a chosen player across a zone change; -Werror cannot name that site, and
        -- this is what stands in its place. Mirrors Parun.GameSpec's Painter's
        -- Servant chosenColors case.
        Spec.it s "CR 400.7 a new incarnation has chosen nobody" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.bob of
            (gs, Just dollId, _) -> do
              -- The discriminator: without it an assertion over an object that
              -- never chose anybody would pass whatever newIncarnation does.
              Spec.assertEqWith s "the object going in genuinely carried a chosen player" (chosenOn dollId gs) (Just S.bob)
              Spec.assertEqWith
                s
                "CR 400.7 the rebuilt object forgot the player it chose"
                (fmap (Map.lookup PlayerChoice.ChosenPlayer . Map.findWithDefault Map.empty Acquisition.Own . Object.chosenPlayers . Object.newIncarnation) (Game.lookupObject dollId gs))
                (Just Nothing)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"

-- CR 122.1 / 122.6 with CR 614.1: Vorinclex, Monstrous Raider ({4}{G}{G}
-- Legendary Creature -- Phyrexian Praetor 6/6, "Trample, haste. If you would put
-- one or more counters on a permanent or player, put twice that many . . .
-- instead. If an opponent would put one or more counters on a permanent or
-- player, they put half that many . . . instead, rounded down.").
--
-- The card the player-counter funnel needed: its own ruling says it "cares deeply
-- about who is putting the counters", so a board that reads the RECIPIENT instead
-- answers differently on every case below but the first.
--
-- Every count is chosen so the three readings differ: three energy is six
-- doubled, one halved and three unreplaced, and one poison counter is two, zero
-- and one.
vorinclexSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vorinclexSpec s registry = Spec.describe s "Vorinclex, Monstrous Raider (CR 122.1)" $ do
  let -- THREE seats: at two, "an opponent" and "the other player" are the same
      -- seat, and carol is what tells CR 122.6a's putter apart from a reading
      -- that asks who is affected.
      --
      -- `withVorinclex` is the only difference between a case and its control --
      -- same printing, same seat, same trigger.
      board withVorinclex printing pid = do
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let seated = if withVorinclex then snd (S.addPermanent vorinclex S.alice S.threePlayerGame) else S.threePlayerGame
            (oid, entered) = S.entersWithTrigger printing pid seated
        pure (oid, S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority))
      energyIn = S.playerCounterOf PlayerCounterKind.Energy S.alice
      poisonIn g = (S.playerCounterOf PlayerCounterKind.Poison S.alice g, S.playerCounterOf PlayerCounterKind.Poison S.bob g, S.playerCounterOf PlayerCounterKind.Poison S.carol g)
  -- CR 122.6: the gain reaches the CR 616.1 loop at all. Without the funnel the
  -- energy lands unreplaced at three, which is the control below.
  Spec.it s "CR 614.1 alice's own three energy are doubled to six" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, doubled) <- board True sage S.alice
    (_, plain) <- board False sage S.alice
    Spec.assertEqWith s "twice that many" (energyIn doubled) 6
    Spec.assertEqWith s "and three without the praetor" (energyIn plain) 3
  -- CR 107.1a's rounding. bob is putting these on HIMSELF, so this case does not
  -- separate the putter from the recipient -- both are bob, and either reading
  -- halves. What it does prove is the halving clause and its rounding: three is
  -- one, not two and not three. The putter axis is the Ichor Rats case below.
  Spec.it s "CR 107.1a bob's three energy are halved to one, rounded down" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, halved) <- board True sage S.bob
    (_, plain) <- board False sage S.bob
    Spec.assertEqWith s "half of three, rounded down" (S.playerCounterOf PlayerCounterKind.Energy S.bob halved) 1
    Spec.assertEqWith s "and three without the praetor" (S.playerCounterOf PlayerCounterKind.Energy S.bob plain) 3
    Spec.assertEqWith s "alice, who put none on herself, has none" (energyIn halved) 0
  -- CR 122.6a: alice is the one PUTTING all three counters, so all three are
  -- doubled -- including the ones bob and carol receive, who are her opponents.
  -- This is the case a recipient-based reading gets wrong in both directions at
  -- once: bob's and carol's would be halved to zero instead.
  Spec.it s "CR 122.6a alice's Ichor Rats poisons the whole table twice over" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, doubled) <- board True ichorRats S.alice
    (_, plain) <- board False ichorRats S.alice
    Spec.assertBool s (S.onBattlefield rats doubled) "the Rats are on the battlefield"
    Spec.assertEqWith s "twice that many, for opponents too" (poisonIn doubled) (2, 2, 2)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 614.1a's "instead" taken to zero: half of one, rounded down, is a
  -- replacement that removes the event rather than resizing it. The Rats still
  -- entered and their trigger still resolved, which is what separates this from a
  -- trigger that never ran.
  Spec.it s "CR 107.1a bob's Ichor Rats poisons nobody: half of one is zero" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, erased) <- board True ichorRats S.bob
    (_, plain) <- board False ichorRats S.bob
    Spec.assertBool s (S.onBattlefield rats erased) "the Rats are on the battlefield"
    Spec.assertEqWith s "nobody is poisoned" (poisonIn erased) (0, 0, 0)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 122.6's OBJECT half, on the same axis: alice casts Battlegrowth at bob's
  -- creature, so the counters go on a permanent she does not control and are
  -- doubled anyway. Doubling Season's clause -- which reads whose permanent it is
  -- -- would leave this one alone.
  Spec.it s "CR 122.6 alice doubles a counter she puts on bob's creature" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [vorinclex] [pikerPrinting]
        (bare, bareSpell, _, bareTheirs) = counterBoard forest battlegrowth [] [pikerPrinting]
    case (mine, theirs, bareTheirs) of
      (praetor : _, piker : _, barePiker : _) -> do
        Spec.assertEqWith s "1 * 2" (countersOn CounterKind.PlusOnePlusOne piker (castAndResolve (raceAnswer praetor piker) gs spellId)) 2
        Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne barePiker (castAndResolve (raceAnswer barePiker barePiker) bare bareSpell)) 1
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The object half's other direction, and the pair the subject check on that arm
  -- turns on: BOB casts the Battlegrowth, at his own creature, with alice's
  -- praetor watching. Half of one counter is none. The two boards differ in the
  -- praetor alone.
  Spec.it s "CR 122.6 bob's counter on bob's own creature is halved away" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let bobsBoard withVorinclex =
          let (_, g1) = S.addPermanent forest S.bob (S.landsInPlay forest 1)
              (piker, g2) = S.addPermanent pikerPrinting S.bob g1
              g3 = if withVorinclex then snd (S.addPermanent vorinclex S.alice g2) else g2
              (spell, g4) = S.addHandCard battlegrowth S.bob g3
           in (piker, S.runPure (raceAnswer piker piker) g4 (S.cast S.bob spell >> Stack.resolveTop))
        (halvedPiker, halved) = bobsBoard True
        (plainPiker, plain) = bobsBoard False
    Spec.assertEqWith s "half of one, rounded down" (countersOn CounterKind.PlusOnePlusOne halvedPiker halved) 0
    Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne plainPiker plain) 1

-- CR 120.3b / 120.3d with CR 122.6: the counters a DAMAGE event causes are put on
-- through the same two placement funnels every other counter goes through, so a
-- counter replacement reaches them.
--
-- Ichor Rats ({1}{B}{B} Creature -- Phyrexian Rat 2/1, "Infect. When this
-- creature enters, each player gets a poison counter.") is the source, and its two
-- power is what keeps the three readings apart: two unreplaced, four doubled, one
-- halved. One power would make the halved reading zero, which no board can tell
-- from a placement that never happened.
--
-- The praetor's SEAT is the only difference between each pair, and it is the axis
-- CR 120.3b and CR 120.3d both name: "that source's controller" puts these
-- counters, so alice's praetor doubles them wherever they land, and bob's halves
-- the ones he himself receives. A reading that asked whose permanent or player
-- was AFFECTED would double bob's boards instead.
--
-- Three seats, so the praetor's controller can be a third party to the placement
-- rather than always one of its two ends.
damageCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageCountersSpec s registry = Spec.describe s "Counters damage causes (CR 120.3b, CR 120.3d)" $ do
  let -- alice attacks bob with one Ichor Rats; `watcher` seats that card under that
      -- player, Nothing being the control board. bob gets one creature per printing
      -- in `blockers` and blocks with all of them.
      board watcher blockers = do
        rats <- S.printingOf s registry "Ichor Rats"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        printings <- Monad.mapM (S.printingOf s registry) blockers
        let (gs0, mine, theirs, _) = S.threePlayerCombat [rats] printings []
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addPermanent printing pid gs0)
              _ -> gs0
        pure $ case mine of
          [rat] -> Just (theirs, S.runCombat (fights rat theirs) seated)
          _ -> Nothing
      -- ONLY the Rats attack and only bob's `blockers` block, so a hasty praetor on
      -- either side neither attacks nor blocks and the boards differ in nothing but
      -- the replacement. With one blocker CR 510.1c forces the whole assignment, so
      -- no division is asked for.
      fights :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      fights rat blockers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== rat) ids
        Prompt.DeclareBlockers _ _ mine _ ->
          Map.fromList (fmap (\b -> (b, Set.singleton rat)) (filter (\b -> elem b blockers) mine))
        Prompt.ChooseDefender {} -> S.bob
        _ -> S.identityAnswer p
      poisonOn = S.playerCounterOf PlayerCounterKind.Poison
      shrunk = countersOn CounterKind.MinusOneMinusOne
  Spec.it s "CR 120.3b alice's praetor doubles the poison her Ichor Rats deals bob" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) []
    plain <- board Nothing []
    case (doubled, plain) of
      (Just (_, twice), Just (_, once)) -> do
        Spec.assertEqWith s "twice that many" (poisonOn S.bob twice) 4
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
        Spec.assertEqWith s "CR 120.3b's poison instead of the life loss" (S.lifeOf S.bob twice) (Just 20)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The putter axis: bob's own praetor HALVES the poison bob receives, because
  -- alice is the one putting it. This is the case a recipient-based reading gets
  -- backwards -- bob is its controller's "you", so it would double instead.
  Spec.it s "CR 120.3b bob's praetor halves the poison he receives" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) []
    plain <- board Nothing []
    case (halved, plain) of
      (Just (_, half), Just (_, once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (poisonOn S.bob half) 1
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 120.3d's creature half, on the same axis. Wall of Stone ({1}{R}{R} Creature
  -- -- Wall 0/8, defender) is the blocker: it survives every reading, so the count
  -- is readable rather than gone with the creature, and its zero power assigns no
  -- damage back for CR 704.5h to act on.
  Spec.it s "CR 120.3d alice's praetor doubles the -1/-1 counters her Ichor Rats causes" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (doubled, plain) of
      (Just ([wall], twice), Just ([bare], once)) -> do
        Spec.assertEqWith s "twice that many" (shrunk wall twice) 4
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
        Spec.assertEqWith s "CR 120.3d's counters instead of marked damage" (S.damageOf wall twice) (Just 0)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The creature half's putter axis, and the pair that settles it: the Wall is
  -- BOB's permanent, so a recipient-based reading would double these four times
  -- over. Alice is putting them, so bob's praetor halves them.
  Spec.it s "CR 120.3d bob's praetor halves the counters put on his own Wall of Stone" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (halved, plain) of
      (Just ([wall], half), Just ([bare], once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (shrunk wall half) 1
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 614.16 versus CR 614.1, which is what CounterCause.ByRule settles here: rule
  -- 120.3's results are dictated by the rules, so Doubling Season -- "if an EFFECT
  -- would put one or more counters on a permanent you control" -- does not reach
  -- them, where the praetor's clauses name a player and do. Bob controls the Season
  -- and the Wall, so the only thing keeping it off is the cause.
  --
  -- Not vacuous: the cases above are the same board with the praetor in the same
  -- seat, and they move the count in both directions.
  Spec.it s "CR 614.16 Doubling Season does not reach the counters damage causes" $ do
    seasoned <- board (Just ("Doubling Season", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (seasoned, plain) of
      (Just ([wall], watched), Just ([bare], once)) -> do
        Spec.assertEqWith s "two, not four" (shrunk wall watched) 2
        Spec.assertEqWith s "the same two the bare board shows" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 614.1's third subject, and the case that needs it: Vizier of Remedies
  -- ({1}{W} Creature -- Human Cleric 2/1, "If one or more -1/-1 counters would be
  -- put on a creature you control, that many -1/-1 counters minus one are put on
  -- it instead.") names neither an effect nor a player, so it reaches a placement
  -- rule 120.3d dictates -- which the case above shows CR 614.16's subject does
  -- not.
  --
  -- Both other subjects would leave these alone, so the board separates all three:
  -- the cause is CounterCause.ByRule, which rule 614.16's subject refuses, and the
  -- putter is ALICE (the damage source's controller), where bob controls the
  -- vizier -- so a "if you would put" clause in bob's seat would miss them too.
  --
  -- The vizier neither attacks nor blocks, for the reason `fights` gives, so the
  -- pair of boards differs in the replacement alone.
  Spec.it s "CR 614.1 bob's Vizier of Remedies shrinks the counters damage causes" $ do
    shrinking <- board (Just ("Vizier of Remedies", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (shrinking, plain) of
      (Just ([wall], fewer), Just ([bare], once)) -> do
        Spec.assertEqWith s "that many minus one" (shrunk wall fewer) 1
        Spec.assertEqWith s "and two without the vizier" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"

-- CR 122.6 with CR 701.53a: the counters an EFFECT says a token enters the
-- battlefield with. "To incubate N, create an Incubator token that enters the
-- battlefield with N +1/+1 counters on it" (CR 701.53a) is one of the two
-- wordings in data/cards that write Parun.Types.EntryRiders' `counters` onto an
-- Effect.Create -- Printlifter Ooze's is the other, in printlifterSpec below, and
-- undying and persist write the rider onto a MoveToZone -- and Eyes of Gitaxias
-- ({2}{U} Sorcery, "Incubate 3. Draw a card.") is the producer picked for it: its
-- other sentence asks for no choice, and its THREE is the count that tells the
-- readings apart.
--
-- Six doubled, one halved and three unreplaced are three different numbers, which
-- is what a board with one or two counters could not say.
--
-- The token is CR 111.10i's predefined Incubator token: a colorless Incubator
-- artifact with "{2}: Transform this token", whose back face is a 0/0 colorless
-- Phyrexian artifact creature named Phyrexian Token. The transform case,
-- data/scenarios/entry-replacement/cr-701-53a-the-doubled-counters-are-the-transformed-token-s-praetor.json,
-- is what makes the count matter rather than merely be readable: a 0/0 wearing these
-- counters is the P/T the rest of the game sees.
--
-- Every assertion reads a permanent on the BATTLEFIELD: a token that left it
-- would cease to exist (CR 111.7) and take the assertion with it.
entryCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entryCountersSpec s registry = Spec.describe s "The counters a Create says its token enters with (CR 122.6)" $ do
  let incubatorName = CardName.MkCardName (Text.pack "Incubator Token")
      -- BOTH seats hold five untapped Islands, so `caster` moves who is paying and
      -- nothing else; `watcher` seats one printing under one player, and Nothing is
      -- the control board.
      --
      -- The caster's library is stocked because the sorcery's second sentence draws
      -- (CR 104.3c).
      board caster watcher = do
        island <- S.printingOf s registry "Island"
        eyes <- S.printingOf s registry "Eyes of Gitaxias"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        let bothSeated = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) (S.landsInPlay island 5) [1 .. 5 :: Int]
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addPermanent printing pid bothSeated)
              _ -> bothSeated
            (held, g1) = S.addHandCard eyes caster seated
            (_, g2) = S.addLibraryCard island caster g1
            after = S.runPure S.identityAnswer g2 (S.cast caster held >> Stack.resolveTop)
        pure (newestNamed incubatorName after, after)
      plusOnes = countersOn CounterKind.PlusOnePlusOne
  -- CR 614.16 reaches the placement at all, which is the whole of what the rider
  -- being routed through Event.putCounters buys: without it the token arrives with
  -- the three the effect asked for and no praetor can move them.
  Spec.it s "CR 122.6a alice's praetor doubles the three counters her Incubator token enters with" $ do
    (doubledToken, doubled) <- board S.alice (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.alice Nothing
    case (doubledToken, plainToken) of
      (Just twice, Just once) -> do
        Spec.assertEqWith s "twice that many" (plusOnes twice doubled) 6
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"
  -- CR 122.6a's default putter: "if the effect doesn't specify a player, the
  -- object's controller puts those counters on it", and CR 111.2 makes that bob.
  -- So ALICE's praetor halves them, which is the direction a putter read off the
  -- praetor's own controller gets backwards -- it would double these instead.
  Spec.it s "CR 107.1a alice's praetor halves the counters on bob's Incubator token" $ do
    (halvedToken, halved) <- board S.bob (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.bob Nothing
    case (halvedToken, plainToken) of
      (Just half, Just once) -> do
        Spec.assertEqWith s "half of three, rounded down" (plusOnes half halved) 1
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"

-- CR 614.5's "only one opportunity", at the entry level. Perennation
-- ({3}{W}{B}{G} Sorcery, whole text: "Return target permanent card from your
-- graveyard to the battlefield with a hexproof counter and an indestructible
-- counter on it." -- oracle checked on Scryfall) gives one permanent TWO KINDS
-- of entry counter from ONE effect, which is the board the rule needs:
-- CR 122.6 makes both counters part of the permanent's single entry event even
-- though the rider comes from Perennation rather than the permanent's own text
-- (CR 614.12), so a scaling row whose pattern matches every kind gets ONE
-- opportunity covering both, not one per kind.
--
-- The arithmetic alone cannot say that -- scaling two kinds together and scaling
-- each separately give the same numbers -- so the board makes the NUMBER of
-- opportunities observable instead: two order-sensitive rows, alice's Doubling
-- Season and bob's Vorinclex (CR 616.1e orders them), answered so that the first
-- order taken and any second order taken would disagree. One opportunity means
-- one order asked and both kinds on the side it chose; an opportunity per kind
-- would ask twice and let the kinds disagree.
--
-- Odd counts throughout: Replacement.scale rounds Halve down, so 1 doubled then
-- halved is 1 where 1 halved then doubled is 0. An even count would read the
-- same under either order and prove nothing.
--
-- The returned card is read by NAME: CR 400.7 mints a new object as it leaves
-- the graveyard, so the id the fixture buried names nothing on the battlefield.
perennationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
perennationSpec s registry = Spec.describe s "Perennation (CR 614.5)" $ do
  let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      hexproofs = countersOn (CounterKind.Keyword (Keyword.Hexproof Nothing))
      indestructibles = countersOn (CounterKind.Keyword Keyword.Indestructible)
      -- Six untapped lands in three colours pay the {3}{W}{B}{G}; the Piker in
      -- alice's graveyard is the permanent card the sorcery targets, and the only
      -- card there, so the identity answer cannot aim it anywhere else.
      --
      -- `scalers` is the only difference between the two boards: alice's Doubling
      -- Season ("counters would be put on a permanent YOU control") and bob's
      -- Vorinclex ("if an OPPONENT would put") both reach alice's placement, and
      -- CR 122.6a's default putter -- the entering permanent's controller -- is
      -- what makes the praetor's second clause the one that applies.
      board scalers = do
        plains <- S.printingOf s registry "Plains"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        perennation <- S.printingOf s registry "Perennation"
        doublingSeason <- S.printingOf s registry "Doubling Season"
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let bare = S.landsFor forest S.alice 2 (S.landsFor swamp S.alice 2 (S.landsInPlay plains 2))
            (seasonId, withSeason) = S.addPermanent doublingSeason S.alice bare
            (_, withPraetor) = S.addPermanent vorinclex S.bob withSeason
            (_, buried) = S.addGraveyardCard piker S.alice (if scalers then withPraetor else bare)
            (held, ready) = S.addHandCard perennation S.alice buried
        pure (seasonId, held, ready)
      -- Cast and resolve, answering the entry's CR 616.1 orders with
      -- `ordersEntry` and counting them.
      returnIt seasonFirst (seasonId, held, ready) =
        let ((_, after), asked) = State.runState (Engine.runGame (ordersEntry seasonFirst seasonId) ready (S.cast S.alice held >> Stack.resolveTop)) 0
         in (asked, newestNamed pikerName after, after)
  -- The control, and the setup every case below rests on: one effect, two kinds,
  -- one counter each, and nothing on the board that could scale either.
  Spec.it s "CR 122.6 the returned permanent enters with one counter of each kind" $ do
    built <- board False
    case returnIt True built of
      (asked, Just oid, after) -> do
        Spec.assertEqWith s "a hexproof counter and an indestructible counter" (hexproofs oid after, indestructibles oid after) (1, 1)
        Spec.assertEqWith s "and with no row in the CR 616.1 pool there was nothing to order" asked 0
      _ -> Spec.assertFailure s "the card did not return to the battlefield"

-- CR 614.5 again, on the permanent's OWN text rather than on a rider an effect
-- supplied. CR 614.1c's "as this permanent enters" clause can name SEVERAL KINDS
-- of counter in one sentence, and it is then one row, one candidate in CR 616.1's
-- pool, and one opportunity for a scaling replacement across every kind (#2314).
-- A kind per row would put an ordering in that pool the sentence does not have.
--
-- Agent's Toolkit {1}{G}{U} Artifact - Clue (New Capenna Commander; name, cost,
-- type line and oracle text checked against Scryfall 2026-08-25), whose entry
-- line is "this artifact enters with a +1/+1 counter, a flying counter, a
-- deathtouch counter, and a shield counter on it". Its other two lines are
-- Parun.MoveCounterSpec's -- CR 122.5's move, and the Clue line its sacrifice
-- case spends.
--
-- Scryfall `o:/enters with .*counter.? and a .*counter/ -is:digital`,
-- 2026-08-25, answers three printings and no more: this one, Voidpouncer and
-- Dust Animus. The other two condition the clause, which is CR 604.2's clause on
-- Parun.Types.PrintedReplacement rather than anything an EntryRewrite carries --
-- and both are in the pool too, Dust Animus at dustAnimusSpec and Voidpouncer at
-- data/scenarios/entry-replacement. This one is the unconditional member of the three.
--
-- THE BOARD IS PERENNATION'S, and for its reasons: alice's Doubling Season and
-- bob's Vorinclex are order-sensitive against each other (CR 616.1e), the counts
-- are odd so Replacement.scale's rounding makes the two orders disagree, and
-- `ordersEntry` answers the first order one way and every later order the other,
-- so a second opportunity would move the kinds apart. What differs is where the
-- counters come from: here the entering permanent's own text (CR 614.1c), which
-- is the half the multi-kind row is for.
toolkitSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
toolkitSpec s registry = Spec.describe s "Agent's Toolkit (CR 614.1c)" $ do
  let toolkitName = CardName.MkCardName (Text.pack "Agent's Toolkit")
      -- All four kinds the row names, read off the permanent that entered.
      kindsOn oid gs =
        ( countersOn CounterKind.PlusOnePlusOne oid gs,
          countersOn (CounterKind.Keyword Keyword.Deathtouch) oid gs,
          countersOn (CounterKind.Keyword Keyword.Flying) oid gs,
          countersOn CounterKind.Shield oid gs
        )
      -- Three untapped lands pay the {1}{G}{U}. `scalers` is the only difference
      -- between the two boards, exactly as in perennationSpec.
      board scalers = do
        plains <- S.printingOf s registry "Plains"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        toolkit <- S.printingOf s registry "Agent's Toolkit"
        doublingSeason <- S.printingOf s registry "Doubling Season"
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let bare = S.landsFor forest S.alice 1 (S.landsFor island S.alice 1 (S.landsInPlay plains 1))
            (seasonId, withSeason) = S.addPermanent doublingSeason S.alice bare
            (_, withPraetor) = S.addPermanent vorinclex S.bob withSeason
            (held, ready) = S.addHandCard toolkit S.alice (if scalers then withPraetor else bare)
        pure (seasonId, held, ready)
      castIt seasonFirst (seasonId, held, ready) =
        let ((_, after), asked) = State.runState (Engine.runGame (toolkitOrders seasonFirst seasonId) ready (S.cast S.alice held >> Stack.resolveTop)) 0
         in (asked, newestNamed toolkitName after, after)
  -- The control: one counter of each of the four kinds, and nothing to order.
  Spec.it s "CR 614.1c the artifact enters with one counter of each kind it names" $ do
    built <- board False
    case castIt True built of
      (asked, Just oid, after) -> do
        Spec.assertEqWith s "a +1/+1, a deathtouch, a flying and a shield counter" (kindsOn oid after) (1, 1, 1, 1)
        Spec.assertEqWith s "and with no scaling row in the CR 616.1 pool there was nothing to order" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"

-- ordersEntry's answerer, and TOTAL where that one errors: a row per kind offers
-- pools this one's `wantSeason` is not in, so an error there would preempt the
-- counter assertions and report itself instead of the behaviour. It falls back to
-- the first row offered, which leaves the counts free to disagree and the count of
-- orders free to grow -- both of which the cases below read.
toolkitOrders :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
toolkitOrders seasonFirst seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries -> do
    asked <- State.get
    State.put (asked + 1)
    let wantSeason = if asked <= (0 :: Int) then seasonFirst else not seasonFirst
    pure (maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantSeason) . (== seasonId) . ReplacementEntry.source) entries))
  -- Unreachable in a green run: the prompt fires exactly once per `castIt`
  -- call, on a pool where `wantSeason` matches one of the two rows offered
  -- (Doubling Season's and Vorinclex's), so `findIndex` always answers `Just`.
  -- Kept rather than `error`, per toolkitSpec's own comment above: a wrong
  -- fallback here would fail the counter assertions in a readable way instead
  -- of an opaque one.
  _ -> pure (S.identityAnswer p)

-- Answer an entry's CR 616.1 orders and COUNT them. The first order taken is
-- Doubling Season's row when `seasonFirst`, and every later order taken is the
-- other row -- deliberately inconsistent, so that a second order, if the engine
-- ever asked for one, would move the two kinds of counter apart. Stateful rather
-- than a pure Prompt r -> r for that reason: the two orders are structurally
-- identical, and a pure answerer would answer them the same way and see nothing.
ordersEntry :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
ordersEntry seasonFirst seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries -> do
    asked <- State.get
    State.put (asked + 1)
    let wantSeason = if asked <= (0 :: Int) then seasonFirst else not seasonFirst
    pure
      ( maybe
          (error "Parun.EntryReplacementSpec.ordersEntry: no matching row offered")
          Int.toNaturalSaturating
          (List.findIndex ((== wantSeason) . (== seasonId) . ReplacementEntry.source) entries)
      )
  _ -> pure (S.identityAnswer p)

-- CR 614.1c's counter row with a CARD-AUTHORED condition on it, which is CR
-- 604.2's clause on Parun.Types.PrintedReplacement -- the same clause Monstrous
-- War-Leech's "if it was kicked" rides above, over a WithCounters rewrite
-- instead of a RunEffects one (see #2317). Nothing on Parun.Types.EntryRewrite says
-- when the row applies: rule 702.54a's bloodthirst and rule 702.150a's
-- compleated ask their conditions in Parun.Engine.Replacement.admitsEntry
-- because those conditions are RULES', with nowhere on a card to live, and this
-- one is the card's.
--
-- Dust Animus {1}{W} Creature -- Spirit 2/3, whole text: "Flying. If you control
-- five or more untapped lands, this creature enters with two +1/+1 counters and
-- a lifelink counter on it. Plot {1}{W}." (oracle checked on Scryfall
-- 2026-08-25)
--
-- THE TWO BOARDS DIFFER IN ONE TAP. Both give alice seven Plains and the Animus
-- in hand; the negative board taps one Plains first. Casting spends {1}{W}, so
-- five lands are untapped as the Animus enters on the first board and four on
-- the second -- CR 614.12 checks the condition against the board the permanent
-- would enter on, which is the board AFTER the cast's own lands were tapped.
-- Same land count, same mana available, same spell: only the tap differs, so a
-- negative that passed because the spell was uncastable is ruled out.
--
-- Seven and five and two and one are distinct, and the readout is doubled: the
-- counters themselves, and the CR 613.4c power and toughness they move in
-- layer 613.1g (4/5 against the printed 2/3).
dustAnimusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dustAnimusSpec s registry = Spec.describe s "Dust Animus (CR 614.1c)" $ do
  let animusName = CardName.MkCardName (Text.pack "Dust Animus")
      countsOn oid gs =
        ( countersOn CounterKind.PlusOnePlusOne oid gs,
          countersOn (CounterKind.Keyword Keyword.Lifelink) oid gs
        )
      board tapOne = do
        plains <- S.printingOf s registry "Plains"
        animus <- S.printingOf s registry "Dust Animus"
        let lands = S.landsInPlay plains 7
            oneLand = List.sort (Set.toList (GameState.battlefield lands))
            tapped = case oneLand of
              landId : _ | tapOne -> S.tapObject landId lands
              _ -> lands
            (held, ready) = S.addHandCard animus S.alice tapped
        pure (held, ready)
      castIt (held, ready) =
        let after = S.runPure S.identityAnswer ready (S.cast S.alice held >> Stack.resolveTop)
         in (newestNamed animusName after, after)
  Spec.it s "CR 614.1c five untapped lands as it enters, so the row applies: two +1/+1 counters and a lifelink counter" $ do
    built <- board False
    case castIt built of
      (Just oid, after) -> do
        Spec.assertEqWith s "two +1/+1 counters and one lifelink counter" (countsOn oid after) (2, 1)
        Spec.assertEqWith s "so the printed 2/3 is a 4/5" (S.powerToughnessOf oid after) (Just (4, 5))
        Spec.assertEqWith s "and the cast tapped two of the seven Plains, leaving five untapped" (S.tappedCount S.alice after) 2
      _ -> Spec.assertFailure s "the Spirit did not reach the battlefield"
  -- The same board and the same seven lands, one of them already tapped. The
  -- Animus ENTERS HERE TOO, so what the pair tells apart is whether the row
  -- applied and not whether the permanent arrived.
  Spec.it s "CR 614.1c only four untapped lands as it enters, so the row does not apply: no counters at all" $ do
    built <- board True
    case castIt built of
      (Just oid, after) -> do
        Spec.assertEqWith s "neither kind of counter arrives" (countsOn oid after) (0, 0)
        Spec.assertEqWith s "so it is the printed 2/3" (S.powerToughnessOf oid after) (Just (2, 3))
        Spec.assertEqWith s "and three of the seven Plains are tapped, leaving four untapped" (S.tappedCount S.alice after) 3
      _ -> Spec.assertFailure s "the Spirit did not reach the battlefield"

-- Synthetic Magnetic Lockdown {2}{W} Instant: "Until end of turn, if you control
-- three or more artifacts, artifacts enter the battlefield tapped." The FLOATING
-- twin of Frontier Mastodon's (data/scenarios/entry-replacement): the same CR
-- 614.12a board question, asked of a row CR 614.3 installed rather than of a
-- permanent's static ability.
--
-- SYNTHETIC because the shape has no printing. Both halves are printed, and by
-- different cards -- Kismet writes the EntryR/Tapped rewrite, Galvanic Blast's
-- metalcraft writes the "if you control three or more artifacts" clause -- and
-- what nothing prints is a DURATION-bounded row carrying such a clause. Scryfall
-- with a User-Agent, 2026-08-27: `(t:instant or t:sorcery) o:"would enter the
-- battlefield" o:instead` answers one card, Gather Specimens, which prints no
-- "if"; `o:"instead if you control"` answers fourteen, every one a one-shot
-- instead settled inside its own resolution (Galvanic Blast, Crater's Claws,
-- Mirran Mettle, ...); `o:"until end of turn" o:instead o:"if you control"`
-- answers nineteen modal or pump-vs-pump spells; `o:enters o:"this turn"
-- (t:instant or t:sorcery)` answers seventeen, of which Gather Specimens alone
-- installs an entry REPLACEMENT -- the near miss, Theoretical Duplication, prints
-- "whenever a nontoken creature an opponent controls enters this turn, create a
-- token that's a copy of that creature", which is a delayed trigger and replaces
-- nothing. A printing of any of those families with a stated duration AND a
-- printed "if" would refute this and replace the synthetic. The same enumeration
-- inside data/cards: only Galvanic Blast and Synthetic Voltaic Surge write a
-- condition on an Effect.Replace at all, and both gate a DamageR.
--
-- THE PAIR differs in exactly one permanent, and the entering artifact is what
-- either reading disagrees about. alice controls two Bonesplitters, casts the
-- Lockdown, then casts a Conjurer's Bauble: as the Bauble's entry loop runs she
-- controls TWO artifacts under CR 614.12a and THREE under a reading that counts
-- the permanent mid-entry, so the clause is false one way and true the other and
-- the Bauble enters untapped or tapped accordingly. The positive board is the
-- same board with a third Bonesplitter, where the clause is true either way --
-- so the negative is not the row being off, missing, or expired.
--
-- Conjurer's Bauble rather than a third Bonesplitter: distinct names make
-- `newestNamed` name the entering permanent rather than trust an id order, and
-- {1} keeps the mana apart from the Lockdown's {2}{W} out of four Plains.
magneticLockdownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magneticLockdownSpec s registry = Spec.describe s "Synthetic Magnetic Lockdown (CR 614.12a)" $ do
  let baubleName = CardName.MkCardName (Text.pack "Conjurer's Bauble")
      splitterName = CardName.MkCardName (Text.pack "Bonesplitter")
      board owned = do
        plains <- S.printingOf s registry "Plains"
        splitter <- S.printingOf s registry "Bonesplitter"
        lockdown <- S.printingOf s registry "Synthetic Magnetic Lockdown"
        bauble <- S.printingOf s registry "Conjurer's Bauble"
        let lands = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
            stocked = List.foldl' (\g _ -> snd (S.addPermanent splitter S.alice g)) lands [1 .. owned]
            (lockdownId, g1) = S.addHandCard lockdown S.alice stocked
            (baubleId, g2) = S.addHandCard bauble S.alice g1
            ready =
              g2
                { GameState.turn =
                    (GameState.turn g2)
                      { TurnRegisters.phase = Phase.PrecombatMain,
                        TurnRegisters.activePlayer = S.alice,
                        TurnRegisters.priority = Just S.alice
                      }
                }
            armed = castAndResolve S.identityAnswer ready lockdownId
        pure (armed, castAndResolve S.identityAnswer armed baubleId)
  -- THE PROVING CASE.
  Spec.it s "CR 614.12a the artifact mid-entry is not one of the three its own row counts, so it enters untapped" $ do
    (armed, after) <- board (2 :: Int)
    case newestNamed baubleName after of
      Just oid -> do
        Spec.assertEqWith s "the Bauble entered untapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Untapped)
        Spec.assertEqWith s "setup: alice controlled two artifacts as it entered" (controlledNamed splitterName S.alice after) 2
        Spec.assertEqWith s "setup: the row was installed and is still installed" (length (StoredEffects.replacements (GameState.stored armed)), length (StoredEffects.replacements (GameState.stored after))) (1, 1)
      _ -> Spec.assertFailure s "the Bauble did not reach the battlefield"
  Spec.it s "CR 614.12a a third artifact already on the battlefield does count, so the same row taps it" $ do
    (_, after) <- board (3 :: Int)
    case newestNamed baubleName after of
      Just oid -> do
        Spec.assertEqWith s "the Bauble entered tapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Tapped)
        Spec.assertEqWith s "setup: alice controlled three artifacts as it entered" (controlledNamed splitterName S.alice after) 3
      _ -> Spec.assertFailure s "the Bauble did not reach the battlefield"

-- CR 122.6 with CR 107.3c: an entry rider whose COUNT is not a number. Printlifter
-- Ooze -- {1}{G} 2/2 Creature -- Ooze with deathtouch and disguise {3}{G} --
-- prints "whenever this creature or another creature you control is turned face
-- up, create a 0/0 green Ooze creature token with trample. The token enters with
-- X +1/+1 counters on it, where X is the number of other creatures you control",
-- and is the printing that puts the count INSIDE the rider: Scryfall
-- `o:/token enters with/ -is:digital`, 2026-08-22, answers this card and Ochre
-- Jelly, whose split token is an Effect.CreateCopy -- the OTHER opcode that now
-- carries the same rider (Parun.CopySpec, Littjara Mirrorlake). The other
-- "X +1/+1 counters on it, where X" printings say it in a SECOND SENTENCE, which
-- is an Effect.PutCounters after the Create and needs no rider at all -- CR 704.3
-- is why the 0/0 survives in between.
--
-- X is defined by the ability's own text (CR 107.3c) and answered once, when the
-- effect is applied (CR 608.2h) -- Parun.Engine.Resolve.Common.freezeRiders, against the
-- resolution's own context, which is what makes Filter.IsSource and CR 109.5's
-- "you" live on this path.
--
-- THE BOARD SEPARATES FIVE READINGS. alice controls Printlifter Ooze, an Ainok
-- Tracker cast face down for CR 702.37a's {3}, and two Goblin Pikers; bob
-- controls two Goblin Pikers of his own. Turning the Ainok face up (CR 702.37e,
-- paying {4}{R} out of the eight Mountains) fires the trigger, and X is THREE --
-- the other three creatures alice controls. A reading that counts the source too
-- says four, as does one that counts after the token arrived; one that drops CR
-- 109.5's "you" says five; one that reads only the permanent that turned over
-- says one; and a rider dropped entirely says zero, which CR 704.5f then puts in
-- the graveyard before anything can read it.
--
-- Two rather than one Goblin Piker per seat so that those five are five different
-- numbers, and a face-down permanent is CR 708.2a's 2/2, so three is not a
-- toughness on the board either.
printlifterSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
printlifterSpec s registry = Spec.describe s "The counters a Create says its token enters with (CR 122.6)" $ do
  let oozeTokenName = CardName.MkCardName (Text.pack "Ooze Token")
      -- The permanent a move added to the battlefield, or Nothing when it added
      -- none or several. Identifies the new incarnation without asking what card
      -- is under it, which is the whole point of a face-down one.
      enteredOne gs after = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield gs)) of
        [oid] -> Just oid
        _ -> Nothing
      -- Eight Mountains: three for CR 702.37a's morph cast and five for CR
      -- 702.37e's {4}{R} turn-up cost. Answers the settled board and the
      -- face-down permanent, or Nothing if the morph cast did not land.
      board = do
        mountain <- S.printingOf s registry "Mountain"
        ooze <- S.printingOf s registry "Printlifter Ooze"
        ainok <- S.printingOf s registry "Ainok Tracker"
        piker <- S.printingOf s registry "Goblin Piker"
        let (handed, morphCard) = S.handOne ainok (S.landsInPlay mountain 8)
            seat pid gs = snd (S.addPermanent piker pid gs)
            seated = seat S.bob (seat S.bob (seat S.alice (seat S.alice (snd (S.addPermanent ooze S.alice handed)))))
            after = S.runPure S.identityAnswer seated (Cast.castSpell S.manaPerformer S.alice morphCard (S.printingName ainok) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
        pure (fmap (\permanent -> (after, permanent)) (enteredOne seated after))
  Spec.it s "CR 122.6 the token enters with one +1/+1 counter per OTHER creature alice controls" $ do
    made <- board
    case made of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (settled, morphling) -> do
        -- The controls: the action really is on offer beforehand, so a board with
        -- no token below is the rider and not a permanent that never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice settled) [(morphling, TurnUpProcedure.Morph)]
        let after = S.runPure S.identityAnswer settled (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Morph morphling >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 110.5 it turned face up" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceUp)
        case newestNamed oozeTokenName after of
          Nothing -> Spec.assertFailure s "CR 111.1 the Ooze token did not reach the battlefield"
          Just token -> do
            -- THE DISCRIMINATING ASSERTION. Three: the Ainok, and alice's two
            -- Goblin Pikers. Not the Ooze itself (CR 122.6 through the ability's
            -- own Filter.IsSource), not bob's two, and not the token.
            Spec.assertEqWith s "CR 122.6 three +1/+1 counters, one per other creature alice controls" (countersOn CounterKind.PlusOnePlusOne token after) 3
            -- What the count is FOR: the token is printed 0/0, so CR 613.4c's
            -- layer 7c leaves a 3/3 rather than something CR 704.5f removes.
            Spec.assertEqWith s "CR 613.4c so the 0/0 token is a 3/3" (S.powerToughnessOf token after) (Just (3, 3))

-- Zameck Guildmage {G}{U} Creature -- Elf Wizard 2/2, whole text: "{G}{U}: This
-- turn, each creature you control enters with an additional +1/+1 counter on it.
-- / {G}{U}, Remove a +1\/+1 counter from a creature you control: Draw a card."
-- (oracle checked on Scryfall 2026-09-05)
--
-- CR 614.3's FLOATING row carrying CR 614.1c's counter rewrite, aimed at other
-- permanents by Filter: Gather Specimens' shape with the rewrite swapped from
-- EntryRewrite.UnderControlOf to EntryRewrite.WithCounters. Every other
-- counter row in data/cards rides the entering permanent's own static ability
-- with Filter.IsSource -- Barkhide Troll's, the counter half of Faerie Squadron's
-- EntryRewrite.EntersWith, and the ones Parun.Engine.Keyword mints for vanishing,
-- fading and modular -- so this is the
-- first row that reads the counter count for a permanent OTHER than the one the
-- row came from.
--
-- The second ability is transcribed too, on
-- CostComponent.RemoveCounters, and is Parun.CostSpec's Zameck Guildmage
-- group rather than this one's: nothing here activates it, so the boards below
-- are unchanged by it.
--
-- THE BOARD: alice's Guildmage plus a Forest, an Island, a Mountain and a Plains
-- -- the ability's {G}{U} and the Goblin Piker's {1}{R} with nothing over -- and
-- bob holds a Piker of his own over a Mountain and a Plains. The three cases
-- below are the SAME board and differ in one action each: whether the ability
-- resolved before the Piker was cast, and whose Piker it was.
zameckGuildmageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zameckGuildmageSpec s registry = Spec.describe s "Zameck Guildmage (CR 614.3)" $ do
  let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      board = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        guildmage <- S.printingOf s registry "Zameck Guildmage"
        piker <- S.printingOf s registry "Goblin Piker"
        let alicesLands = S.landsFor plains S.alice 1 (S.landsFor mountain S.alice 1 (S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))))
            lands = S.landsFor plains S.bob 1 (S.landsFor mountain S.bob 1 alicesLands)
            (mageId, withMage) = S.addPermanent guildmage S.alice lands
            (alicesPiker, withAlices) = S.addHandCard piker S.alice withMage
            (bobsPiker, withBobs) = S.addHandCard piker S.bob withAlices
            -- Stocked so CR 104.3c does not decide the last case before its
            -- assertion runs: it advances two whole turns, and both players draw.
            stock g _ = snd (S.addLibraryCard plains S.alice (snd (S.addLibraryCard plains S.bob g)))
            stocked = List.foldl' stock withBobs [1 .. (4 :: Int)]
            ready =
              stocked
                { GameState.turn =
                    (GameState.turn stocked)
                      { TurnRegisters.phase = Phase.PrecombatMain,
                        TurnRegisters.activePlayer = S.alice,
                        TurnRegisters.priority = Just S.alice
                      }
                }
        pure (theAbility guildmage, mageId, alicesPiker, bobsPiker, ready)
      -- Activate the first ability and resolve it, which is what installs the row.
      arm (ability, mageId, _, _, ready) =
        S.runPure S.identityAnswer ready (Activate.activateAbility S.alice mageId ability >> Stack.resolveTop)
      castFor pid gs pikerId = S.runPure S.identityAnswer gs (S.cast pid pikerId >> Stack.resolveTop)
  -- THE PROVING CASE. The Piker is printed 2/1 and enters 3/2, the counter being
  -- the observable and the power/toughness what it is for (CR 613.4c layer 7c).
  Spec.it s "CR 614.1c a creature entering after the ability resolved enters with the additional counter" $ do
    built@(_, mageId, alicesPiker, _, _) <- board
    let after = castFor S.alice (arm built) alicesPiker
    case newestNamed pikerName after of
      Just oid -> do
        Spec.assertEqWith s "one +1/+1 counter on the Piker that entered" (countersOn CounterKind.PlusOnePlusOne oid after) 1
        Spec.assertEqWith s "so the printed 2/1 is a 3/2" (S.powerToughnessOf oid after) (Just (3, 2))
        -- CR 614.1's row replaces an ENTRY, so the permanent that was already on
        -- the battlefield when the ability resolved gets nothing.
        Spec.assertEqWith s "and the Guildmage, already on the battlefield, has none" (countersOn CounterKind.PlusOnePlusOne mageId after) 0
      _ -> Spec.assertFailure s "the Piker did not reach the battlefield"
  -- CR 109.5 on an ACTIVATED ability: "you" is the player who activated it, so
  -- the row's Filter.HeldBy Holder.Controller passes over bob's creature. Without this case
  -- a row matching every creature would look identical to the printed one.
  Spec.it s "CR 109.5 the opponent's creature is not one alice controls, so the row passes over it" $ do
    built@(_, _, _, bobsPiker, _) <- board
    let after = castFor S.bob (arm built) bobsPiker
    case newestNamed pikerName after of
      Just oid -> do
        Spec.assertEqWith s "no +1/+1 counter on bob's Piker" (countersOn CounterKind.PlusOnePlusOne oid after) 0
        Spec.assertEqWith s "so it is the printed 2/1" (S.powerToughnessOf oid after) (Just (2, 1))
      _ -> Spec.assertFailure s "bob's Piker did not reach the battlefield"

-- CR 702.38: amplify, on Feral Throwback ({4}{G}{G} 3/3 Beast, "Amplify 2" and
-- provoke). Its gameplay cases are scenarios under data/scenarios/entry-replacement.
amplifySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
amplifySpec s _ = Spec.describe s "Amplify (CR 702.38)" $ do
  -- CR 702.38b: "if a creature has multiple instances of amplify, each one
  -- works separately." Asserted at the mint rather than at gameplay level,
  -- because nothing in the pool prints or grants a second instance -- the
  -- footing the bloodthirst group's rule 702.54c case states.
  Spec.it s "CR 702.38b each instance is its own row" $
    Spec.assertEqWith
      s
      "amplify 2 held twice mints two rows"
      (Keyword.Engine.mintedReplacementsFor (Keyword.Amplify 2) 2)
      (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.Amplify 2))))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Replacement" $ do
  riotSpec s registry
  bloodthirstSpec s registry
  amplifySpec s registry
  sunburstSpec s registry
  stuffyDollSpec s registry
  vorinclexSpec s registry
  damageCountersSpec s registry
  entryCountersSpec s registry
  perennationSpec s registry
  toolkitSpec s registry
  dustAnimusSpec s registry
  magneticLockdownSpec s registry
  printlifterSpec s registry
  zameckGuildmageSpec s registry
