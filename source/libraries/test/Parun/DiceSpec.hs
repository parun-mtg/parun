{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 706 ROLLING A DIE -- Parun.Types.RollDie, Effect.RollDie's arm in
-- Parun.Engine.Resolve, and the Parun.Types.Prompt / Parun.Types.Response pair the
-- roll is externalised through. The transcript legs live in Parun.ReplaySpec
-- with the other randomness prompts.
--
-- THE FIXTURES. Ancient Copper Dragon ("Flying /
-- Whenever this creature deals combat damage to a player, roll a d20. You create
-- a number of Treasure tokens equal to the result") is CR 706.4's, the result
-- read straight into a count; Djinni Windseer ("Flying / When this creature
-- enters, roll a d20. / 1-9 | Scry 1. / 10-19 | Scry 2. / 20 | Scry 3.") is CR
-- 706.3's results table, where the result selects an effect instead; Diviner's
-- Portent, CR 706.2's modifier printed in the instruction, is proved in
-- data/scenarios/dice. Valiant Endeavor ("Roll two d6 and
-- choose one result. Destroy each creature with power greater than or equal to
-- that result. Then create a number of 2/2 white Knight creature tokens with
-- vigilance equal to the other result") is the fourth fixture and CR 706.1's
-- count, CR 706.4's choice among the results and the "other result" beside it.
-- Neverwinter Hydra, after it, reads CR 706.4's total of the results instead.
-- CR 614.1a's replacement over the roll (Pixie Guide, with CR 706.6's ignore)
-- and CR 706.2b's first step (Clam-I-Am's reroll) are proved in
-- data/scenarios/dice. CR 706.2a's cost on such a modifier is the fifth
-- fixture, Wall of Fortune -- the only printing whose reroll charges anything
-- and the only one reaching a roll its own controller did not make.
-- CR 706.2b's second step is the EIGHTH, Night Shift of the Living Dead, at the
-- very bottom -- a modifier from another source that increases or decreases the
-- result, with a life cost and a once-each-turn budget.
-- CR 706.3c's "Roll again" is the NINTH, Delina, Wild Mage, at the end of
-- the file -- the roll inside Effect.Repeat, asked only on the 15--20 striation.
-- CR 706.5's doubles, two results each read on their own and compared, is
-- Celebr-8000's, in data\/scenarios\/dice\/. CR 706.1's roll does record its
-- event, but the trigger reading it lives in Parun.EventTriggerSpec beside the
-- other condition cases.
--
-- THE ASSERTED QUANTITY on the DRAGON's boards is how many Treasure tokens alice
-- controls once combat damage has been dealt. It is the roll's result made
-- visible: nothing else on this board mints a token, bob controls nothing, and
-- the Treasure's own mana ability is never activated. Tokens enter under their
-- creator's control (CR 111.2) and alice is the only creator, so there is no
-- second trace to the same number.
--
-- FOUR BOARDS differing in ONE thing -- the number the interpreter answers with:
-- 13, 7, 20 and 21. 13 is the primary pin because it is none of the values a
-- wrong implementation could produce by accident: not 1 (Replay.defaultAnswer,
-- which S.identityAnswer falls through to), not 20 (the die's size), not 6 or 5
-- (the dragon's power and toughness) and not 0. The 7 leg falsifies a hard-coded
-- 13. The 20/21 pair straddles CR 706.1a's upper bound, which is the only place
-- an off-by-one on the face count is visible: 20 IS an outcome of a d20, and 21
-- is not.
--
-- TWO SEATS, not three: the roll's result is read by a Create scoped to "you",
-- so no clause here ranges over opponents, and a third seat would only add a
-- defending-player question. Bob controls nothing, because a blocker would keep
-- the combat damage off him and the trigger would never fire at all.
--
-- THE ASSERTED QUANTITY on the WINDSEER's boards is the identity of the top card
-- of alice's library after the trigger resolves, over a library of six cards
-- interned from six different printings. Every scry prompt is answered by
-- bottoming the whole look, so scry N moves exactly N cards off the top and the
-- top card names N: card 2 for scry 1, card 3 for scry 2, card 4 for scry 3, and
-- card 1 for a table that fired nothing at all. GameEvent.PlayerActed carries a
-- PlayerId and no count, so library order is the only gameplay-level quantity
-- that can tell the striations apart.
--
-- SIX CARDS rather than four so that a table firing two striations at once is
-- distinguishable rather than clamped by a short library: dropping the 10-19
-- band's upper endpoint makes a roll of 20 scry 2 AND scry 3, which is five cards
-- bottomed and card 6 on top.
--
module Parun.DiceSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Resolve" $ do
  resultsTableSpec s registry
  costedRerollSpec s registry
  activatedRerollSpec s registry
  nightShiftSpec s registry
  delinaSpec s registry

knight :: CardName.CardName
knight = CardName.MkCardName (Text.pack "Knight Token")

-- Djinni Windseer entering under her and its CR 603.6a trigger pending. The
-- printings differ so that no two library positions can be confused for each
-- other; the returned ids are the library reading top-first.
--
-- addLibraryCard puts its card ON TOP, so the deck is dealt deepest-first.
tableBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m ([ObjectId.ObjectId], GameState.GameState)
tableBoard s registry = do
  djinni <- S.printingOf s registry "Djinni Windseer"
  deck <- traverse (S.printingOf s registry) ["Goblin Piker", "Bird Maiden", "Mountain", "Forest", "Island", "Plains"]
  let deal (acc, gs) printing = let (oid, gs2) = S.addLibraryCard printing S.alice gs in (oid : acc, gs2)
      (ids, stocked) = List.foldl' deal ([], Setup.emptyGame S.bothPlayers) (reverse deck)
      (_, entered) = S.entersWithTrigger djinni S.alice stocked
  pure (ids, entered)

-- Pins the d20 to `n` and BOTTOMS the whole look of every scry, so the number of
-- cards that leave the top is the scry's count and nothing else. Bottoming the
-- offered list is not a search for a legal answer -- it names whatever it was
-- shown -- so a mutation that changed which striation fired changes the library
-- rather than being repaired by the answerer.
tableAnswer :: Natural.Natural -> Prompt.Prompt r -> r
tableAnswer n p = case p of
  Prompt.RollDie _ -> n
  Prompt.ChooseScry _ _ looked -> (looked, [])
  _ -> S.identityAnswer p

-- The pending enters trigger, put on the stack and not yet resolved.
placeTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
placeTable n board = S.runPure (tableAnswer n) board (Engine.placePendingTriggers [])

-- And resolved, under the same answerer, so the roll and the scry it selects are
-- the same run.
runTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
runTable n board = S.runPure (tableAnswer n) (placeTable n board) Stack.resolveTop

-- Alice's library, top-first.
tableLibrary :: GameState.GameState -> [ObjectId.ObjectId]
tableLibrary = Game.zoneMembers Zone.Library S.alice

resultsTableSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resultsTableSpec s registry = Spec.describe s "ResultsTable" $ do
  -- CR 706.3a's three striation forms on one card, read through the one thing
  -- that separates them: how many cards a scry took off the top. Card 1 on top
  -- is the vacuous board -- the table fired nothing -- and it is a DIFFERENT
  -- value from all three, so "the gate never held" cannot pass as any leg.
  Spec.it s "CR 706.3a the result selects one striation of the results table" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, fourth, _, _] -> do
        -- 10-19, the closed range in the middle: scry 2, so the third card is on
        -- top. First because the middle band is the one a half-open reading of
        -- CR 706.3a gets wrong in both directions.
        Spec.assertEqWith s "CR 706.3a: a roll of 10 is in 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 10 board))) (Just third)
        -- 1-9, the other closed range: scry 1, so the second card is on top.
        Spec.assertEqWith s "CR 706.3a: a roll of 9 is in 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 9 board))) (Just second)
        -- The single number: scry 3, so the fourth card is on top. A REGRESSION
        -- FENCE on this striation's own shape rather than a proof of it: 20 is a
        -- d20's top face, so a `20+` reading agrees with the printed `20` on
        -- every outcome, and swapping the card's Exactly for an AtLeast leaves
        -- this assertion green. Windseer's own instruction prints no modifier;
        -- the IncreaseOrDecrease group's "a result shifted past the die's top
        -- face" case proves the exact form under Night Shift of the Living
        -- Dead's 21, and the single-endpoint form is proved on Diviner's Portent
        -- in data/scenarios/dice. What the assertion DOES prove is that 20 selects this striation
        -- and not the 10-19 band above it.
        Spec.assertEqWith s "CR 706.3a: a roll of 20 is the 20 striation, so scry 3" (Maybe.listToMaybe (tableLibrary (runTable 20 board))) (Just fourth)
      _ -> Spec.assertFailure s "expected six library cards"
  -- Both endpoints of a printed N1-N2 belong to it (CR 706.3a). 9 and 10 above
  -- are the adjacent pair that separates the two bands; these are the far ends,
  -- which an off-by-one at the other endpoint moves.
  Spec.it s "CR 706.3a a closed range includes both of its endpoints" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, _, _, _] -> do
        -- Moving this endpoint UP to 2 is visible here; deleting the test
        -- altogether is not, 1 being the die's floor. Same shape as the 20
        -- striation above, at the other end.
        Spec.assertEqWith s "CR 706.3a: 1 is the low end of 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 1 board))) (Just second)
        Spec.assertEqWith s "CR 706.3a: 19 is the high end of 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 19 board))) (Just third)
      _ -> Spec.assertFailure s "expected six library cards"
  -- The roll, the striations and the table are ONE ability, so entering puts one
  -- object on the stack and one resolution runs the whole table. Four striations
  -- transcribed as four abilities would put four there.
  Spec.it s "CR 706.3b the roll and its table are one ability" $ do
    (_, board) <- tableBoard s registry
    Spec.assertEqWith s "CR 706.3b: one ability on the stack, not one per striation" (length (TurnRegisters.stack (GameState.turn (placeTable 9 board)))) 1
    Spec.assertEqWith s "and it is gone after one resolution" (length (TurnRegisters.stack (GameState.turn (runTable 9 board)))) 0
  -- The fixture pin, in its own case so it cannot stand in for a striation
  -- above: the library really is the six cards in the order those assertions
  -- index, and CR 701.22a moves no card OUT of it, which is what makes "the top
  -- card" the whole reading.
  Spec.it s "CR 701.22a the fixture is six cards and the scry keeps them all" $ do
    (ids, board) <- tableBoard s registry
    Spec.assertEqWith s "six cards, top-first" (tableLibrary board) ids
    Spec.assertEqWith s "CR 701.22a: the library still holds all six" (length (tableLibrary (runTable 9 board))) 6

-- alice holds Valiant Endeavor with six untapped Plains, and BOB controls the
-- two creatures the destruction judges: Bird Maiden (power 1) and Barkhide
-- Mauler (power 4). The two powers straddle every result the cases below choose,
-- so which creature survives IS the number the roller chose.
--
-- Under bob so that the Knight tokens, which arrive under alice (CR 111.2), are
-- the only things she owns on the battlefield -- S.countOnBattlefieldByName
-- indexes by owner, so a victim of hers would be counted while it lived.
endeavorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
endeavorBoard s registry = do
  endeavor <- S.printingOf s registry "Valiant Endeavor"
  plains <- S.printingOf s registry "Plains"
  maiden <- S.printingOf s registry "Bird Maiden"
  mauler <- S.printingOf s registry "Barkhide Mauler"
  let (held, spell) = S.handOne endeavor (S.landsInPlay plains 6)
      (weak, withWeak) = S.addPermanent maiden S.bob held
      (strong, withBoth) = S.addPermanent mauler S.bob withWeak
  pure (spell, weak, strong, withBoth)

-- Answers all three questions one Endeavor under a CR 706.2 reroll asks: each
-- die comes up the next number of `rolls`, each reroll offer takes the next
-- answer of `decisions`, and the roller chooses the result at `index`. Neither
-- the modifier nor its payer is anything this answerer reads.
--
-- STATEFUL, because a die's two throws are the same Prompt.RollDie question,
-- so a pure answerer cannot tell a die from its own reroll.
--
-- Six for a roll the script did not plan and Declines for an offer it did not
-- plan: a surplus throw shows up as the die's top face rather than as CR
-- 706.1a's floor, and a surplus offer stops rather than looping.
rerollAnswer :: Natural.Natural -> Prompt.Prompt r -> State.State ([Natural.Natural], [OptionalDecision.OptionalDecision]) r
rerollAnswer index p = case p of
  Prompt.RollDie _ -> do
    (rolls, decisions) <- State.get
    case rolls of
      h : t -> do
        State.put (t, decisions)
        pure h
      [] -> pure 6
  Prompt.Rethrow {} -> do
    (rolls, decisions) <- State.get
    case decisions of
      h : t -> do
        State.put (rolls, t)
        pure h
      [] -> pure OptionalDecision.Declines
  Prompt.ChooseDieResult {} -> pure index
  _ -> pure (S.identityAnswer p)

-- CR 706.2a's OTHER half: "Modifiers may be optional and\/or have associated
-- costs." Wall of Fortune ("Defender \/ You may tap an untapped Wall you control
-- to have any player reroll a die that player rolled") is the whole producer --
-- the only printing whose reroll charges anything, and the only one whose
-- modifier reaches a roll the modifier's own controller did not make.
--
-- THE SAME ENDEAVOR FIXTURE and the same 3-6-2 script as Clam-I-Am's scenario,
-- deliberately: the Wall states no die size and no natural result, so the two
-- modifiers differ in the COST alone and the Clam's numbers carry over. Six
-- Knights means the reroll happened, three means it did not.
--
-- THE COST PROVES ITSELF by tapping the only Wall on the board. The Wall is a
-- Wall, so it pays for itself; having paid it is tapped, and its own filter no
-- longer admits anything -- which is why ONE Exercises is the whole script even
-- though the Wall gates on no number and would otherwise be offered over every
-- throw. An engine that charged nothing offers the second throw's 6 a reroll
-- too.
--
-- THREE BOARDS differing in one thing: no Wall, an untapped Wall, and a Wall
-- already tapped. The third is the cost's own leg -- the modifier is in force
-- and matches the die, and only CR 118.3 -- no paying a cost without the
-- resources to pay it fully -- keeps the reroll off the board.
--
-- THE SEAT is the fourth case and needs the second player: BOB's Wall over
-- ALICE's roll. Rule 109.5 puts the "may" and the payment on the Wall's
-- controller, so bob is asked and bob's Wall taps, and the die alice rolled is
-- the one thrown again. All four, and the reroll as alice's own roll, are in
-- data/scenarios/dice.
costedRerollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
costedRerollSpec s registry = Spec.describe s "Costed reroll" $ do
  Spec.it s "CR 706.2b the player who rolled picks which modifier applies" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    wall <- S.printingOf s registry "Wall of Fortune"
    let (clammer, clammed) = S.addPermanent clam S.alice board
        (walled, both) = S.addPermanent wall S.bob clammed
        pickedWall = pickRun [3, 2, 6] [OptionalDecision.Exercises] walled spell both
        pickedClam = pickRun [3, 2, 6] [OptionalDecision.Exercises] clammer spell both
    -- THE GAMEPLAY ASSERTION: alice's Clam-I-Am and bob's Wall of Fortune
    -- both offer to reroll alice's natural 3, and every offer put is taken.
    -- Alice picks the Wall, so bob is asked first and his Wall pays. An engine
    -- that put the offers in timestamp order asks alice's Clam first, and the
    -- Wall stays untapped.
    Spec.assertBool s (Game.isTapped walled pickedWall) "CR 706.2b: alice picks bob's Wall, so it is the modifier applied"
    -- The paired run, one answer different: picking her own Clam rerolls the 3
    -- into a 6, which the Wall is then declined over.
    Spec.assertBool s (not (Game.isTapped walled pickedClam)) "and picking her own Clam leaves the Wall untapped"
  Spec.it s "CR 706.2b the roller picks among their own modifiers" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    clam <- S.printingOf s registry "Clam-I-Am"
    let (_, walled) = S.addPermanent wall S.alice board
        (clammer, both) = S.addPermanent clam S.alice walled
    -- THE GAMEPLAY ASSERTION: alice controls both, the Wall first by
    -- timestamp. She picks the free Clam; its reroll comes up 1, and the Wall,
    -- still standing, rerolls that into the 6 that is the other result. An
    -- engine that put her own offers in timestamp order spends the Wall on the
    -- 3, the 1 stands, and one Knight arrives.
    Spec.assertEqWith
      s
      "CR 706.2b: the Clam first, then the Wall over its 1"
      (S.countOnBattlefieldByName knight S.alice (pickRun [3, 2, 1, 6] [OptionalDecision.Exercises, OptionalDecision.Exercises] clammer spell both))
      6

-- The Endeavor cast under rerollAnswer, except that the roller's CR 706.2b
-- pick takes the modifier `wanted` states, pinned by index.
pickRun :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
pickRun rolls decisions wanted spell board =
  let answering :: Prompt.Prompt r -> State.State ([Natural.Natural], [OptionalDecision.OptionalDecision]) r
      answering p = case p of
        Prompt.ChooseRollModifier _ _ candidates -> pure (List.genericLength (NonEmpty.takeWhile (/= Just wanted) candidates))
        _ -> rerollAnswer 1 p
   in snd (State.evalState (Engine.runGame answering board (S.cast S.alice spell >> Stack.resolveTop)) (rolls, decisions))

-- Goblin Bookie's "{R}, {T}: Reflip any coin or reroll any die. (Activate only
-- any time it makes sense.)", read as a window inside CR 706.2's modification
-- step where the ability is activated and resolves at once. The reroll itself
-- is the scenarios dice/cr-706-2b-goblin-bookie-*, the coin half
-- coin/cr-705-1-goblin-bookie-*.
activatedRerollSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
activatedRerollSpec s registry = Spec.describe s "Activated reroll" $ do
  Spec.it s "CR 117.1b the Bookie cannot be activated at priority" $ do
    (_, _, _, board, bookie, _) <- bookieBoard s registry
    -- No die is being rolled, so it never makes sense; with the mana and the
    -- untapped Bookie the reroll scenario pays with.
    Spec.assertBool
      s
      (not (any (\ability -> Activatable.activatable S.bob bookie ability board) (Activatable.abilitiesFor bookie board)))
      "the Bookie's ability is not activatable outside a roll"
    Spec.assertBool s (not (null (Activatable.abilitiesFor bookie board))) "and the Bookie has the ability"

-- The Endeavor board plus bob's Goblin Bookie and a Mountain to pay its {R}.
bookieBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bookieBoard s registry = do
  (spell, weak, strong, board) <- endeavorBoard s registry
  bookiePrinting <- S.printingOf s registry "Goblin Bookie"
  mountainPrinting <- S.printingOf s registry "Mountain"
  let (bookie, withBookie) = S.addPermanent bookiePrinting S.bob board
      (mountain, withBoth) = S.addPermanent mountainPrinting S.bob withBookie
  pure (spell, weak, strong, withBoth, bookie, mountain)

-- CR 706.2's third sentence, "Modifiers may also come from other sources", in
-- rule 706.2b's SECOND bucket: Night Shift of the Living Dead ("After you roll a
-- die, you may pay 1 life. If you do, increase or decrease the result by 1. Do
-- this only once each turn. / Whenever you roll a 6, create a 2/2 black Zombie
-- Employee creature token."). Its first ability is the modifier, with CR
-- 706.2a's optional cost; its second reads the RESULT, which CR 706.2's last
-- sentence makes the number after every modifier -- so a 5 shifted up mints a
-- Zombie and a 6 shifted down does not.
--
-- THE SAME ENDEAVOR FIXTURE as the reroll groups above: two d6, the roller
-- picks the destroying result and "the other result" counts Knights. Two
-- readings, then: the Knights say which number the unchosen die ended on, and
-- the Zombie Employee tokens say whether a 6 was among the final results.
--
-- CR 603.3 is why nightShiftRun drains the stack after the Endeavor resolves:
-- "whenever you roll a 6" triggers during that resolution and reaches the stack
-- only at the next placement.
nightShiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nightShiftSpec s registry = Spec.describe s "IncreaseOrDecrease" $ do
  -- CR 706.6: Berserker's Frenzy's "roll two d20 and ignore the lower roll".
  -- The ignored 6 never happened, so "whenever you roll a 6" does not see it and
  -- the shift is offered over the 17 alone. Should the 6 count, the roller is
  -- asked which result to use and takes the 17, so only the Zombie and the offer
  -- tell the readings apart.
  Spec.it s "CR 706.6 an ignored 6 mints no Zombie and is offered no shift" $ do
    frenzy <- S.printingOf s registry "Berserker's Frenzy"
    mountain <- S.printingOf s registry "Mountain"
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let (held, spell) = S.handOne frenzy (S.landsInPlay mountain 3)
        board = snd (S.addPermanent shift S.alice held)
        (after, shown) = nightShiftRun [6, 17] [] [] 1 spell board
    Spec.assertEqWith s "CR 706.6: no ability triggers on the ignored 6" (S.countOnBattlefieldByName zombieEmployee S.alice after) 0
    Spec.assertEqWith s "CR 706.6: no effect applies to it, so only the 17 is shown" shown [[17]]
  -- CR 701.67a reaches a waterbend cost wherever it is paid, so CR 118.13's
  -- announcement of a modifier's Phyrexian symbol weighs its taps as the gate
  -- did. A test-local Night Shift costing {1}{W/P} and waterbend {1}, which no
  -- printing states: alice has no mana left after the Endeavor and one untapped
  -- creature, so paying 2 life and tapping the creature is the only route. An
  -- announcement blind to the taps finds none, falls back to the mana, and the
  -- payment fails.
  Spec.it s "CR 701.67a a modifier's waterbend cost is weighed when its Phyrexian symbol is announced" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.cardOf s registry "Night Shift of the Living Dead"
    maiden <- S.printingOf s registry "Bird Maiden"
    let waterbent = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.Phyrexian Color.White]), Cost.Type.components = [CostComponent.Waterbend (CostAmount.Fixed 1)]}
        recost effect = case effect of
          PlayerEffect.ModifyDieRoll offer -> PlayerEffect.ModifyDieRoll offer {ModifiedRoll.cost = Just waterbent}
          other -> other
        restate ability = ability {PlayerStaticAbility.effect = recost (PlayerStaticAbility.effect ability)}
        variant = shift {Card.Type.faces = fmap (\face -> face {Face.playerAbilities = fmap restate (Face.playerAbilities face)}) (Card.Type.faces shift)}
        (bird, withBird) = S.addPermanent maiden S.alice board
        shifted = snd (S.addPermanent (Printing.ofCard variant) S.alice withBird)
        (after, shown) = nightShiftRun [5, 2] [] [Just (0, RollAdjustment.Increase)] 1 spell shifted
    -- THE GAMEPLAY ASSERTION: the 5 was pushed to 6, so six Knights.
    Spec.assertEqWith s "CR 706.2a: the waterbent shift moves the 5 to 6" (S.countOnBattlefieldByName knight S.alice after) 6
    Spec.assertEqWith s "CR 107.4f: the Phyrexian symbol was paid with 2 life" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "CR 701.67a: the creature was tapped for the generic" (Object.tapped <$> Game.lookupObject bird after) (Just TapState.Tapped)
    Spec.assertEqWith s "the offer was raised" shown [[5, 2]]
  Spec.it s "CR 706.2b a declined shift is offered again once another applies" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice (snd (S.addPermanent shift S.alice board)))
        answers = [Nothing, Just (0 :: Natural.Natural, RollAdjustment.Increase), Just (0, RollAdjustment.Increase)]
        (after, _) = nightShiftRun [4, 2] [] answers 1 spell shifted
    -- THE GAMEPLAY ASSERTION: two Night Shifts over a 4. The first is
    -- declined, the second pushes the 4 to 5, and the first, put again over the
    -- 5, pushes it to 6. An engine that spent the declined offer stops at 5.
    Spec.assertEqWith s "CR 706.2b: the declined Night Shift moves the 5 to 6" (S.countOnBattlefieldByName knight S.alice after) 6
  -- CR 706.3a's single-number striation against a result the die cannot
  -- show: Djinni Windseer's "20 | Scry 3." under a natural 20 shifted to 21. The
  -- table's `20` is an exact number, so 21 fires nothing and alice's top card
  -- is the first; a `20+` reading scries 3. The decrease leg is the pair: 19 is
  -- in 10-19 and scries 2.
  Spec.it s "CR 706.3a a result shifted past the die's top face fires no single-number striation" $ do
    (ids, board) <- tableBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        shiftAnswer :: RollAdjustment.RollAdjustment -> Prompt.Prompt r -> r
        shiftAnswer direction p = case p of
          Prompt.AdjustDieRoll {} -> Just (0, direction)
          _ -> tableAnswer 20 p
        run direction = S.runPure (shiftAnswer direction) (S.runPure (shiftAnswer direction) shifted (Engine.placePendingTriggers [])) Stack.resolveTop
    case ids of
      [first, _, third, _, _, _] -> do
        Spec.assertEqWith s "CR 706.3a: 21 is not 20, so nothing is scried" (Maybe.listToMaybe (tableLibrary (run RollAdjustment.Increase))) (Just first)
        Spec.assertEqWith s "CR 706.3a: 19 is in 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (run RollAdjustment.Decrease))) (Just third)
      _ -> Spec.assertFailure s "the fixture library is six cards"

zombieEmployee :: CardName.CardName
zombieEmployee = CardName.MkCardName (Text.pack "Zombie Employee Token")

-- Answers every question one Endeavor under Night Shift asks, rerollAnswer's
-- script extended by the shift: each die comes up the next of `rolls`, each
-- reroll offer takes the next of `rerolls`, each shift offer the next of
-- `shifts`, and the roller chooses the result at `index`. Records the results
-- each shift offer SHOWED, which the board cannot say.
--
-- Declines for an unplanned shift offer, and six for an unplanned throw,
-- rerollAnswer's reasons. Takes every waterbend tap offered.
nightShiftAnswer :: Natural.Natural -> Prompt.Prompt r -> State.State ([Natural.Natural], [OptionalDecision.OptionalDecision], [Maybe (Natural.Natural, RollAdjustment.RollAdjustment)], [[Integer]]) r
nightShiftAnswer index p = case p of
  Prompt.RollDie _ -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case rolls of
      h : t -> State.put (t, rerolls, shifts, shown) >> pure h
      [] -> pure 6
  Prompt.Rethrow {} -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case rerolls of
      h : t -> State.put (rolls, t, shifts, shown) >> pure h
      [] -> pure OptionalDecision.Declines
  Prompt.AdjustDieRoll _ _ results _ _ -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case shifts of
      h : t -> State.put (rolls, rerolls, t, NonEmpty.toList results : shown) >> pure h
      [] -> State.put (rolls, rerolls, [], NonEmpty.toList results : shown) >> pure Nothing
  Prompt.ChooseDieResult {} -> pure index
  -- CR 701.67a's offer, ascending: the last entry taps the most.
  Prompt.ChooseCost _ _ _ offered@(_ : _) _ -> pure (last offered)
  _ -> pure (S.identityAnswer p)

-- Cast the Endeavor, resolve it, then place and drain whatever triggered (CR
-- 603.3) under the same script. Returns the board and the results each shift
-- offer showed, in order.
nightShiftRun :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> [Maybe (Natural.Natural, RollAdjustment.RollAdjustment)] -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[Integer]])
nightShiftRun rolls rerolls shifts index spell board =
  let drain :: Int -> Game.Type.Game ()
      drain n = do
        gs <- State.get
        Monad.unless (n <= 0 || null (TurnRegisters.stack (GameState.turn gs))) (Stack.resolveTop >> drain (n - 1))
      script = S.cast S.alice spell >> Stack.resolveTop >> Engine.placePendingTriggers [] >> drain 8
      ((_, after), (_, _, _, shown)) = State.runState (Engine.runGame (nightShiftAnswer index) board script) (rolls, rerolls, shifts, [])
   in (after, reverse shown)

-- CR 706.3c on Delina, Wild Mage {3}{R} 3/2, "Whenever Delina attacks, choose
-- target creature you control, then roll a d20. / 1-14 | Create a tapped and
-- attacking token that's a copy of that creature, except it's not legendary and
-- it has "At end of combat, exile this token." / 15-20 | Create one of those
-- tokens. You may roll again." Delina attacks bob alone, so she is her own
-- target, and her tokens surviving CR 704.5j beside her is the "not legendary".
--
-- `rolls` answers the d20s in order, and `again` every ask while a roll is left
-- to answer; past the script alice declines, so a missing gate costs one extra
-- token rather than a loop that never ends. Returns the board at declare
-- blockers, the board after combat, and how many times alice was asked.
delinaRun :: [Natural.Natural] -> OptionalDecision.OptionalDecision -> GameState.GameState -> (GameState.GameState, GameState.GameState, Int)
delinaRun rolls again board =
  let answer :: Prompt.Prompt r -> State.State ([Natural.Natural], Int) r
      answer p = case p of
        Prompt.RollDie _ -> do
          (left, seen) <- State.get
          case left of
            n : rest -> State.put (rest, seen) >> pure n
            [] -> pure 1
        Prompt.ChooseRepeat {} -> do
          (left, seen) <- State.get
          State.put (left, seen + 1)
          pure (if null left then OptionalDecision.Declines else again)
        _ -> pure (S.attackTo S.bob p)
      go stop n gs =
        if n <= (0 :: Int) || stop gs || not (S.inCombatPhase (TurnRegisters.phase (GameState.turn gs)))
          then pure gs
          else do
            (_, next) <- Engine.runGame answer gs Engine.runStep
            go stop (n - 1) next
      atBlockers = State.evalState (go (\gs -> TurnRegisters.phase (GameState.turn gs) == Phase.Combat CombatStep.DeclareBlockers) 8 board) (rolls, 0)
      (after, (_, asked)) = State.runState (go (const False) 24 board) (rolls, 0)
   in (atBlockers, after, asked)

delinaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
delinaSpec s registry = Spec.describe s "Delina" $ do
  let delinaCase rolls again = do
        delina <- S.printingOf s registry "Delina, Wild Mage"
        let (board, _, _) = S.combatBoardOf [delina] []
        pure (delinaRun rolls again board)
      attackingBob gs = fmap (\oid -> (fmap Object.tapped (Game.lookupObject oid gs), Map.lookup oid (Combat.Type.attackers (TurnRegisters.combat (GameState.turn gs))))) (S.tokensOf gs)
      tappedAtBob n = replicate n (Just TapState.Tapped, Just (AttackTarget.OfPlayer S.bob))
  -- Each script carries one roll past where the gate should stop the loop, so
  -- an ungated ask is answered yes and shows up as an extra token.
  Spec.it s "CR 706.3c a 17 then a 9 makes two tokens and asks once" $ do
    (atBlockers, after, asked) <- delinaCase [17, 9, 9] OptionalDecision.Exercises
    Spec.assertEqWith s "CR 508.4 two non-legendary Delinas, tapped and attacking bob" (attackingBob atBlockers) (tappedAtBob 2)
    Spec.assertEqWith s "alice was asked after the 17 and not after the 9" asked 1
    Spec.assertEqWith s "CR 510.1b bob takes 3 from Delina and 3 from each token" (S.lifeOf S.bob after) (Just 11)
    Spec.assertEqWith s "CR 511.2 each token's own end-of-combat trigger exiles it" (S.tokensOf after) []
  Spec.it s "CR 706.3c a 9 makes one token and asks nothing" $ do
    (atBlockers, _, asked) <- delinaCase [9, 9] OptionalDecision.Exercises
    Spec.assertEqWith s "one token" (attackingBob atBlockers) (tappedAtBob 1)
    Spec.assertEqWith s "alice was never asked" asked 0
  Spec.it s "CR 706.3c a 20 and a 15 each offer another roll" $ do
    (atBlockers, _, asked) <- delinaCase [20, 15, 14, 9] OptionalDecision.Exercises
    Spec.assertEqWith s "three tokens" (attackingBob atBlockers) (tappedAtBob 3)
    Spec.assertEqWith s "asked after the 20 and the 15" asked 2
  Spec.it s "CR 608.2d a declined roll again ends the loop" $ do
    (atBlockers, _, asked) <- delinaCase [17, 9] OptionalDecision.Declines
    Spec.assertEqWith s "one token" (attackingBob atBlockers) (tappedAtBob 1)
    Spec.assertEqWith s "asked once" asked 1
