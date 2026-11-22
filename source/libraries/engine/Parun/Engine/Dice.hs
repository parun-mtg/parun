-- CR 706 past the roll itself: rule 706.6's ignored roll, and rule 706.2's
-- modifiers that reach a roll from a source other than its own instruction.
-- Rule 705's Parun.Engine.Coin one rule over, and the same division of labour --
-- the replaceable event itself is a funnel in Parun.Engine.Event
-- (proposeDiceRoll), because CR 614's loop lives there, and what is left of rule
-- 706 lives here.
module Parun.Engine.Dice where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Turn as Turn
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.Filter as Filter
import Parun.Types.Game (Game)
import Parun.Types.GameEvent (GameEvent)
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.PermissionLimit as PermissionLimit
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.RerollReach as RerollReach
import qualified Parun.Types.RollModifier as RollModifier
import qualified Parun.Types.ShownDie as ShownDie
import qualified Parun.Types.TapPermanents as TapPermanents
import qualified Parun.Types.TurnRegisters as TurnRegisters

-- | The CR 706.2 modifiers in force over the rolls `pid` is making, read fresh
-- for EACH die rather than once for the instruction: rule 706.2 words a modifier
-- against "the roll" singular, and a reroll of the first die can move the board
-- the second die's offer is read off.
--
-- Parun.Engine.Coin.statementsFor's sibling, and deliberately NOT its
-- once-per-instruction posture: that one is Edgar, King of Figaro's printed
-- "the first time you flip one or more coins each turn", plain card text with no
-- rule behind it, and no printing of a die-roll modifier says anything like it.
modifiersFor :: PlayerId -> Game [(Maybe ObjectId, ModifiedRoll.ModifiedRoll)]
modifiersFor pid = fmap (PlayerEffect.rollModifiers pid) State.get

-- | CR 706.2b's first step: which of the modifiers in force offer a REROLL of a
-- die of `sides` that came up the natural result `natural`, each still tagged
-- with the object that states it.
--
-- The NATURAL result, because rule 706.2b considers rerolls before any increase
-- or decrease -- so nothing has moved the number when this is asked, and an
-- implementation that read the instruction's own modifier in first would gate
-- Clam-I-Am's "if you roll a 3" on a number no die showed.
--
-- The matching modifiers and not a Bool, because CR 706.2a lets each one carry
-- its own COST and name its own payer, and two offers that differ in either are
-- two different questions.
--
-- An unstated `sides` or `natural` matches every roll -- Wall of Fortune's bare
-- "a die" -- rather than none.
--
-- `reach` picks the offers that reach that many dice (Parun.Types.RerollReach).
-- `natural` is Nothing where the offer is over several dice at once, and then
-- a modifier stating a number is not one of them; no printing states both.
rerollOffers :: RerollReach.RerollReach -> Natural.Natural -> Maybe Natural.Natural -> [(Maybe ObjectId, ModifiedRoll.ModifiedRoll)] -> [(Maybe ObjectId, ModifiedRoll.ModifiedRoll)]
rerollOffers reach sides natural modifiers =
  let matches modifier =
        ModifiedRoll.modifier modifier == RollModifier.Reroll reach
          && all (== sides) (ModifiedRoll.sides modifier)
          && all (\stated -> natural == Just stated) (ModifiedRoll.natural modifier)
   in filter (matches . snd) modifiers

-- | Pippa, Duchess of Dice's ruling ("Die creatures ... can be rerolled at any
-- time ... by effects of cards like Wall of Fortune"): each reroll modifier
-- reaching a permanent that is a die outside any roll, with the object stating
-- it, the die, and the player who rerolls it -- the die's owner, who rolled it
-- (docs/decisions.md, "Who rolled a Die creature's die"). Only a modifier
-- reaching that die alone and stating no number, `rerollOffers`'s Nothing:
-- docs/decisions.md, "Whether Monitor Monitor rerolls a Die creature outside a
-- roll". One with no object behind it has no payer and is not offered.
shownDieRerolls :: GameState -> [(ObjectId, ModifiedRoll.ModifiedRoll, ObjectId, PlayerId)]
shownDieRerolls gs =
  [ (stated, offer, die, roller)
  | die <- Game.diceOnBattlefield gs,
    Just obj <- [Game.lookupObject die gs],
    let roller = Object.owner obj,
    Just shown <- [Object.die obj],
    (Just stated, offer) <- rerollOffers RerollReach.ThatDie (ShownDie.sides shown) Nothing (PlayerEffect.rollModifiers roller gs)
  ]

-- | CR 706.2b's SECOND step: which of the modifiers in force offer to increase
-- or decrease the result of a die of `sides`, each tagged with the object that
-- states it and paired with the amount.
--
-- `natural` is not read: it is a reroll's narrowing (Clam-I-Am), and no
-- increase-or-decrease printing states one.
adjustOffers :: Natural.Natural -> [(Maybe ObjectId, ModifiedRoll.ModifiedRoll)] -> [(Maybe ObjectId, ModifiedRoll.ModifiedRoll, Natural.Natural)]
adjustOffers sides modifiers =
  let amountOf (stated, modifier) = case ModifiedRoll.modifier modifier of
        RollModifier.Reroll _ -> Nothing
        RollModifier.IncreaseOrDecrease amount ->
          if all (== sides) (ModifiedRoll.sides modifier) then Just (stated, modifier, amount) else Nothing
   in Maybe.mapMaybe amountOf modifiers

-- | CR 706.2b: which of the competing modifiers `roller` applies next, each
-- tagged with its payer and the object that states it, and the rest. Every
-- candidate must already be applicable now; Nothing when none is. `dieOf`
-- names the one die a candidate reaches, by its place in the roll and its
-- natural result, Nothing where it reaches the roll as a whole. Also which
-- die one player's own effect reaches, `roller` being that player (CR 608.2d,
-- throwSettled's `offeringAll`).
--
-- Asked whenever two or more stand, the roller's own included (Parun.DiceSpec's
-- "CR 706.2b the roller picks among their own modifiers").
--
-- FILTERED, NOT TRUSTED: an index past the end takes the first.
pickModifier :: PlayerId -> (a -> Maybe (Natural.Natural, Natural.Natural)) -> [(PlayerId, Maybe ObjectId, a)] -> Game (Maybe ((PlayerId, Maybe ObjectId, a), [(PlayerId, Maybe ObjectId, a)]))
pickModifier roller dieOf candidates = case candidates of
  [] -> pure Nothing
  first : rest
    | null rest -> pure (Just (first, rest))
    | otherwise -> do
        gs <- State.get
        index <- Game.choose (Prompt.ChooseRollModifier (Decide.deciderFor roller gs) roller (fmap (\(_, stated, reached) -> (stated, dieOf reached)) (first NonEmpty.:| rest)))
        pure $ case List.genericSplitAt index candidates of
          (before, chosen : after) -> Just (chosen, before <> after)
          _ -> Just (first, rest)

-- | Whether a modifier's printed budget still admits it: Night Shift of the
-- Living Dead's "Do this only once each turn", read against
-- TurnRegisters.rollModifiersUsedThisTurn; a once-each-of-your-turns budget only on
-- `payer`'s own turn as well (CR 805.4a's team turn included), `payer` being
-- the modifier's "you" (CR 109.5). A
-- budgeted modifier with no object behind it has nowhere to be spent and is not
-- offered; no printing has one.
withinLimit :: GameState -> PlayerId -> Maybe ObjectId -> ModifiedRoll.ModifiedRoll -> Bool
withinLimit gs payer stated modifier =
  let unspent = case stated of
        Nothing -> False
        Just oid -> Set.notMember modifier (Map.findWithDefault Set.empty oid (Turn.keyedThisTurn Set.union TurnRegisters.rollModifiersUsedThisTurn gs))
   in case ModifiedRoll.limit modifier of
        PermissionLimit.Unlimited -> True
        PermissionLimit.OnceEachTurn -> unspent
        PermissionLimit.OnceEachOfYourTurns -> Turn.isActive gs payer && unspent
        -- A modifier is never a stored row with a use to drop.
        PermissionLimit.OncePerEffect -> False

-- | Whether two offers state the same question to the same payer, so asking
-- both is asking among options nothing tells apart. Two free offers always
-- are: either throws the same die. Two with a CR 706.2a cost are where the
-- modifier is the same and unbudgeted (`withinLimit` then reads no object),
-- and paying the cost reads nothing of the object stating it: Wall of
-- Fortune's "tap an untapped Wall you control" taps any Wall, whichever Wall
-- states it. The scenario
-- cr-706-2b-offers-from-walls-nothing-tells-apart-are-one-question proves it.
sameOffer :: (PlayerId, Maybe ObjectId, ModifiedRoll.ModifiedRoll) -> (PlayerId, Maybe ObjectId, ModifiedRoll.ModifiedRoll) -> Bool
sameOffer (payer, stated, offer) (payer', stated', offer') =
  payer == payer' && case (ModifiedRoll.cost offer, ModifiedRoll.cost offer') of
    (Nothing, Nothing) -> True
    (Just cost, Just _) ->
      offer == offer'
        && ModifiedRoll.limit offer == PermissionLimit.Unlimited
        && Maybe.isJust stated
        && Maybe.isJust stated'
        && readsNoStater cost
    _ -> False

-- Whether paying this cost reads nothing of the object it is paid against.
-- CONSERVATIVE, an allowlist: any mana (whose spending a restriction may read
-- the object by), a component other than tapping a fixed number of
-- permanents, or a Filter atom other than those read of the candidate and of
-- the payer alone, answers False, which only keeps two offers apart.
readsNoStater :: Cost.Cost keyword -> Bool
readsNoStater cost = maybe False (null . ManaCost.unwrap) (Cost.mana cost) && all component (Cost.components cost)
  where
    component c = case c of
      CostComponent.TapPermanents tap -> case TapPermanents.count tap of
        CostAmount.Fixed _ -> candidateOnly (TapPermanents.whichPermanents tap)
        CostAmount.Announced _ -> False
      _ -> False
    candidateOnly f = case f of
      Filter.And fs -> all candidateOnly fs
      Filter.Or fs -> all candidateOnly fs
      Filter.Not g -> candidateOnly g
      Filter.HasSubtype _ -> True
      Filter.HasCardType _ -> True
      Filter.IsTapped -> True
      -- "You" is the payer, whom both offers share.
      Filter.HeldBy _ (ControllerRelation.Related _) -> True
      _ -> False

-- | Spend a modifier's budget once it is taken; a no-op for an unbudgeted one.
spendLimit :: Maybe ObjectId -> ModifiedRoll.ModifiedRoll -> GameState -> GameState
spendLimit stated modifier gs = case stated of
  Just oid
    | ModifiedRoll.limit modifier /= PermissionLimit.Unlimited ->
        gs
          { GameState.turn =
              (GameState.turn gs)
                { TurnRegisters.rollModifiersUsedThisTurn = Map.insertWith Set.union oid (Set.singleton modifier) (TurnRegisters.rollModifiersUsedThisTurn (GameState.turn gs))
                }
          }
  _ -> gs

-- | CR 706.6: throw away the `n` lowest of these rolls, so that what comes back
-- is the rolls the instruction may still read. Rule 706.6 makes an ignored roll
-- one that HAPPENED -- the die was thrown and its face asked for -- and is then
-- treated as never having happened, so this runs on the results rather than on
-- the count of dice, and the caller has already asked for every face. `value` is
-- the number a roll is compared by: a die's result, or the 1 or 20 a coin stands
-- for (Pokey, the Scallywagg).
--
-- ONE AT A TIME, re-finding the minimum after each, because CR 706.6's sentence
-- is "the lowest roll" singular and two rows each ignore their own: over [1,1,5]
-- a single ignore leaves [1,5] and a second leaves [5], where a pass that took
-- every copy of the minimum at once would leave [5] for both.
--
-- A TIE is asked about only where the board can tell the tied rolls apart.
-- Rule 706.6's third sentence gives the roller the choice "if multiple results
-- are tied for the lowest". Tied rolls are equal NUMBERS, so where nothing else
-- separates them dropping either leaves the instruction the same results, and
-- where the rules leave nothing to ask, don't prompt -- the first is taken, so a
-- transcript replays. `key` is what else a roll carries: the player who threw it
-- (Pippa, Duchess of Dice's ruling gives a reroll to its thrower's "whenever you
-- roll"), or the outcome of the call a coin standing for a die was flipped
-- against, or inside a reroll the face it was thrown with before a reroll of
-- it, which the reroll's own event reads, or the rerolls it carries and the
-- board each came up on. Where the tied rolls differ in it, the roller chooses
-- which is ignored, through Prompt.ChooseDieResult, offered one roll per
-- distinct key, earliest first, each as `shownOf` shows it, which the caller
-- keeps apart wherever the keys differ. FILTERED, NOT TRUSTED: an index past
-- the end ignores the first.
--
-- Ignoring more rolls than were made leaves none rather than failing; the
-- instruction then binds nothing and reads an unbound slot. Unreachable from card
-- data -- every ignore comes from a row that added the die it ignores -- and so
-- defensive.
ignoreLowest :: (Ord k) => PlayerId -> (a -> Integer) -> (a -> k) -> (a -> (Natural.Natural, PlayerId, [GameEvent], [Natural.Natural])) -> Natural.Natural -> [a] -> Game [a]
ignoreLowest roller valueOf keyOf shownOf n rolls = case NonEmpty.nonEmpty rolls of
  Just held | n > 0 -> do
    let low = minimum (fmap valueOf held)
        tied = [i | (i, roll) <- zip [0 :: Int ..] rolls, valueOf roll == low]
        firstOfEach seen indices = case indices of
          [] -> []
          i : rest -> case drop i rolls of
            roll : _
              | Set.member (keyOf roll) seen -> firstOfEach seen rest
              | otherwise -> i : firstOfEach (Set.insert (keyOf roll) seen) rest
            [] -> firstOfEach seen rest
    dropped <- case firstOfEach Set.empty tied of
      first : rest@(_ : _) -> do
        gs <- State.get
        answer <- Game.choose (Prompt.ChooseDieResult (Decide.deciderFor roller gs) roller Nothing (fmap (\i -> maybe (Integer.toNaturalSaturating low, roller, [], []) shownOf (Maybe.listToMaybe (drop i rolls))) (first NonEmpty.:| rest)))
        pure (Maybe.fromMaybe first (Maybe.listToMaybe (List.genericDrop answer (first : rest))))
      first : _ -> pure first
      -- Unreachable: the minimum of a non-empty list is one of its members.
      [] -> pure 0
    let (before, after) = splitAt dropped rolls
    ignoreLowest roller valueOf keyOf shownOf (n - 1) (before <> drop 1 after)
  _ -> pure rolls
