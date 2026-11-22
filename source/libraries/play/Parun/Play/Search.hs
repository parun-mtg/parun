{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The search bot: flat Monte Carlo over a seat's priority and combat
-- decisions. Each
-- option is played out in worlds dealt consistently with what the seat has
-- seen, the budget's playout policy in every seat, and the option winning
-- most is taken.
module Parun.Play.Search where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Engine.Card as Engine.Card
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Registry as Registry
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Combat as Combat
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Features as Features
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.History as History
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PastTurn as PastTurn
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Program as Program
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Weights as Weights
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange

-- | How a viewer deals an owner's unseen cards in a world it samples: given
-- what the viewer remembers, the viewer, the world so far and the owner, the
-- world with every printing the deal names interned and the contents to deal
-- onto that owner's 'unseen' objects, in their order, any object past the
-- last keeping its own.
newtype Prior = MkPrior (forall m. (Monad m) => Memory -> PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> State.StateT Generator.Type.Generator m (GameState.GameState, [Contents]))

-- | What a card object is, apart from where it sits: the card behind it and
-- its identity (CR 108.3).
type Contents = (Source.Source, Maybe CardIdentity.CardIdentity)

-- | The known deck list: an owner's unseen objects hold exactly their list
-- less every card the viewer has seen, so dealing their own contents anew,
-- shuffled, is dealing from that remainder. It assumes each list is known.
known :: Prior
known = MkPrior $ \memory viewer gs owner ->
  fmap ((,) gs) . Generator.shuffle $
    [ (Object.source object, Object.identity object)
    | oid <- unseen memory viewer gs owner,
      object <- Foldable.toList (Engine.Game.lookupObject oid gs)
    ]

-- | A meta's decks, each with its share, for lists not known: the viewer's
-- own cards dealt as 'known' deals them, and each other owner's from one deck
-- drawn by its 'posterior' given what the viewer 'seen' of theirs, less those
-- cards, with the 'loose' ones, shuffled, and once that runs short from
-- the whole deck shuffled.
inferred :: NonEmpty.NonEmpty (Natural, Deck.Deck) -> Prior
inferred meta = MkPrior $ \memory viewer gs owner ->
  let MkPrior ownList = known
      shown = seen memory viewer gs owner
      dealtOnto = unseen memory viewer gs owner
      loosened = Map.findWithDefault Map.empty owner (loose memory)
   in if owner == viewer
        then ownList memory viewer gs owner
        else do
          deck <- Generator.weighted (posterior meta shown)
          rest <- Generator.shuffle (spread (Map.differenceWith (\k s -> if k > s then Just (k - s) else Nothing) (Deck.cards deck) shown) <> spread loosened)
          whole <- Generator.shuffle (spread (Deck.cards deck))
          pure (dealtAs gs (zip dealtOnto (rest <> if null whole then [] else cycle whole)))

-- | A format's pool, for lists not known and no meta to infer them from, each
-- candidate a printing in it or Nothing for one not: the viewer's own cards
-- dealt as 'known' deals them, and each other owner's the 'loose' ones,
-- shuffled, then each a printing of the pool drawn evenly and independently,
-- blind to what they showed and to any copy limit. A candidate drawn that is
-- not one is drawn again, so a candidate is read only once drawn. Nothing
-- when no candidate is a printing.
pooled :: Seq.Seq (Maybe Printing.Printing) -> Maybe Prior
pooled candidates =
  let draw :: (Monad m) => State.StateT Generator.Type.Generator m Printing.Printing
      draw = Generator.between 0 (Natural.length candidates - 1) >>= maybe draw pure . Seq.index candidates . Natural.toIntSaturating
      deal :: (Monad m) => Memory -> PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> State.StateT Generator.Type.Generator m (GameState.GameState, [Contents])
      deal memory viewer gs owner =
        let MkPrior ownList = known
            dealtOnto = unseen memory viewer gs owner
         in if owner == viewer
              then ownList memory viewer gs owner
              else do
                rest <- Generator.shuffle (spread (Map.findWithDefault Map.empty owner (loose memory)))
                drawn <- Monad.replicateM (length dealtOnto - length rest) draw
                pure (dealtAs gs (zip dealtOnto (rest <> drawn)))
   in if any Maybe.isJust candidates then Just (MkPrior deal) else Nothing

-- | The world with each printing dealt interned, and the contents to deal
-- onto each object, its identity kept.
dealtAs :: GameState.GameState -> [(ObjectId.ObjectId, Printing.Printing)] -> (GameState.GameState, [Contents])
dealtAs gs dealt =
  let (world, ids) = Foldable.foldl' (\(w, acc) printing -> let (pid, w') = Engine.Game.intern printing w in (w', Map.insert printing pid acc)) (gs, Map.empty) (ListUtils.nubOrd (fmap snd dealt))
      identityOf oid = Object.identity =<< Engine.Game.lookupObject oid gs
   in (world, [(Source.OfCard pid, identityOf oid) | (oid, printing) <- dealt, pid <- Foldable.toList (Map.lookup printing ids)])

-- | Each printing as many times as its count.
spread :: Map.Map Printing.Printing Natural -> [Printing.Printing]
spread = foldMap (\(printing, n) -> List.genericReplicate n printing) . Map.toList

-- | The cards of `owner`'s that `viewer` sees now, by printing, with those it
-- remembers in their hand or library: the cards in the game outside the
-- command zone (CR 408.1), which holds what a player brings beside their
-- deck's cards (Deck).
seen :: Memory -> PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> Map.Map Printing.Printing Natural
seen memory viewer gs owner =
  Map.unionsWith
    (+)
    [ Map.fromListWith
        (+)
        [ (printing, 1)
        | (oid, object) <- Map.toList (GameState.objects gs),
          Object.owner object == owner,
          maybe False (/= Zone.Command) (Engine.Game.zoneOf oid gs),
          Visibility.sees viewer oid gs == Visibility.Type.Visible,
          printing <- Foldable.toList (printingOf object gs)
        ],
      Map.fromListWith (+) [(printing, 1) | oid <- pinned memory viewer gs owner, (_, _, printing) <- Foldable.toList (Map.lookup oid (remembered memory))],
      Map.findWithDefault Map.empty owner (loose memory)
    ]

-- | The printing behind a card object.
printingOf :: Object.Object -> GameState.GameState -> Maybe Printing.Printing
printingOf object gs = case Object.source object of
  Source.OfCard pid -> Engine.Game.printingOf pid gs
  _ -> Nothing

-- | Each deck of a meta weighed by its share times the chance a hand of its
-- cards the size of those seen is exactly them (multivariate hypergeometric),
-- normalised; by its share alone when no deck holds them all, and evenly when
-- no share is positive.
posterior :: NonEmpty.NonEmpty (Natural, Deck.Deck) -> Map.Map Printing.Printing Natural -> NonEmpty.NonEmpty (Rational, Deck.Deck)
posterior meta shown =
  let choose :: Natural -> Natural -> Rational
      choose n k = if k > n then 0 else toRational (product [n - k + 1 .. n] `div` product [1 .. k])
      likelihood deck =
        let ways = choose (sum (Deck.cards deck)) (sum shown)
         in if ways == 0 then 0 else product [choose (Map.findWithDefault 0 printing (Deck.cards deck)) k | (printing, k) <- Map.toList shown] / ways
      normalised weights =
        let total = sum (fmap fst weights)
         in if total == 0 then Nothing else Just (fmap (Bifunctor.first (/ total)) weights)
      evenly = fmap (\(_, deck) -> (1 / toRational (length meta), deck)) meta
   in Maybe.fromMaybe evenly (normalised (fmap (\(share, deck) -> (toRational share * likelihood deck, deck)) meta) <|> normalised (fmap (Bifunctor.first toRational) meta))

-- | CR 400.2: the objects in `owner`'s hand and library `viewer` may not see,
-- in hand-then-library order.
hidden :: PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> [ObjectId.ObjectId]
hidden viewer gs owner =
  let unseeable oid = Visibility.sees viewer oid gs /= Visibility.Type.Visible
   in filter unseeable (Foldable.toList (Map.findWithDefault mempty owner (GameState.hand gs)) <> Foldable.toList (Map.findWithDefault mempty owner (GameState.library gs)))

-- | The 'hidden' objects of `owner`'s that `viewer` does not remember, which
-- a world deals anew.
unseen :: Memory -> PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> [ObjectId.ObjectId]
unseen memory viewer gs owner = filter (`Map.notMember` remembered memory) (hidden viewer gs owner)

-- | The 'hidden' objects of `owner`'s that `viewer` remembers, which keep
-- what they hold in every world.
pinned :: Memory -> PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> [ObjectId.ObjectId]
pinned memory viewer gs owner = filter (`Map.member` remembered memory) (hidden viewer gs owner)

-- | What a viewer remembers of the cards it has seen, as of the first
-- `logged` entries of the game's whole log ('logOf').
data Memory = MkMemory
  { -- | Each card object it knows, with its owner, its zone and its printing.
    remembered :: Map.Map ObjectId.ObjectId (PlayerId.PlayerId, Zone.Zone, Printing.Printing),
    -- | By owner, the printings it knows among that owner's hand and library
    -- without knowing which object holds them (CR 701.24a).
    loose :: Map.Map PlayerId.PlayerId (Map.Map Printing.Printing Natural),
    -- | How many entries of the whole log it has read.
    logged :: Int,
    -- | The last of them.
    lastRead :: Maybe LoggedEvent.LoggedEvent
  }
  deriving (Eq, Show)

-- | Remembering nothing, the log unread.
blank :: Memory
blank = MkMemory {remembered = Map.empty, loose = Map.empty, logged = 0, lastRead = Nothing}

-- | The game's whole log, oldest first: every finished turn's
-- (History.pastTurns), then this one's.
logOf :: GameState.GameState -> Seq.Seq LoggedEvent.LoggedEvent
logOf gs = foldMap PastTurn.events (History.pastTurns (GameState.history gs)) <> TurnRegisters.events (GameState.turn gs)

-- | `memory` brought up to `gs` for `viewer`, reading the log past what it
-- read, or all of it from 'blank' once the log no longer extends what it read
-- (a restart, a subgame, a rollback): each move of a card remembered, or seen
-- (Visibility.witnessed), remembers the object it became; each
-- shuffle loosens the cards remembered in the shuffled library; then every
-- card `viewer` sees is remembered, and a card remembered last in a hand or
-- library that is gone is loosened. A card seen leaving a hand or library, or
-- seen in one, not remembered before takes one of its printing off its
-- owner's loose ones, as it may be that one, so none counts twice.
-- Parun.SearchSpec's bounce and shuffle cases are the proof.
remember :: PlayerId.PlayerId -> GameState.GameState -> Memory -> Memory
remember viewer gs memory
  | logged memory > 0 && Seq.lookup (logged memory - 1) whole /= lastRead memory = remember viewer gs blank
  | otherwise =
      let unread = Seq.drop (logged memory) whole
          witnessed entry = Visibility.witnessed viewer entry == Visibility.Type.Visible
          reading m entry = case LoggedEvent.event entry of
            GameEvent.Moved moved -> follow (witnessed entry) (Moved.change moved) m
            GameEvent.PlayerActed acted | PlayerActed.action acted == PlayerAction.Shuffle -> shuffled (PlayerActed.player acted) m
            event
              | witnessed entry,
                Just oid <- Event.shownCard event,
                Map.notMember oid (remembered m),
                Just object <- Engine.Game.lookupObject oid gs,
                Just printing <- printingOf object gs ->
                  (if Engine.Game.isHiddenZone (Object.zone object) then unloose (Object.owner object) printing else id)
                    m {remembered = Map.insert oid (Object.owner object, Object.zone object, printing) (remembered m)}
            _ -> m
          follow sawIt change m =
            let arrived = ZoneChange.object change
                to = ZoneChange.to change
             in case Map.lookup (ZoneChange.departed change) (remembered m) of
                  Just (owner, _, printing) -> m {remembered = Map.insert arrived (owner, to, printing) (Map.delete (ZoneChange.departed change) (remembered m))}
                  Nothing
                    | sawIt,
                      Just object <- Engine.Game.lookupObject arrived gs,
                      Just printing <- printingOf object gs ->
                        (if Engine.Game.isHiddenZone (ZoneChange.from change) then unloose (Object.owner object) printing else id)
                          m {remembered = Map.insert arrived (Object.owner object, to, printing) (remembered m)}
                  Nothing -> m
          shuffled pid m =
            let (mixed, kept) = Map.partition (\(owner, zone, _) -> owner == pid && zone == Zone.Library) (remembered m)
             in Foldable.foldl' (\m' (owner, _, printing) -> loosen owner printing m') m {remembered = kept} mixed
          loosen owner printing m = m {loose = Map.insertWith (Map.unionWith (+)) owner (Map.singleton printing 1) (loose m)}
          unloose owner printing m = m {loose = Map.update (\counts -> let less = Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) printing counts in if Map.null less then Nothing else Just less) owner (loose m)}
          followed = Foldable.foldl' reading memory unread
          (present, gone) = Map.partitionWithKey (\oid _ -> Map.member oid (GameState.objects gs)) (remembered followed)
          loosened = Foldable.foldl' (\m (owner, zone, printing) -> if Engine.Game.isHiddenZone zone then loosen owner printing m else m) followed {remembered = present} gone
          sighted =
            [ (oid, (Object.owner object, Object.zone object, printing))
            | (oid, object) <- Map.toList (GameState.objects gs),
              Maybe.isJust (Engine.Game.zoneOf oid gs),
              Visibility.sees viewer oid gs == Visibility.Type.Visible,
              printing <- Foldable.toList (printingOf object gs)
            ]
          newly = [(owner, printing) | (oid, (owner, zone, printing)) <- sighted, Map.notMember oid present, Engine.Game.isHiddenZone zone]
          unloosened = Foldable.foldl' (\m (owner, printing) -> unloose owner printing m) loosened newly
       in unloosened {remembered = Map.union (Map.fromList sighted) present, logged = Seq.length whole, lastRead = Seq.lookup (Seq.length whole - 1) whole}
  where
    whole = logOf gs

-- | Brings each player's memory up to the game as each prompt is asked
-- ('remember'), through `keep`, which is given the memories before, a player
-- with none starting 'blank'.
remembering :: (Monad m) => ((Map.Map PlayerId.PlayerId Memory -> Map.Map PlayerId.PlayerId Memory) -> m ()) -> Observer.Observer (Game.Play m)
remembering keep =
  Observer.MkObserver
    { Observer.before = \asked ->
        let gs = Asked.game asked
         in Game.underneath (keep (\memories -> let brought = Map.fromList [(pid, remember pid gs (Map.findWithDefault blank pid memories)) | pid <- Map.keys (GameState.players gs)] in foldr seq brought brought)),
      Observer.after = \_ _ -> pure ()
    }

-- | A world `viewer` cannot tell from `gs`: each owner's unseen objects dealt
-- the prior's contents, one each, every id staying where it is. Following
-- Magarena's showRandomizedHiddenCards (docs/prior-art-lessons.md, 10.3).
-- SearchSpec proves the viewer's view is unchanged.
sample :: (Monad m) => Prior -> Memory -> PlayerId.PlayerId -> GameState.GameState -> State.StateT Generator.Type.Generator m GameState.GameState
sample (MkPrior prior) memory viewer gs =
  Monad.foldM
    ( \world owner -> do
        (dealing, dealt) <- prior memory viewer world owner
        pure dealing {GameState.objects = Foldable.foldl' (\objects (oid, contents) -> Map.adjust (\target -> target {Object.source = fst contents, Object.identity = snd contents}) oid objects) (GameState.objects dealing) (zip (unseen memory viewer world owner) dealt)}
    )
    gs
    (Map.keys (GameState.players gs))

-- | The search bot for its seat: each option of a decision it searches,
-- where they offer a choice ('searchable'), played out in
-- `SearchBudget.samples` worlds ('worlds'), every option in each world from
-- the same random stream, the best total taken, one of the best at random on
-- a tie, or at a 'mistaken' decision the best below it, where one is;
-- every other prompt answered by the random bot (Game.bot). It
-- searches priority decisions ('options') and the first declaration of
-- attackers ('attacks') and of blockers ('blocks'). It reads the whole game
-- only through 'sample', which deals every card its seat cannot see now and
-- does not remember by the memory `recalled` gives ('remembering'), and the
-- opponents' passes `watched` gives ('watching') only through
-- 'plausibility'.
--
-- Not implemented: searching targets, which the random bot answers (#9409).
search :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Prior -> Game.Play m [Passed] -> Game.Play m Memory -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
search registry budget weights prior watched recalled = Answerer.MkAnswerer $ \asked ->
  let best :: Game.Type.Game Result.Result -> PlayerId.PlayerId -> NonEmpty.NonEmpty a -> (a -> Pick) -> Game.Play m a
      best restart pid choices pick = do
        passes <- watched
        memory <- recalled
        deal <- worlds budget prior memory passes pid (Asked.game asked)
        rounds <- Monad.replicateM (Natural.toIntSaturating (SearchBudget.samples budget)) $ do
          world <- deal
          shared <- State.get
          traverse (\option -> State.put shared >> rollout registry budget weights restart pid (pick option) world) choices
        let scored = NonEmpty.zip (foldr (NonEmpty.zipWith (+)) (0 <$ choices) rounds) choices
            top = maximum (fmap fst scored)
        slipped <- mistaken budget
        let aim = if slipped then maybe top maximum (NonEmpty.nonEmpty [total | (total, _) <- NonEmpty.toList scored, total < top]) else top
        maybe (pure (NonEmpty.head choices)) Generator.pick (NonEmpty.nonEmpty [option | (total, option) <- NonEmpty.toList scored, total == aim])
   in case Asked.prompt asked of
        Prompt.ChooseAction decider pid menu
          | searchable asked decider pid (options menu),
            Just choices <- NonEmpty.nonEmpty (options menu) ->
              best Engine.resumePriority pid choices (acting pid)
        Prompt.DeclareAttackers decider pid offered
          | searchable asked decider pid (attacks offered),
            Just choices <- NonEmpty.nonEmpty (attacks offered) ->
              best Engine.resumeTurnBasedAction pid choices (attacking pid)
        Prompt.DeclareBlockers decider pid candidates attacked
          | firstBlocks pid (Asked.game asked) && searchable asked decider pid (blocks candidates attacked),
            Just choices <- NonEmpty.nonEmpty (blocks candidates attacked) ->
              best Engine.resumeTurnBasedAction pid choices (blocking pid)
        _ -> Answerer.unwrap (Game.bot registry) asked

-- | Whether a searched decision is a mistake, at the budget's
-- `SearchBudget.mistakes` percent: drawn once its playouts are, and only at
-- a rate above 0, so a budget making none plays as it did without the field.
mistaken :: (Monad m) => SearchBudget.SearchBudget -> State.StateT Generator.Type.Generator m Bool
mistaken budget
  | SearchBudget.mistakes budget == 0 = pure False
  | otherwise = fmap (< SearchBudget.mistakes budget) (Generator.between 0 99)

-- | A priority decision a player passed at (CR 117.3d): the player and
-- the game as they were asked.
type Passed = (PlayerId.PlayerId, GameState.GameState)

-- | Keeps each 'restartable' priority decision a player passes at, however
-- many options its player saw, through `keep`, which is given the passes
-- kept before and returns them less every one 'stale' now, the new one after
-- them, its spine forced once the result is, so no stale game is held.
--
-- Not implemented: weighing a play other than a pass, or a declaration of
-- attackers or blockers (#9448).
watching :: (Monad m) => (([Passed] -> [Passed]) -> m ()) -> Observer.Observer (Game.Play m)
watching keep =
  Observer.MkObserver
    { Observer.before = const (pure ()),
      Observer.after = \asked answer -> case Asked.prompt asked of
        Prompt.ChooseAction decider pid _
          | answer == Action.Pass && restartable asked decider pid ->
              Game.underneath (keep (\passes -> let kept = filter (not . stale (Asked.game asked)) passes <> [(pid, Asked.game asked)] in length kept `seq` kept))
        _ -> pure ()
    }

-- | Whether a pass is no longer weighed in `gs`: made before the turn
-- before `gs`'s, which keeps the weighing to a reduced budget, or none of the
-- cards in the passer's hand then is there now, so it says nothing of it.
stale :: GameState.GameState -> Passed -> Bool
stale gs (pid, past) =
  let handIn g = Map.findWithDefault mempty pid (GameState.hand g)
      turnOf g = TurnRegisters.turnNumber (GameState.turn g)
   in turnOf past + 1 < turnOf gs || not (any (`elem` handIn gs) (handIn past))

-- | How `viewer`, remembering `memory`, deals each playout's world from
-- `gs`: with no `SearchBudget.candidates`, a fresh 'sample' each; otherwise
-- that many samples drawn once, then each playout's drawn among them weighted
-- by its 'plausibility' given the opponents' `passes`, evenly when every
-- weight is 0.
worlds :: (Monad m) => SearchBudget.SearchBudget -> Prior -> Memory -> [Passed] -> PlayerId.PlayerId -> GameState.GameState -> Game.Play m (Game.Play m GameState.GameState)
worlds budget prior memory passes viewer gs
  | SearchBudget.candidates budget == 0 = pure (sample prior memory viewer gs)
  | otherwise = do
      drawn <- Monad.replicateM (Natural.toIntSaturating (SearchBudget.candidates budget)) (sample prior memory viewer gs)
      weighed <- traverse (\world -> fmap (\w -> (w, world)) (plausibility prior memory viewer passes gs world)) drawn
      pure $ case NonEmpty.nonEmpty weighed of
        Nothing -> sample prior memory viewer gs
        Just pool
          | all ((== 0) . fst) pool -> Generator.pick (fmap snd pool)
          | otherwise -> Generator.weighted pool

-- | How likely the opponents of `viewer`, remembering `memory`, were to make
-- their `passes` not 'stale' had `gs`'s unseen cards been `world`'s, each
-- pass scored as the random bot choosing among the options searched
-- ('options'), the opponent model: one over their number in the game as it
-- was, its cards `viewer` could not see then dealt anew ('sample') save those
-- it still cannot see now, which hold `world`'s. A pass none of whose
-- then-unseen cards in the passer's hand is still unseen counts 1, as does
-- one whose restarted menu is not asked first (Engine.resumePriority).
plausibility :: (Monad m) => Prior -> Memory -> PlayerId.PlayerId -> [Passed] -> GameState.GameState -> GameState.GameState -> Game.Play m Rational
plausibility prior memory viewer passes gs world =
  fmap product . Monad.forM [(pid, past) | (pid, past) <- passes, pid /= viewer, not (stale gs (pid, past))] $ \(pid, past) -> do
    let hiddenNow = Set.fromList (unseen memory viewer gs pid)
        shared = filter (`Set.member` hiddenNow) (unseen memory viewer past pid)
        heldThen = Map.findWithDefault mempty pid (GameState.hand past)
    if not (any (`elem` heldThen) shared)
      then pure 1
      else do
        redealt <- sample prior memory viewer past
        let carried = Foldable.foldl' (carry world) redealt shared
        pure $ case menuAgain pid carried of
          Just menu | n <- length (options menu), n > 0, elem Action.Pass menu -> 1 / toRational n
          _ -> 1

-- | `target` with object `oid` holding what it holds in `from`, its printing
-- interned in `target`.
carry :: GameState.GameState -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
carry from target oid = case Engine.Game.lookupObject oid from of
  Nothing -> target
  Just object ->
    let (source, interned) = case Object.source object of
          Source.OfCard pid | Just printing <- Engine.Game.printingOf pid from -> let (pid', g) = Engine.Game.intern printing target in (Source.OfCard pid', g)
          other -> (other, target)
     in interned {GameState.objects = Map.adjust (\o -> o {Object.source = source, Object.identity = Object.identity object}) oid (GameState.objects interned)}

-- | The menu of `pid`'s priority decision restarted in `gs`
-- (Engine.resumePriority), every concession prompt before it answered
-- Continues; Nothing if anything else is asked first or play ends.
menuAgain :: PlayerId.PlayerId -> GameState.GameState -> Maybe [Action.Action]
menuAgain pid gs =
  let go :: Program.Program (Asked.Asked GameState.GameState) a -> Maybe [Action.Action]
      go program = case program of
        Program.Then asked k -> case Asked.prompt asked of
          Prompt.Concede _ -> go (k Concession.Continues)
          Prompt.ChooseAction _ holder menu | holder == pid -> Just menu
          _ -> Nothing
        Program.Return _ -> Nothing
   in go (State.runStateT Engine.resumePriority gs)

-- | The answer a playout gives the first prompt it is asked that it fits:
-- the searched option, given to the decision restarted.
newtype Pick = MkPick (forall r. Prompt.Prompt r -> Maybe r)

-- | `pid`'s priority decision answered `action`.
acting :: PlayerId.PlayerId -> Action.Action -> Pick
acting pid action = MkPick $ \prompt -> case prompt of
  Prompt.ChooseAction _ holder _ | holder == pid -> Just action
  _ -> Nothing

-- | `pid`'s declaration of attackers answered `declared`.
attacking :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Pick
attacking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareAttackers _ declarer _ | declarer == pid -> Just declared
  _ -> Nothing

-- | `pid`'s declaration of blockers answered `declared`.
blocking :: PlayerId.PlayerId -> Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId) -> Pick
blocking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareBlockers _ declarer _ _ | declarer == pid -> Just declared
  _ -> Nothing

-- | The actions of a priority menu worth searching: all but CR 605.3a's mana
-- abilities activated at priority, which a payment activates as it needs
-- (CR 601.2g), and life paid for mana at priority (Action.PayLifeForMana).
options :: [Action.Action] -> [Action.Action]
options = filter $ \action -> case action of
  Action.ActivateManaAbility {} -> False
  Action.PayLifeForMana {} -> False
  _ -> True

-- | The declarations of attackers searched, from the creatures offered (CR
-- 508.1a): every subset of up to 'widest' declarations, else none, all, each
-- creature alone and all but each. The engine judges each (CR 508.1c, 508.1d).
attacks :: [ObjectId.ObjectId] -> [[ObjectId.ObjectId]]
attacks offered
  | 2 ^ length offered <= widest = List.subsequences offered
  | otherwise = ListUtils.nubOrd ([[], offered] <> fmap pure offered <> [List.delete oid offered | oid <- offered])

-- | The declarations of blockers searched, from the creatures offered and
-- the attackers they may block (CR 509.1a): every one blocking at most one
-- attacker each, where there are up to 'widest', else none and each blocker
-- alone on each attacker. The engine judges each (CR 509.1b, 509.1c).
blocks :: [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> [Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId)]
blocks candidates attacked
  | (length attacked + 1) ^ length candidates <= widest = fmap (Map.fromList . Maybe.catMaybes) (traverse (\blocker -> Nothing : [Just (blocker, Set.singleton attacker) | attacker <- attacked]) candidates)
  | otherwise = Map.empty : [Map.singleton blocker (Set.singleton attacker) | blocker <- candidates, attacker <- attacked]

-- | The most declarations 'attacks' and 'blocks' search exhaustively.
widest :: Int
widest = 16

-- | Whether `pid` makes the first declaration of blockers, the only one
-- Engine.resumeTurnBasedAction restarts: the first defending player's, from
-- no piles (CR 509.1, Combat.declareBlockers).
firstBlocks :: PlayerId.PlayerId -> GameState.GameState -> Bool
firstBlocks pid gs =
  let combat = TurnRegisters.combat (GameState.turn gs)
   in fmap (Engine.Game.primaryOf gs) (Maybe.listToMaybe (Combat.defenders combat)) == Just pid && Maybe.isNothing (Combat.pileBlocks combat)

-- | Whether a decision is searched: a choice among two or more at a
-- 'restartable' decision.
searchable :: Asked.Asked GameState.GameState r -> Decider.Decider -> PlayerId.PlayerId -> [a] -> Bool
searchable asked decider pid choices = length (take 2 choices) == 2 && restartable asked decider pid

-- | Whether a decision can be asked again: the player deciding for
-- themselves (CR 724.1), in a step of the main game, where
-- Engine.resumePriority or Engine.resumeTurnBasedAction restarts it.
restartable :: Asked.Asked GameState.GameState r -> Decider.Decider -> PlayerId.PlayerId -> Bool
restartable asked decider pid =
  let gs = Asked.game asked
   in Decider.unwrap decider == pid
        && null (Asked.enclosing asked)
        && null (GameState.turnMarkers gs)
        && Map.null (GameState.parkedTurns gs)

-- | One playout for `pid` from a world at a decision, `restart` asking it
-- again and `pick` answering it, every seat answered by the budget's
-- 'Playout.Playout', scored 'won' a win, minus it a loss, 0 a
-- draw; a playout cut off once it begins `SearchBudget.turns` turns after the
-- decision's, or after `SearchBudget.prompts` prompts, is scored by
-- 'evaluate' at the prompt it stopped at, short of a win or a loss.
rollout :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> Pick -> GameState.GameState -> Game.Play m Integer
rollout registry budget weights restart pid (MkPick pick) world =
  let policy = case SearchBudget.playout budget of
        Playout.Random -> Game.bot registry
        Playout.Greedy -> greedy registry
      seats = Game.consulting registry (Map.fromList [(seat, policy) | seat <- Map.keys (GameState.players world)])
      final = TurnRegisters.turnNumber (GameState.turn world) + SearchBudget.turns budget
      go :: Natural -> Bool -> Program.Program (Asked.Asked GameState.GameState) (Result.Result, GameState.GameState) -> Game.Play m Integer
      go left pending program = case program of
        Program.Return (result, _) -> pure (score pid result)
        Program.Then asked k
          | left == 0 || TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) > final -> pure (max (1 - won) (min (won - 1) (evaluate weights pid (Asked.game asked))))
          | pending, Just answer <- pick (Asked.prompt asked) -> go (left - 1) False (k answer)
          | otherwise -> Answerer.unwrap seats asked >>= go (left - 1) pending . k
   in go (SearchBudget.prompts budget) True (State.runStateT restart world)

-- | A result for `pid`.
score :: PlayerId.PlayerId -> Result.Result -> Integer
score pid result = case result of
  Result.Won winner | winner == pid -> won
  Result.Drawn -> 0
  _ -> negate won

-- | A win's score, above any cut-off playout's.
won :: Integer
won = 1000

-- | A position for `pid`: their 'features' weighted by `Weights.ours`, less
-- the highest of every other player's weighted by `Weights.theirs`.
evaluate :: Weights.Weights -> PlayerId.PlayerId -> GameState.GameState -> Integer
evaluate weights pid gs =
  let board = Projection.projectAll gs
      grants = Projection.View.controlGrants gs
      weighed side player = dot (side weights) (features board grants player gs)
   in weighed Weights.ours pid - maybe 0 maximum (NonEmpty.nonEmpty [weighed Weights.theirs other | other <- Map.keys (GameState.players gs), other /= pid])

-- | The weighted sum of a player's features.
dot :: Features.Features -> Features.Features -> Integer
dot weight count =
  sum [field weight * field count | field <- [Features.life, Features.hand, Features.power, Features.toughness, Features.lands, Features.permanents]]

-- | `pid`'s generic features, from the board projected and the control
-- grants of `gs`: what every player may see or count (CR 400.2, 402.3), and
-- no effect's identity.
features :: Map.Map ObjectId.ObjectId ProjectedCharacteristics.ProjectedCharacteristics -> [Projection.View.ControlGrant] -> PlayerId.PlayerId -> GameState.GameState -> Features.Features
features board grants pid gs =
  let controlled = [characteristics | (oid, characteristics) <- Map.toList board, Projection.View.controllerOfGiven grants oid gs == Just pid]
      typed cardType = filter (Set.member cardType . ProjectedCharacteristics.cardTypes) controlled
      total stat = sum (fmap (max 0 . Maybe.fromMaybe 0 . stat) (typed CardType.Creature))
   in Features.MkFeatures
        { Features.life = maybe 0 Player.life (Map.lookup pid (GameState.players gs)),
          Features.hand = toInteger (length (Map.findWithDefault mempty pid (GameState.hand gs))),
          Features.power = total ProjectedCharacteristics.power,
          Features.toughness = total ProjectedCharacteristics.toughness,
          Features.lands = toInteger (length (typed CardType.Land)),
          Features.permanents = toInteger (length controlled)
        }

-- | The greedy playout policy, cheap enough to answer every prompt of every
-- playout: a land played where one may be, else a permanent spell cast, else
-- another action or a pass ('greedyAction'). Every other prompt is the random
-- bot's (Game.bot). It reads only its own menu and the card types of the
-- cards on it.
--
-- Not implemented: choosing targets, modes and payments by their effect's
-- classification; the random bot answers them (#9442).
greedy :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
greedy registry = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction _ _ menu -> greedyAction (Asked.game asked) menu
  _ -> Answerer.unwrap (Game.bot registry) asked

-- | A priority decision: a land play (CR 305.1) at random where the menu
-- offers one, else a cast (CR 601) of a card with a permanent type (CR
-- 110.4), else one of the other 'options' or a pass, each equally likely.
-- The menu offers only what may be afforded (Engine.Cast.castableSpells).
-- Another spell waits for the random pick, since the random bot chooses its
-- targets (#9442) and casting it at the first chance would hit either side
-- alike.
greedyAction :: (Monad m) => GameState.GameState -> [Action.Action] -> State.StateT Generator.Type.Generator m Action.Action
greedyAction gs menu =
  let plays = [action | action@Action.Play {} <- menu]
      permanent oid = any Engine.Card.isPermanentType (ProjectedCharacteristics.cardTypes (Projection.project oid gs))
      casts = [action | action@(Action.Cast oid _ _) <- menu, permanent oid]
      rest = [action | action <- options menu, notElem action (Action.Pass : plays <> casts)]
   in case (NonEmpty.nonEmpty plays, NonEmpty.nonEmpty casts) of
        (Just some, _) -> Generator.pick some
        (_, Just some) -> Generator.pick some
        _ -> Generator.pick (Action.Pass NonEmpty.:| rest)
