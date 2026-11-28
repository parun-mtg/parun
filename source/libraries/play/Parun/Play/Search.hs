{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The search bot: Monte Carlo search over a seat's priority and combat
-- decisions, flat or by an information-set tree, in worlds dealt
-- consistently with what the seat has seen, the budget's playout policy in
-- every seat.
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
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Card as Engine.Card
import qualified Parun.Engine.Cost as Engine.Cost
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.Mana as Engine.Mana
import qualified Parun.Engine.Modal as Engine.Modal
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Target as Target
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Polarity as Polarity
import qualified Parun.Registry as Registry
import qualified Parun.Types.Action as Action
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.Combat as Combat
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Features as Features
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.History as History
import qualified Parun.Types.InherentTriggerSource as InherentTriggerSource
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordInstance as KeywordInstance
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PastTurn as PastTurn
import qualified Parun.Types.PayBranch as PayBranch
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Polarity as Polarity.Type
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Program as Program
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Source as Source
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Weights as Weights
import qualified Parun.Types.XDecision as XDecision
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

-- | The search bot for its seat: each decision it searches, where it offers
-- a choice ('searchable'), searched by the budget's 'SearchBudget.method'
-- ('flat' or 'informationSet') over worlds dealt by 'worlds', at a
-- 'mistaken' decision the best below the best taken, where one is; every other
-- prompt answered by the random bot (Game.bot). It searches priority
-- decisions ('options'), each action beside each 'Line' of answers to the
-- CR 601.2b announcements, announced numbers of targets and first choice of
-- targets it leads to ('aimed', 'lined'), and the first declaration of
-- attackers ('attacks') and of blockers ('blocks'). The line of the action
-- taken is kept in `aim` and answers those prompts where it still may, past
-- each step that lapses ('stepping'). A priority decision it does not search
-- is the random bot's over the 'options' alone; a payment taps a source
-- 'paymentSource' finds needed and nothing past what covers the cost. Past
-- its own mana sources and pool there, it reads the whole game only through
-- 'sample', which deals
-- every card its seat cannot see now and does not remember by the memory
-- `recalled` gives ('remembering'), and the opponents' plays `watched` gives
-- ('watching') only through 'plausibility'. Each prompt a playout answers
-- waits on `breathe` first, which lets the caller's other work run. Each
-- searched decision starts `clock`, and stops playing out once it runs out
-- ('spending').
--
-- Not implemented: searching a line past a buyback, entwine, splice or
-- assist announcement (CR 601.2b, 702.132a), more than 'widest' lines, or any
-- choice of targets but the first an action leads to; the random bot answers
-- them (#9488).
search :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Prior -> Game.Play m [Watched] -> Game.Play m Memory -> Aim m -> Game.Play m () -> Clock m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
search registry budget weights prior watched recalled aim breathe clock = Answerer.MkAnswerer $ \asked ->
  let dealing pid = do
        plays <- watched
        memory <- recalled
        worlds budget prior memory plays pid (Asked.game asked)
      best :: Game.Type.Game Result.Result -> PlayerId.PlayerId -> Game.Play m GameState.GameState -> NonEmpty.NonEmpty a -> (a -> Move) -> Game.Play m a
      best restart pid deal choices move = do
        expired <- clock
        case SearchBudget.method budget of
          SearchMethod.Flat -> flat registry budget weights breathe expired restart pid deal choices move
          SearchMethod.InformationSet -> informationSet registry budget weights breathe expired restart pid (Asked.game asked) deal choices move
      unaimed = hold aim Nothing >> Answerer.unwrap (Game.bot registry) asked
      kept pid = do
        line <- held aim
        let fit steps = case steps of
              step : rest | MkPick pick <- stepping pid step -> case pick (Asked.prompt asked) of
                Fits answer -> Just (answer, rest)
                _ -> fit rest
              [] -> Nothing
        case fit (foldMap NonEmpty.toList line) of
          Just (answer, rest) -> answer <$ hold aim (NonEmpty.nonEmpty rest)
          Nothing -> unaimed
   in case Asked.prompt asked of
        Prompt.ChooseAction decider pid menu
          | searchable asked decider pid (options menu),
            Just actions <- NonEmpty.nonEmpty (options menu) -> do
              deal <- dealing pid
              probe <- deal
              (action, line) <- best Engine.resumePriority pid deal (actions >>= aimed pid probe) (\(a, l) -> maybe (Acted a) (Aimed a) l)
              action <$ hold aim line
          | otherwise -> hold aim Nothing >> Answerer.unwrap (Game.bot registry) asked {Asked.prompt = Prompt.ChooseAction decider pid (options menu)}
        prompt | Just (decider, pid, _) <- lined prompt, Decider.unwrap decider == pid -> kept pid
        Prompt.DeclareAttackers decider pid offered
          | searchable asked decider pid (attacks offered),
            Just choices <- NonEmpty.nonEmpty (attacks offered) -> do
              deal <- dealing pid
              best Engine.resumeTurnBasedAction pid deal choices Attacked
        Prompt.DeclareBlockers decider pid candidates attacked
          | firstBlocks pid (Asked.game asked) && searchable asked decider pid (blocks candidates attacked),
            Just choices <- NonEmpty.nonEmpty (blocks candidates attacked) -> do
              deal <- dealing pid
              best Engine.resumeTurnBasedAction pid deal choices Blocked
        Prompt.ChooseManaSource _ pid cost sources _
          | Just source <- paymentSource pid cost sources (Asked.game asked) -> pure (Just source)
        Prompt.ChooseExtraManaSource {} -> pure Nothing
        _ -> Answerer.unwrap (Game.bot registry) asked

-- | What a choice of targets takes: each slot's targets (CR 601.2c).
type Targets = Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)

-- | An answer the search bot keeps for a prompt its priority action leads to.
data Step
  = -- | The modes chosen (CR 601.2b, 700.2).
    Moded (Seq.Seq ModeIndex.ModeIndex)
  | -- | The cost the spell is cast for (CR 601.2b, 118.9a).
    Costed CandidateCost.CandidateCost
  | -- | The times an optional additional cost of that keyword's instance is
    -- paid (CR 601.2b, 702.33a).
    Kicked Keyword.Keyword (Maybe KeywordInstance.KeywordInstance) KickerDecision.KickerDecision
  | -- | The value announced for that variable (CR 601.2b, 107.3a).
    Valued SlotName.SlotName Natural
  | -- | Each slot's announced number of targets (CR 601.2c).
    Announced (Map.Map SlotName.SlotName Natural)
  | -- | The targets chosen (CR 601.2c).
    Targeted Targets
  deriving (Eq, Ord, Show)

-- | The answers kept for the prompts a priority action leads to, in order.
type Line = NonEmpty.NonEmpty Step

-- | Where the search bot keeps the line it chose beside its last priority
-- action, for the prompts that action leads to.
data Aim m = MkAim
  { -- | The line kept, if any.
    held :: Game.Play m (Maybe Line),
    -- | Keeps this line, or none.
    hold :: Maybe Line -> Game.Play m ()
  }

-- | Keeping nothing: every prompt a line answers the random bot's.
unkept :: (Monad m) => Aim m
unkept = MkAim {held = pure Nothing, hold = const (pure ())}

-- | `pid`'s line kept among every seat's, read by `kept` and changed through
-- `keep`.
keptBy :: (Monad m) => m (Map.Map PlayerId.PlayerId Line) -> ((Map.Map PlayerId.PlayerId Line -> Map.Map PlayerId.PlayerId Line) -> m ()) -> PlayerId.PlayerId -> Aim m
keptBy kept keep pid = MkAim {held = Game.underneath (fmap (Map.lookup pid) kept), hold = Game.underneath . keep . (`Map.alter` pid) . const}

-- | `pid`'s priority action searched: beside each 'Line' it leads to in
-- `world` ('linesAfter'), where there are two to 'widest', else alone.
aimed :: PlayerId.PlayerId -> GameState.GameState -> Action.Action -> NonEmpty.NonEmpty (Action.Action, Maybe Line)
aimed pid world action = case Maybe.mapMaybe NonEmpty.nonEmpty (take (widest + 1) (linesAfter pid action world)) of
  first : second : rest | length rest + 2 <= widest -> fmap ((,) action . Just) (first NonEmpty.:| second : rest)
  _ -> pure (action, Nothing)

-- | Every line of answers `pid` gives once taking `action` at the priority
-- decision restarted in `world` (Engine.resumePriority), a spell's or an
-- ability's (CR 601.2b, 601.2c, 602.2b, 603.3d): each answer to each prompt
-- 'lined' offers that `pid` decides for themselves, every concession prompt
-- answered Continues, a line ending at its first choice of targets or at any
-- other prompt, a prompt offering none ending it there.
linesAfter :: PlayerId.PlayerId -> Action.Action -> GameState.GameState -> [[Step]]
linesAfter pid action world =
  let go :: Bool -> [Step] -> Program.Program (Asked.Asked GameState.GameState) a -> [[Step]]
      go acted taken program = case program of
        Program.Then asked k -> case Asked.prompt asked of
          Prompt.Concede _ -> go acted taken (k Concession.Continues)
          Prompt.ChooseAction _ holder _ | holder == pid && not acted -> go True taken (k action)
          prompt
            | acted,
              Just (decider, chooser, MkLined offers _ ends) <- lined prompt,
              chooser == pid && Decider.unwrap decider == pid,
              _ : _ <- offers ->
                if ends
                  then [reverse (step : taken) | (step, _) <- offers]
                  else foldMap (\(step, answer) -> go acted (step : taken) (k answer)) offers
          _ -> [reverse taken]
        Program.Return _ -> [reverse taken]
   in go False [] (State.runStateT Engine.resumePriority world)

-- | How a line answers a prompt: every answer it may give, as the random bot
-- gives them (Bot.bot), each beside its step; the answer a kept step gives it,
-- where the step may; and whether the line ends there.
data Lined r = MkLined [(Step, r)] (Step -> Maybe r) Bool

-- | The prompt's decider, chooser and how a line answers it, where a line
-- does: a modal choice (Engine.Modal.selections), a cost to cast for, an
-- optional additional cost's times paid, a variable's value (CR 601.2b), an
-- announcement of numbers of targets ('announcements', 'announcing') and a
-- choice of targets, which ends it ('aims', 'fitting'; CR 601.2c).
lined :: Prompt.Prompt r -> Maybe (Decider.Decider, PlayerId.PlayerId, Lined r)
lined prompt = case prompt of
  Prompt.ChooseModes decider chooser _ legal selection ->
    let picks = Engine.Modal.selections legal selection
     in Just (decider, chooser, MkLined [(Moded modes, modes) | modes <- picks] (\step -> case step of Moded modes | elem modes picks -> Just modes; _ -> Nothing) False)
  Prompt.ChooseKicker decider chooser _ keyword which limit ->
    let paid = fmap KickerDecision.Pays [0 .. Maybe.fromMaybe 1 limit]
     in Just (decider, chooser, MkLined [(Kicked keyword which times, times) | times <- paid] (\step -> case step of Kicked k i (KickerDecision.Pays times) | k == keyword && i == which && maybe True (times <=) limit -> Just (KickerDecision.Pays times); _ -> Nothing) False)
  Prompt.ChooseCost decider chooser _ candidates _ _ ->
    Just (decider, chooser, MkLined [(Costed candidate, candidate) | candidate <- candidates] (\step -> case step of Costed candidate | elem candidate candidates -> Just candidate; _ -> Nothing) False)
  Prompt.ChooseX decider chooser _ slot least greatest ->
    Just (decider, chooser, MkLined [(Valued slot value, XDecision.Announces value) | value <- [least .. greatest]] (\step -> case step of Valued named value | named == slot && least <= value && value <= greatest -> Just (XDecision.Announces value); _ -> Nothing) False)
  Prompt.AnnounceTargets decider chooser _ offers ->
    Just (decider, chooser, MkLined [(Announced counts, counts) | counts <- announcements offers] (\step -> case step of Announced counts | announcing offers counts -> Just counts; _ -> Nothing) False)
  Prompt.ChooseTargets decider chooser _ offered ->
    Just (decider, chooser, MkLined [(Targeted targets, targets) | targets <- aims offered] (\step -> case step of Targeted targets | fitting offered targets -> Just targets; _ -> Nothing) True)
  _ -> Nothing

-- | Every announcement of these slots' numbers of targets, each slot's from
-- its least through what its legal recipients allow (TargetCount.ceilingOn),
-- as the random bot announces them (Bot.bot).
announcements :: Map.Map SlotName.SlotName (TargetCount.TargetCount, Set.Set Recipient.Recipient) -> [Map.Map SlotName.SlotName Natural]
announcements = traverse (\(count, legal) -> [TargetCount.least count .. TargetCount.ceilingOn (Natural.length legal) count])

-- | Whether a kept announcement may answer an announcement of numbers of
-- targets: each slot it offers given a number in its range, and no other.
announcing :: Map.Map SlotName.SlotName (TargetCount.TargetCount, Set.Set Recipient.Recipient) -> Map.Map SlotName.SlotName Natural -> Bool
announcing offers counts =
  Map.keysSet offers == Map.keysSet counts
    && and (Map.intersectionWith (\(count, legal) n -> TargetCount.least count <= n && n <= TargetCount.ceilingOn (Natural.length legal) count) offers counts)

-- | Every choice of targets for these slots, each slot its count of its legal
-- recipients or all of them where it is offered fewer, as the random bot
-- takes them (Bot.bot); none where there are more than 'widest'. The engine
-- judges each (CR 601.2e).
aims :: Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient) -> [Targets]
aims offered =
  let each = fmap (\(count, legal) -> fmap Set.fromList (combinations count (Set.toList legal))) offered
   in if all (\choices -> length (take (widest + 1) choices) <= widest) each && product (fmap length each) <= widest then sequenceA each else []

-- | Every choice of `count` of the items in order, or of all of them where
-- there are fewer.
combinations :: Natural -> [a] -> [[a]]
combinations count items
  | count == 0 = [[]]
  | Natural.length items <= count = [items]
  | otherwise = case items of
      [] -> [[]]
      item : rest -> fmap (item :) (combinations (count - 1) rest) <> combinations count rest

-- | Whether targets kept may answer a choice of targets: each slot it offers
-- given its count of its legal recipients, or all of them where it offers
-- fewer, and no other slot.
fitting :: Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient) -> Targets -> Bool
fitting offered chosen =
  Map.keysSet offered == Map.keysSet chosen
    && and (Map.intersectionWith (\(count, legal) taken -> Set.isSubsetOf taken legal && Natural.length taken == min count (Natural.length legal)) offered chosen)

-- | Starts a searched decision's clock, giving whether its time has run out.
type Clock m = Game.Play m (Game.Play m Bool)

-- | A clock that never runs out: every decision spends its whole budget, so
-- a seeded game plays alike however fast it is played.
unhurried :: (Monad m) => Clock m
unhurried = pure (pure False)

-- | `samples` rounds, each in a world `deal` deals playing each of `choices`
-- out from the same random stream, folded by `play` from `start`; cut short
-- once `expired` says so, asked before every playout but the first.
spending :: (Monad m) => Game.Play m Bool -> Natural -> Game.Play m GameState.GameState -> (GameState.GameState -> s -> a -> Game.Play m s) -> s -> [a] -> Game.Play m s
spending expired samples deal play start choices = rounds samples False start
  where
    rounds left asking folded
      | left == 0 = pure folded
      | otherwise = do
          over <- if asking then expired else pure False
          if over
            then pure folded
            else do
              world <- deal
              shared <- State.get
              let each acc rest = case rest of
                    [] -> rounds (left - 1) True acc
                    option : later -> do
                      State.put shared
                      played <- play world acc option
                      stop <- if null later then pure False else expired
                      if stop then pure played else each played later
              each folded choices

-- | The choices scored, less every one never scored where one was.
scoredOnly :: NonEmpty.NonEmpty (Maybe s, a) -> NonEmpty.NonEmpty (Maybe s, a)
scoredOnly scored = Maybe.fromMaybe scored (NonEmpty.nonEmpty (NonEmpty.filter (Maybe.isJust . fst) scored))

-- | Flat Monte Carlo: each of `pid`'s choices played out ('rollout') in
-- `SearchBudget.samples` worlds `deal` deals, every option in each world from
-- the same random stream, until `expired` ('spending'); the best average
-- taken among those played out ('slipping').
flat :: (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Play m () -> Game.Play m Bool -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> Game.Play m GameState.GameState -> NonEmpty.NonEmpty a -> (a -> Move) -> Game.Play m a
flat registry budget weights breathe expired restart pid deal choices move = do
  let indexed = NonEmpty.zip (NonEmpty.iterate (+ 1) (0 :: Int)) choices
      play world tallies (index, option) = do
        scored <- rollout registry budget weights breathe restart pid (picking pid (move option)) world
        pure (Map.insertWith (\(total, tries) (total', tries') -> (total + total', tries + tries')) index (scored, 1 :: Integer) tallies)
  tallies <- spending expired (SearchBudget.samples budget) deal play Map.empty (NonEmpty.toList indexed)
  let averaged index = (\(total, tries) -> toRational total / toRational tries) <$> Map.lookup index tallies
  slipping budget (idle . move) (scoredOnly (fmap (Bifunctor.first averaged) indexed))

-- | Single-observer information-set Monte Carlo tree search (Cowling, Powley
-- & Whitehouse 2012, "Information Set Monte Carlo Tree Search", SO-ISMCTS):
-- in each of `SearchBudget.samples` worlds `deal` deals, an iteration for
-- each of `viewer`'s choices, all from the same random stream as 'flat'
-- plays its options and until `expired` ('spending'), each descending one
-- 'Node' tree shared by every world ('descend'), so a later decision is
-- valued over the worlds `viewer` cannot tell apart rather than in each as
-- if it could see it. Taken: the choice
-- whose tries scored best on average ('slipping').
-- Parun.SearchSpec's attack into a pump spell is the proof.
informationSet :: (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Play m () -> Game.Play m Bool -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> GameState.GameState -> Game.Play m GameState.GameState -> NonEmpty.NonEmpty a -> (a -> Move) -> Game.Play m a
informationSet registry budget weights breathe expired restart viewer gs deal choices move = do
  let edgeFor option = edgeOf viewer gs viewer (move option)
      forcing option = MkForced (edgeFor option) (edgeOf viewer gs viewer (opening (move option))) (drop 1 (picking viewer (move option)))
      play world grown option = do
        (path, value) <- descend registry budget weights breathe restart viewer (Just (forcing option)) grown world
        pure (backup value path grown)
  MkTree (MkNode root) _ <- spending expired (SearchBudget.samples budget) deal play (MkTree (MkNode Map.empty) Nothing) (NonEmpty.toList choices)
  let tried option = (\branch -> toRational (reward branch) / toRational (max 1 (visits branch))) <$> Map.lookup (edgeFor option) root
  slipping budget (idle . move) (scoredOnly (fmap (\option -> (tried option, option)) choices))

-- | A choice at a decision the tree holds, whatever its prompt answers.
data Move
  = -- | A priority decision's action (CR 117).
    Acted Action.Action
  | -- | A declaration of attackers (CR 508.1).
    Attacked [ObjectId.ObjectId]
  | -- | A declaration of blockers (CR 509.1).
    Blocked (Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId))
  | -- | A priority decision's action and the line of answers it leads to
    -- (CR 601.2b, 601.2c, 602.2b, 603.3d).
    Aimed Action.Action Line
  deriving (Eq, Ord, Show)

-- | The move of the decision a move begins at, as the tree holds it
-- ('decisionOf'): an 'Aimed' action's 'Acted' one.
opening :: Move -> Move
opening move = case move of
  Aimed action _ -> Acted action
  _ -> move

-- | `pid`'s picks making a move, in order: the decision it begins at
-- answered, then each step of an 'Aimed' action's line.
picking :: PlayerId.PlayerId -> Move -> [Pick]
picking pid move = case move of
  Acted action -> [acting pid action]
  Attacked declared -> [attacking pid declared]
  Blocked declared -> [blocking pid declared]
  Aimed action line -> acting pid action : fmap (stepping pid) (NonEmpty.toList line)

-- | The first decision of a descent answered: its edge in the tree, the edge
-- among the decision's legal ones that answers it, and the picks after.
data Forced = MkForced Edge Edge [Pick]

-- | What the searcher tells apart of a mover's move: the move, each object
-- it names that the searcher cannot see (CR 400.2) standing as 'unseenSlot',
-- and beside it, for each object it names in order, that one's printing if
-- it stands so. Two worlds' casts of a card from the same hidden hand tell
-- apart only by the card, not by which object held it.
data Edge = MkEdge PlayerId.PlayerId Move [Maybe Printing.Printing]
  deriving (Eq, Ord, Show)

-- | The edge `viewer` sees of `mover`'s `move` in `gs`.
edgeOf :: PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> Move -> Edge
edgeOf viewer gs mover move = case move of
  Acted action ->
    let (named, shown) = objectsIn hide action
     in MkEdge mover (Acted shown) named
  Aimed action line ->
    let (named, shown) = objectsIn hide action
     in MkEdge mover (Aimed shown line) named
  _ -> MkEdge mover move []
  where
    hide oid = case Engine.Game.lookupObject oid gs >>= (`printingOf` gs) of
      Just printing | Visibility.sees viewer oid gs /= Visibility.Type.Visible -> ([Just printing], unseenSlot)
      _ -> ([Nothing], oid)

-- | Where an 'Edge' names an object its searcher cannot see.
unseenSlot :: ObjectId.ObjectId
unseenSlot = ObjectId.MkObjectId 0

-- | Each object an action names, in order, through `f`.
objectsIn :: (Applicative f) => (ObjectId.ObjectId -> f ObjectId.ObjectId) -> Action.Action -> f Action.Action
objectsIn f action = case action of
  Action.Pass -> pure action
  Action.Play oid face -> (`Action.Play` face) <$> f oid
  Action.Cast oid face facing -> (\o -> Action.Cast o face facing) <$> f oid
  Action.Activate oid ability -> (`Action.Activate` ability) <$> f oid
  Action.ActivateManaAbility oid -> Action.ActivateManaAbility <$> f oid
  Action.TurnFaceUp oid procedure -> (`Action.TurnFaceUp` procedure) <$> f oid
  Action.TurnConspiracyFaceUp oid -> Action.TurnConspiracyFaceUp <$> f oid
  Action.Unlock oid door -> (`Action.Unlock` door) <$> f oid
  Action.DiscardFromHand oid -> Action.DiscardFromHand <$> f oid
  Action.Ignore oid ability -> (`Action.Ignore` ability) <$> f oid
  Action.Plot oid cost -> (`Action.Plot` cost) <$> f oid
  Action.Foretell oid -> Action.Foretell <$> f oid
  Action.Suspend oid -> Action.Suspend <$> f oid
  Action.PutCompanionIntoHand -> pure action
  Action.EndEffect paid ended -> Action.EndEffect <$> f paid <*> f ended
  Action.MakeLaterPayment oid stamp -> (`Action.MakeLaterPayment` stamp) <$> f oid
  Action.PerformAsASorcery oid -> Action.PerformAsASorcery <$> f oid
  Action.PayLifeForMana {} -> pure action
  Action.RollPlanarDie -> pure action
  Action.RerollShownDie die modifier -> Action.RerollShownDie <$> f die <*> f modifier

-- | The tree's root and the lowest and highest rewards backed up through it,
-- whose spread scales its exploration ('select').
data Tree = MkTree Node (Maybe (Integer, Integer))

-- | A decision of the tree: each move tried from it, by its 'Edge'.
newtype Node = MkNode (Map.Map Edge Branch)

-- | A move tried from a node.
data Branch = MkBranch
  { -- | The rewards of the iterations through it, for its mover.
    reward :: Integer,
    -- | The iterations through it.
    visits :: Natural,
    -- | The iterations reaching its node in a world offering it (SO-ISMCTS's
    -- availability count).
    available :: Natural,
    -- | The decisions after it.
    child :: Node
  }

-- | One iteration of `viewer`'s tree from a world at its decision, `restart`
-- asking it again, the first decision answered by the `forced` edge where it
-- is legal, its picks after it answering the prompts they fit ('answering'):
-- each decision the tree holds ('decisionOf') answered by
-- 'select' while its every legal edge has been tried, a decision with an
-- untried one answered by one of those at random, which ends the descent;
-- every other prompt, and every one after, answered as a 'rollout' answers
-- it and cut off as one is. The edges walked, each beside the edges legal at
-- its node, and the reward for each player: 'score' at a result, else
-- 'evaluate' short of a win or a loss.
descend :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Play m () -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> Maybe Forced -> Tree -> GameState.GameState -> Game.Play m ([([Edge], Edge)], PlayerId.PlayerId -> Integer)
descend registry budget weights breathe restart viewer forced (MkTree root range) world =
  let scale = maybe 1 (\(lowest, highest) -> max 1 (highest - lowest)) range
      seats = Game.consulting registry (Map.fromList [(seat, policyOf registry budget) | seat <- Map.keys (GameState.players world)])
      final = TurnRegisters.turnNumber (GameState.turn world) + SearchBudget.turns budget
      go :: Natural -> Maybe Node -> [([Edge], Edge)] -> [Pick] -> Program.Program (Asked.Asked GameState.GameState) (Result.Result, GameState.GameState) -> Game.Play m ([([Edge], Edge)], PlayerId.PlayerId -> Integer)
      go left at path picks program =
        breathe >> case program of
          Program.Return (result, _) -> pure (reverse path, (`score` result))
          Program.Then asked k
            | left == 0 || TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) > final -> pure (reverse path, \pid -> cut weights pid (Asked.game asked))
            | Just (MkNode branches) <- at,
              Just (mover, choices) <- decisionOf asked -> do
                let keyed = ListUtils.nubOrdOn fst [(edgeOf viewer (Asked.game asked) mover move, answer) | (move, answer) <- NonEmpty.toList choices]
                    legal = fmap fst keyed
                    pending = lapsing picks (Asked.prompt asked)
                case (null path, [(taken, answer, rest) | MkForced taken opened rest <- Foldable.toList forced, (edge, answer) <- keyed, edge == opened], NonEmpty.nonEmpty [choice | choice@(edge, _) <- keyed, Map.notMember edge branches]) of
                  (True, (taken, answer, rest) : _, _) -> go (left - 1) (fmap child (Map.lookup taken branches)) ((legal, taken) : path) rest (k answer)
                  (_, _, Just untried) -> do
                    (edge, answer) <- Generator.pick untried
                    go (left - 1) Nothing ((legal, edge) : path) pending (k answer)
                  _ -> do
                    let (edge, answer) = select scale branches keyed
                    go (left - 1) (fmap child (Map.lookup edge branches)) ((legal, edge) : path) pending (k answer)
            | otherwise -> answering seats picks asked >>= \(answer, rest) -> go (left - 1) at path rest (k answer)
   in go (SearchBudget.prompts budget) (Just root) [] [] (State.runStateT restart world)

-- | The decision a prompt asks that the tree holds: a 'searchable' priority
-- decision, declaration of attackers or declaration of blockers, its decider
-- and each choice as a 'Move' beside the answer it gives.
decisionOf :: Asked.Asked GameState.GameState r -> Maybe (PlayerId.PlayerId, NonEmpty.NonEmpty (Move, r))
decisionOf asked = case Asked.prompt asked of
  Prompt.ChooseAction decider pid menu
    | searchable asked decider pid (options menu) -> (,) pid <$> NonEmpty.nonEmpty [(Acted action, action) | action <- options menu]
  Prompt.DeclareAttackers decider pid offered
    | searchable asked decider pid (attacks offered) -> (,) pid <$> NonEmpty.nonEmpty [(Attacked declared, declared) | declared <- attacks offered]
  Prompt.DeclareBlockers decider pid candidates attacked
    | searchable asked decider pid (blocks candidates attacked) -> (,) pid <$> NonEmpty.nonEmpty [(Blocked declared, declared) | declared <- blocks candidates attacked]
  _ -> Nothing

-- | UCB1 over a node's legal edges, all tried, by availability (SO-ISMCTS):
-- each one's mean reward plus 'exploration' times `scale` times the root of
-- the log of its availability over its visits; the first highest.
select :: Integer -> Map.Map Edge Branch -> [(Edge, r)] -> (Edge, r)
select scale branches keyed =
  let value (edge, _) = case Map.lookup edge branches of
        Just branch
          | visits branch > 0 ->
              let n = real (visits branch)
               in real (reward branch) / n + exploration * real scale * sqrt (log (real (max 1 (available branch))) / n)
        _ -> 1 / 0
   in List.maximumBy (\a b -> compare (value a) (value b)) keyed

-- | A count or reward as a 'Double', for 'select's arithmetic.
real :: (Real a) => a -> Double
real = fromRational . toRational

-- | UCB1's exploration constant, of a reward scaled to one.
exploration :: Double
exploration = 0.7

-- | `tree` with an iteration backed up: along the edges walked, each node's
-- legal edges tried before made available once more and the one walked
-- visited, given its mover's reward, and made available.
backup :: (PlayerId.PlayerId -> Integer) -> [([Edge], Edge)] -> Tree -> Tree
backup value path (MkTree root range) =
  let along steps (MkNode branches) = case steps of
        [] -> MkNode branches
        (legal, taken@(MkEdge mover _ _)) : rest ->
          let offered = Foldable.foldl' (flip (Map.adjust (\branch -> branch {available = available branch + 1}))) branches (filter (/= taken) legal)
              before = Map.findWithDefault (MkBranch 0 0 0 (MkNode Map.empty)) taken offered
           in MkNode (Map.insert taken before {reward = reward before + value mover, visits = visits before + 1, available = available before + 1, child = along rest (child before)} offered)
      rewards = [value mover | (_, MkEdge mover _ _) <- path]
      widened = foldr (\r -> Just . maybe (r, r) (\(lowest, highest) -> (min lowest r, max highest r))) range rewards
   in MkTree (along path root) widened

-- | The choice scored best, or at a 'mistaken' decision one of those scored
-- best below it, where one is; on a tie, the 'idle' one where it is among
-- them, else one at random. Parun.SearchSpec's Drudge Skeletons is the proof.
slipping :: (Monad m, Ord s) => SearchBudget.SearchBudget -> (a -> Bool) -> NonEmpty.NonEmpty (s, a) -> State.StateT Generator.Type.Generator m a
slipping budget resting scored = do
  let top = maximum (fmap fst scored)
  slipped <- mistaken budget
  let aim = if slipped then maybe top maximum (NonEmpty.nonEmpty [total | (total, _) <- NonEmpty.toList scored, total < top]) else top
      tied = [option | (total, option) <- NonEmpty.toList scored, total == aim]
  case (List.find resting tied, NonEmpty.nonEmpty tied) of
    (Just option, _) -> pure option
    (Nothing, Just drawn) -> Generator.pick drawn
    (Nothing, Nothing) -> pure (snd (NonEmpty.head scored))

-- | Whether a move does nothing: a pass of priority (CR 117.3d).
idle :: Move -> Bool
idle move = move == Acted Action.Pass

-- | Whether a searched decision is a mistake, at the budget's
-- `SearchBudget.mistakes` percent: drawn once its playouts are, and only at
-- a rate above 0, so a budget making none plays as it did without the field.
mistaken :: (Monad m) => SearchBudget.SearchBudget -> State.StateT Generator.Type.Generator m Bool
mistaken budget
  | SearchBudget.mistakes budget == 0 = pure False
  | otherwise = fmap (< SearchBudget.mistakes budget) (Generator.between 0 99)

-- | A priority decision a player made (CR 117.1): the player, the game as
-- they were asked and the action they took.
type Watched = (PlayerId.PlayerId, GameState.GameState, Action.Action)

-- | Keeps each 'restartable' priority decision a player makes, however many
-- options its player saw, its action one of the 'options' searched, through
-- `keep`, which is given the decisions kept before and returns them less
-- every one 'stale' now, the new one after them, its spine forced once the
-- result is, so no stale game is held. A declaration of attackers or
-- blockers is not kept: what it may declare is of the battlefield (CR
-- 508.1a, 509.1a), which every world deals alike, so the opponent model makes
-- it as likely in each.
watching :: (Monad m) => (([Watched] -> [Watched]) -> m ()) -> Observer.Observer (Game.Play m)
watching keep =
  Observer.MkObserver
    { Observer.before = const (pure ()),
      Observer.after = \asked answer -> case Asked.prompt asked of
        Prompt.ChooseAction decider pid _
          | not (null (options [answer])) && restartable asked decider pid ->
              Game.underneath (keep (\plays -> let kept = filter (not . stale (Asked.game asked)) plays <> [(pid, Asked.game asked, answer)] in length kept `seq` kept))
        _ -> pure ()
    }

-- | Whether a play is no longer weighed in `gs`: made before the turn
-- before `gs`'s, which keeps the weighing to a reduced budget, or none of the
-- cards in its player's hand then is there now, so it says nothing of it.
stale :: GameState.GameState -> Watched -> Bool
stale gs (pid, past, _) =
  let handIn g = Map.findWithDefault mempty pid (GameState.hand g)
      turnOf g = TurnRegisters.turnNumber (GameState.turn g)
   in turnOf past + 1 < turnOf gs || not (any (`elem` handIn gs) (handIn past))

-- | How `viewer`, remembering `memory`, deals each playout's world from
-- `gs`: with no `SearchBudget.candidates`, a fresh 'sample' each; otherwise
-- that many samples drawn once, then each playout's drawn among them weighted
-- by its 'plausibility' given the opponents' `plays`, evenly when every
-- weight is 0.
worlds :: (Monad m) => SearchBudget.SearchBudget -> Prior -> Memory -> [Watched] -> PlayerId.PlayerId -> GameState.GameState -> Game.Play m (Game.Play m GameState.GameState)
worlds budget prior memory plays viewer gs
  | SearchBudget.candidates budget == 0 = pure (sample prior memory viewer gs)
  | otherwise = do
      drawn <- Monad.replicateM (Natural.toIntSaturating (SearchBudget.candidates budget)) (sample prior memory viewer gs)
      weighed <- traverse (\world -> fmap (\w -> (w, world)) (plausibility prior memory viewer plays gs world)) drawn
      pure $ case NonEmpty.nonEmpty weighed of
        Nothing -> sample prior memory viewer gs
        Just pool
          | all ((== 0) . fst) pool -> Generator.pick (fmap snd pool)
          | otherwise -> Generator.weighted pool

-- | How likely the opponents of `viewer`, remembering `memory`, were to make
-- their `plays` not 'stale' had `gs`'s unseen cards been `world`'s, each
-- play scored as the random bot choosing among the options searched
-- ('options'), the opponent model: one over their number in the game as it
-- was, 0 if its action is not among them, its cards `viewer` could not see
-- then dealt anew ('sample') save those it still cannot see now, which hold
-- `world`'s, and those its action names, which hold what each 'became' in
-- `world`. A play none of whose then-unseen cards in its player's hand is
-- still unseen counts 1, as does one whose restarted menu is not asked
-- first (Engine.resumePriority).
plausibility :: (Monad m) => Prior -> Memory -> PlayerId.PlayerId -> [Watched] -> GameState.GameState -> GameState.GameState -> Game.Play m Rational
plausibility prior memory viewer plays gs world =
  fmap product . Monad.forM [play | play@(pid, _, _) <- plays, pid /= viewer, not (stale gs play)] $ \(pid, past, action) -> do
    let hiddenNow = Set.fromList (unseen memory viewer gs pid)
        hiddenThen = unseen memory viewer past pid
        shared = filter (`Set.member` hiddenNow) hiddenThen
        named = filter (`elem` hiddenThen) (fst (objectsIn (\oid -> ([oid], oid)) action))
        heldThen = Map.findWithDefault mempty pid (GameState.hand past)
    if not (any (`elem` heldThen) shared)
      then pure 1
      else do
        redealt <- sample prior memory viewer past
        let carried = Foldable.foldl' (carry world) redealt ([(oid, oid) | oid <- shared] <> [(became past gs oid, oid) | oid <- named])
        pure $ case menuAgain pid carried of
          Just menu | n <- length (options menu), n > 0 -> if elem action menu then 1 / toRational n else 0
          _ -> 1

-- | The object `oid` of `past` became in `gs`, following each move of it
-- logged since ('logOf'), as each makes it a new object (CR 400.7).
became :: GameState.GameState -> GameState.GameState -> ObjectId.ObjectId -> ObjectId.ObjectId
became past gs oid =
  let follow current entry = case LoggedEvent.event entry of
        GameEvent.Moved moved | ZoneChange.departed (Moved.change moved) == current -> ZoneChange.object (Moved.change moved)
        _ -> current
   in Foldable.foldl' follow oid (Seq.drop (Seq.length (logOf past)) (logOf gs))

-- | `target` with its object `into` holding what object `from` holds in
-- `holder`, its printing interned in `target`.
carry :: GameState.GameState -> GameState.GameState -> (ObjectId.ObjectId, ObjectId.ObjectId) -> GameState.GameState
carry holder target (from, into) = case Engine.Game.lookupObject from holder of
  Nothing -> target
  Just object ->
    let (source, interned) = case Object.source object of
          Source.OfCard pid | Just printing <- Engine.Game.printingOf pid holder -> let (pid', g) = Engine.Game.intern printing target in (Source.OfCard pid', g)
          other -> (other, target)
     in interned {GameState.objects = Map.adjust (\o -> o {Object.source = source, Object.identity = Object.identity object}) into (GameState.objects interned)}

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

-- | A part of the searched option a playout gives a prompt: its answer to
-- each prompt.
newtype Pick = MkPick (forall r. Prompt.Prompt r -> Fit r)

-- | What a pick makes of a prompt.
data Fit r
  = -- | It answers it.
    Fits r
  | -- | It waits for a later prompt.
    Waits
  | -- | It answers no prompt now: this one is answered as if it were not
    -- there.
    Lapses

-- | The prompt answered by the first of the picks, past each that 'Lapses'
-- there, where it 'Fits', by `seats` otherwise; the picks left, less those
-- that lapsed and that one where it does not 'Waits'.
answering :: (Monad m) => Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m) -> [Pick] -> Asked.Asked GameState.GameState r -> Game.Play m (r, [Pick])
answering seats picks asked = case picks of
  MkPick pick : rest -> case pick (Asked.prompt asked) of
    Fits answer -> pure (answer, rest)
    Waits -> fmap (\answer -> (answer, picks)) (Answerer.unwrap seats asked)
    Lapses -> answering seats rest asked
  [] -> fmap (\answer -> (answer, [])) (Answerer.unwrap seats asked)

-- | The picks left once a prompt the tree holds is answered: less each one
-- before the first that 'Waits' there.
lapsing :: [Pick] -> Prompt.Prompt r -> [Pick]
lapsing picks prompt = dropWhile (\(MkPick pick) -> case pick prompt of Waits -> False; _ -> True) picks

-- | `pid`'s priority decision answered `action`.
acting :: PlayerId.PlayerId -> Action.Action -> Pick
acting pid action = MkPick $ \prompt -> case prompt of
  Prompt.ChooseAction _ holder _ | holder == pid -> Fits action
  _ -> Waits

-- | `pid`'s declaration of attackers answered `declared`.
attacking :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Pick
attacking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareAttackers _ declarer _ | declarer == pid -> Fits declared
  _ -> Waits

-- | `pid`'s declaration of blockers answered `declared`.
blocking :: PlayerId.PlayerId -> Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId) -> Pick
blocking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareBlockers _ declarer _ _ | declarer == pid -> Fits declared
  _ -> Waits

-- | `pid`'s next prompt a line answers ('lined') answered by the step where
-- it may, lapsing there otherwise or at `pid`'s next priority decision.
stepping :: PlayerId.PlayerId -> Step -> Pick
stepping pid step = MkPick $ \prompt -> case lined prompt of
  Just (_, chooser, MkLined _ answers _) | chooser == pid -> maybe Lapses Fits (answers step)
  _ -> case prompt of
    Prompt.ChooseAction _ holder _ | holder == pid -> Lapses
    _ -> Waits

-- | The actions of a priority menu worth searching: all but CR 605.3a's mana
-- abilities activated at priority, which a payment activates as it needs
-- (CR 601.2g), and life paid for mana at priority (Action.PayLifeForMana).
options :: [Action.Action] -> [Action.Action]
options = filter $ \action -> case action of
  Action.ActivateManaAbility {} -> False
  Action.PayLifeForMana {} -> False
  _ -> True

-- | The first candidate a fewest group of `pid`'s mana sources paying
-- `cost` beside the pool needs (CR 601.2g): the others set aside one at a
-- time while the rest still pays, the cost is not paid without it. Setting a
-- source aside taps it in a copy of `gs`, so one whose ability does not tap
-- it is never needed; Nothing where no candidate is.
paymentSource :: PlayerId.PlayerId -> ManaCost.ManaCost -> NonEmpty.NonEmpty ObjectId.ObjectId -> GameState.GameState -> Maybe ObjectId.ObjectId
paymentSource pid cost candidates gs =
  let sources = Engine.Mana.manaSources Engine.Cost.manaActivations pid gs
      paysWithout aside = Engine.Mana.canPay Engine.Cost.manaActivations pid cost gs {GameState.objects = List.foldl' (flip (Map.adjust (\o -> o {Object.tapped = TapState.Tapped}))) (GameState.objects gs) aside}
      needed candidate =
        let aside = List.foldl' (\kept source -> if paysWithout (source : kept) then source : kept else kept) [] (filter (/= candidate) sources)
         in paysWithout aside && not (paysWithout (candidate : aside))
   in List.find needed (NonEmpty.toList candidates)

-- | The declarations of attackers searched, from the creatures offered (CR
-- 508.1a): every subset of up to 'widest' declarations, else none, all, each
-- creature alone and all but each. The engine judges each (CR 508.1c, 508.1d).
attacks :: [ObjectId.ObjectId] -> [[ObjectId.ObjectId]]
attacks offered
  | 2 ^ length offered <= widest = List.subsequences offered
  | otherwise = ListUtils.nubOrd ([[], offered] <> fmap pure offered <> [List.delete oid offered | oid <- offered])

-- | The declarations of blockers searched, from the creatures offered and
-- the attackers they may block (CR 509.1a): every one blocking at most one
-- attacker each, where there are up to 'widest', else none, each blocker
-- alone on each attacker, every blocker on each attacker, and each rotation
-- of the blockers over the attackers, every blocker blocking or one an attacker.
-- The engine judges each (CR 509.1b, 509.1c).
blocks :: [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> [Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId)]
blocks candidates attacked
  | (length attacked + 1) ^ length candidates <= widest = fmap (Map.fromList . Maybe.catMaybes) (traverse (\blocker -> Nothing : [Just (blocker, Set.singleton attacker) | attacker <- attacked]) candidates)
  | otherwise =
      let declared pairs = Map.fromList (fmap (fmap Set.singleton) pairs)
          rotated k = drop k (cycle attacked)
       in ListUtils.nubOrd
            ( Map.empty
                : [Map.singleton blocker (Set.singleton attacker) | blocker <- candidates, attacker <- attacked]
                  <> [declared [(blocker, attacker) | blocker <- candidates] | attacker <- attacked]
                  <> concat [[declared (zip candidates (rotated k)), declared (zip candidates (take (length attacked) (rotated k)))] | k <- [0 .. length attacked - 1]]
            )

-- | The most declarations 'attacks' and 'blocks' search exhaustively, and
-- the most choices of targets 'aims' offers and lines 'aimed' searches.
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
-- again and the `picked` answering the prompts they fit ('answering'), every
-- seat answered by the budget's 'Playout.Playout' otherwise, scored 'won' a
-- win, minus it a loss, 0 a draw; a playout cut off once it begins `SearchBudget.turns` turns after the
-- decision's, or after `SearchBudget.prompts` prompts, is scored by
-- 'cut' at the prompt it stopped at.
rollout :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Play m () -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> [Pick] -> GameState.GameState -> Game.Play m Integer
rollout registry budget weights breathe restart pid picked world =
  let seats = Game.consulting registry (Map.fromList [(seat, policyOf registry budget) | seat <- Map.keys (GameState.players world)])
      final = TurnRegisters.turnNumber (GameState.turn world) + SearchBudget.turns budget
      go :: Natural -> [Pick] -> Program.Program (Asked.Asked GameState.GameState) (Result.Result, GameState.GameState) -> Game.Play m Integer
      go left picks program =
        breathe >> case program of
          Program.Return (result, _) -> pure (score pid result)
          Program.Then asked k
            | left == 0 || TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) > final -> pure (cut weights pid (Asked.game asked))
            | otherwise -> answering seats picks asked >>= \(answer, rest) -> go (left - 1) rest (k answer)
   in go (SearchBudget.prompts budget) picked (State.runStateT restart world)

-- | The budget's 'Playout.Playout' policy.
policyOf :: (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
policyOf registry budget = case SearchBudget.playout budget of
  Playout.Random -> Game.bot registry
  Playout.Greedy -> greedy registry
  Playout.Polar -> polar registry
  Playout.Classified -> classified registry

-- | A cut-off playout's position for `pid`: 'evaluate' squashed to its
-- expected result, 'won' times tanh (evaluate / 'won'), the logistic win
-- probability `parun fit` assumes ('Parun.Play.Fit.weighted'), held strictly
-- short of a win or a loss.
cut :: Weights.Weights -> PlayerId.PlayerId -> GameState.GameState -> Integer
cut weights pid gs =
  let squashed = round (Integer.toDouble won * tanh (Integer.toDouble (evaluate weights pid gs) / Integer.toDouble won))
   in max (1 - won) (min (won - 1) squashed)

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
  let position = featuresOfAll gs
      weighed side player = dot (side weights) (Map.findWithDefault none player position)
      none = Features.MkFeatures 0 0 0 0 0 0
   in weighed Weights.ours pid - maybe 0 maximum (NonEmpty.nonEmpty [weighed Weights.theirs other | other <- Map.keys position, other /= pid])

-- | Every player's 'features' in `gs`, the board projected once.
featuresOfAll :: GameState.GameState -> Map.Map PlayerId.PlayerId Features.Features
featuresOfAll gs =
  let board = Projection.projectAll gs
      grants = Projection.View.controlGrants gs
   in Map.mapWithKey (\pid _ -> features board grants pid gs) (GameState.players gs)

-- | The weighted sum of a player's features.
dot :: Features.Features -> Features.Features -> Integer
dot weight count =
  sum [field weight * field count | (_, field) <- featureFields]

-- | Every feature, named as Parun.Codec.Features names it, in the order of
-- Features.MkFeatures's fields.
featureFields :: [(Text.Text, Features.Features -> Integer)]
featureFields =
  [ (Text.pack "life", Features.life),
    (Text.pack "hand", Features.hand),
    (Text.pack "power", Features.power),
    (Text.pack "toughness", Features.toughness),
    (Text.pack "lands", Features.lands),
    (Text.pack "permanents", Features.permanents)
  ]

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
-- bot's (Game.bot), targets included. It reads only its own menu and the card
-- types of the cards on it.
greedy :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
greedy registry = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction _ _ menu -> greedyAction (Asked.game asked) menu
  _ -> Answerer.unwrap (Game.bot registry) asked

-- | The greedy policy choosing targets by polarity: each slot's targets chosen by the
-- polarity of the effects naming it ('polarTargets'), its priority decisions
-- the greedy policy's ('greedyAction'). It reads effects' classification
-- (Parun.Play.Polarity), never their identity. Its modes and payments are
-- the random bot's ('classified' chooses them).
polar :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
polar registry = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction _ _ menu -> greedyAction (Asked.game asked) menu
  Prompt.ChooseTargets _ chooser oid slots -> polarTargets chooser (stackEffects oid (Asked.game asked)) (Asked.game asked) slots
  _ -> Answerer.unwrap (Game.bot registry) asked

-- | The polar policy, its modes, its resolution payments and its targets
-- chosen by classification: modes by 'classifiedModes', a CR 118.12 offer at
-- a clause by 'classifiedPayment', and targets by the polarity of the chosen
-- modes' effects alone ('chosenEffects'). Every other prompt, and a mode or
-- payment no classification separates, is the polar policy's.
classified :: (Monad m) => Registry.Registry m -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
classified registry = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseModes _ chooser oid legal selection
    | Just best <- NonEmpty.nonEmpty (classifiedModes chooser oid (Asked.game asked) legal selection) -> Generator.pick best
  Prompt.ChooseToPay _ payer oid (PayOffer.AtClause mode clause) _ _
    | Just decision <- classifiedPayment payer oid mode clause (Asked.game asked) -> pure decision
  Prompt.ChooseTargets _ chooser oid slots -> polarTargets chooser (chosenEffects oid (Asked.game asked)) (Asked.game asked) slots
  _ -> Answerer.unwrap (polar registry) asked

-- | Of CR 700.2's selections over the `legal` modes, those scoring highest
-- for `chooser`, lowest where another player chooses for the object's
-- controller (CR 700.2e). A mode scores a point for each slot whose polarity
-- (over that mode's effects) some legal recipient is favoured by, from the
-- controller's side ('favouredBy'), and loses one for each polar slot none
-- is; a selection is the sum of its modes'.
classifiedModes :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> Set.Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> [Seq.Seq ModeIndex.ModeIndex]
classifiedModes chooser oid gs legal selection =
  let (kind, modals) = stackModals oid gs
      controller = Maybe.fromMaybe chooser (Projection.View.controllerOf oid gs)
      sign = if controller == chooser then 1 else -1
      slotScore effects (slot, targetSlot) = case Polarity.slotPolarity slot effects of
        Nothing -> 0
        Just polarity
          | any (favouredBy controller gs polarity) (Target.legalRecipients kind (Just controller) oid targetSlot gs) -> 1
          | otherwise -> -1 :: Integer
      modeScore idx =
        sum
          [ sum (fmap (slotScore (Mode.allEffects mode)) (Map.toList (Mode.targetSlots mode)))
          | modal <- modals,
            mode <- Foldable.toList (Engine.Modal.modeAtIndex idx modal)
          ]
      scored = [(sign * sum (fmap modeScore modes), modes) | modes <- Engine.Modal.selections legal selection]
   in case NonEmpty.nonEmpty scored of
        Nothing -> []
        Just some -> let top = maximum (fmap fst some) in [modes | (worth, modes) <- scored, worth == top]

-- | Whether `payer` takes the CR 118.12 offer the resolving `oid` makes at
-- `clause` of `mode`, Nothing where the classification does not separate the
-- answers. Each recipient bound to a polar slot of a clause the offer gates
-- (its own, and those naming it as `PayGate.offeredAt`) counts toward the
-- payer where its effect helps the payer's side or harms another's
-- ('favouredBy'), against them where the reverse; paying is worth what the
-- IfPaid clauses count less what the others do.
classifiedPayment :: PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> GameState.GameState -> Maybe PaymentDecision.PaymentDecision
classifiedPayment payer oid mode clause gs =
  let targets = Projection.View.targetSlotsOfStackObject gs oid
      gated =
        [ (PayGate.branch gate, Foldable.toList (Clause.effects c))
        | modal <- snd (stackModals oid gs),
          m <- Foldable.toList (Engine.Modal.resolvingModeAt mode modal),
          (i, c) <- zip [0 ..] (Foldable.toList (Mode.clauses m)),
          Just gate <- [Clause.payGate c],
          Maybe.fromMaybe (ClauseIndex.MkClauseIndex i) (PayGate.offeredAt gate) == clause
        ]
      toward polarity recipient
        | Maybe.isNothing (recipientSide gs recipient) = 0
        | favouredBy payer gs polarity recipient = 1
        | otherwise = -1 :: Integer
      worth effects = sum [toward polarity recipient | (slot, recipients) <- Map.toList targets, Just polarity <- [Polarity.slotPolarity slot effects], recipient <- Set.toList recipients]
      paying = sum [(if branch == PayBranch.IfPaid then 1 else -1) * worth effects | (branch, effects) <- gated]
   in case compare paying 0 of
        GT -> Just PaymentDecision.Pays
        LT -> Just PaymentDecision.Declines
        EQ -> Nothing

-- | A priority decision: a land play (CR 305.1) at random where the menu
-- offers one, else a cast (CR 601) of a card with a permanent type (CR
-- 110.4), else one of the other 'options' or a pass, each equally likely.
-- The menu offers only what may be afforded (Engine.Cast.castableSpells).
-- Another spell waits for the random pick, cast at its first chance being
-- worse with polar targets (Playout.Polar) as well as random ones.
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

-- | CR 601.2c's targets for `chooser`, each slot's drawn first from the
-- recipients on the side its polarity favours (Polarity.slotPolarity over
-- `effects`, 'favouredBy'): an opponent or what an opponent controls where the slot's
-- effect harms, `chooser` or what they control where it helps. The rest of a
-- slot's count, and a slot with no polarity, is drawn at random from the
-- other recipients.
polarTargets :: (Monad m) => PlayerId.PlayerId -> [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)] -> GameState.GameState -> Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient) -> State.StateT Generator.Type.Generator m (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient))
polarTargets chooser effects gs =
  let choose slot (count, legal) = do
        let polarity = Polarity.slotPolarity slot effects
            (first, second) = List.partition (\recipient -> any (\p -> favouredBy chooser gs p recipient) polarity) (Set.toList legal)
        ahead <- Generator.shuffle first
        behind <- Generator.shuffle second
        pure (Set.fromList (List.genericTake count (ahead <> behind)))
   in Map.traverseWithKey choose

-- | Whether `recipient` is on the side `polarity` favours for `chooser`: an
-- opponent or what an opponent controls where it harms, `chooser` or what
-- they control where it helps ('recipientSide').
favouredBy :: PlayerId.PlayerId -> GameState.GameState -> Polarity.Type.Polarity -> Recipient.Recipient -> Bool
favouredBy chooser gs polarity recipient = case polarity of
  Polarity.Type.Harms -> maybe False (/= chooser) (recipientSide gs recipient)
  Polarity.Type.Helps -> recipientSide gs recipient == Just chooser

-- | The player `recipient` is or belongs to: a player, else an object's
-- controller, else its owner (CR 108.4a: a card with no controller is its
-- owner's).
recipientSide :: GameState.GameState -> Recipient.Recipient -> Maybe PlayerId.PlayerId
recipientSide gs recipient = Recipient.playerOf recipient <|> (owned =<< Recipient.objectOf recipient)
  where
    owned target = Projection.View.controllerOf target gs <|> fmap Object.owner (Engine.Game.lookupObject target gs)

-- | The effects the stack object `oid` may carry out: an ability's every
-- mode's, a spell's face's every mode's (Engine.Modal.allEffects).
stackEffects :: ObjectId.ObjectId -> GameState.GameState -> [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)]
stackEffects oid gs = foldMap Engine.Modal.allEffects (snd (stackModals oid gs))

-- | The effects of the modes chosen for `oid` (CR 601.2b), and its shared
-- clauses', once they are bound; 'stackEffects' before.
chosenEffects :: ObjectId.ObjectId -> GameState.GameState -> [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)]
chosenEffects oid gs = case foldMap (Binding.modesOf . Object.bindings) (Engine.Game.lookupObject oid gs) of
  chosen
    | Seq.null chosen -> stackEffects oid gs
    | otherwise -> foldMap (\modal -> Engine.Modal.modesEffects chosen modal <> Foldable.toList (Mode.allEffects (Engine.Modal.sharedMode modal))) (snd (stackModals oid gs))

-- | The stack object `oid`'s kind (CR 601.2c's announcement roads) and the
-- modal payloads it may carry out: an ability's, a spell's face's.
stackModals :: ObjectId.ObjectId -> GameState.GameState -> (StackObjectKind.StackObjectKind, [Modal.Modal Card.Card (GrantedAbility.GrantedAbility Card.Card)])
stackModals oid gs = case fmap Object.source (Engine.Game.lookupObject oid gs) of
  Just (Source.OfAbility src) -> (StackObjectKind.ActivatedAbility, [ActivatedAbility.modal (ActivatedAbilitySource.ability src)])
  Just (Source.OfTrigger src) -> (StackObjectKind.TriggeredAbility, [TriggeredAbility.modal (TriggeredAbilitySource.ability src)])
  Just (Source.OfInherentTrigger src) -> (StackObjectKind.TriggeredAbility, [TriggeredAbility.modal (InherentTriggerSource.ability src)])
  _ -> (StackObjectKind.Spell, fmap Face.spell (Foldable.toList (Projection.View.spellFaceOf oid gs)))
