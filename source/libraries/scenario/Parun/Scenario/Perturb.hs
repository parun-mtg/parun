-- Changes a scenario's checks one at a time and reruns it, so a check that
-- still passes with its expected value changed is found without editing the
-- file by hand (#4420's rule for a converted scenario).
module Parun.Scenario.Perturb where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Extra.Int as Int
import qualified Parun.Types.AttackersAre as AttackersAre
import qualified Parun.Types.BlockersAre as BlockersAre
import qualified Parun.Types.Board as Board
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Check as Check
import qualified Parun.Types.CountIs as CountIs
import qualified Parun.Types.CountersAre as CountersAre
import qualified Parun.Types.DamageIs as DamageIs
import qualified Parun.Types.DefendersAre as DefendersAre
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.KeywordsAre as KeywordsAre
import qualified Parun.Types.Label as Label
import qualified Parun.Types.LifeIs as LifeIs
import qualified Parun.Types.ManaValueIs as ManaValueIs
import qualified Parun.Types.MonarchIs as MonarchIs
import qualified Parun.Types.NamesAre as NamesAre
import qualified Parun.Types.PlayerCountersAre as PlayerCountersAre
import qualified Parun.Types.PowerToughnessIs as PowerToughnessIs
import qualified Parun.Types.Reply as Reply
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.Seat as Seat
import qualified Parun.Types.Start as Start
import qualified Parun.Types.SubtypesAre as SubtypesAre
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TappedIs as TappedIs
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TypesAre as TypesAre
import qualified Parun.Types.ViewIs as ViewIs

-- | Where a check sits in its scenario, by index into its array.
data Location
  = InTimeline Natural.Natural
  | InFinal Natural.Natural
  deriving (Eq, Ord, Show)

-- | One check with its expected value changed.
data Perturbation = MkPerturbation
  { location :: Location,
    original :: Check.Check,
    perturbed :: Check.Check
  }
  deriving (Eq, Ord, Show)

-- | What perturbing a check found worth reporting.
data Finding
  = -- | The scenario still passed with this perturbation.
    Survived Perturbation
  | -- | The check has no perturbation, so nothing was tried.
    Skipped Location Check.Check
  deriving (Eq, Ord, Show)

-- | Each check in turn, each of its perturbations run by `passes`, which says
-- whether a scenario runs clean; in file order, timeline first. The scenario
-- itself is assumed to pass: a caller runs it unmodified first.
findings :: (Monad m) => (Scenario.Scenario -> m Bool) -> Scenario.Scenario -> m (Seq.Seq Finding)
findings passes scenario =
  let seats = seatsOf scenario
      try (at, check) = case perturb seats check of
        [] -> pure (Seq.singleton (Skipped at check))
        changes -> fmap (Seq.fromList . Maybe.catMaybes) (traverse (survives at check) changes)
      survives at check change = do
        passed <- passes (replace at change scenario)
        pure (if passed then Just (Survived (MkPerturbation at check change)) else Nothing)
   in fmap Foldable.fold (traverse try (checksOf scenario))

-- | Every check the scenario makes, with where it sits.
checksOf :: Scenario.Scenario -> Seq.Seq (Location, Check.Check)
checksOf scenario =
  let timed index entry = case Timed.entry entry of
        Entry.Expect check -> Seq.singleton (InTimeline (Int.toNaturalSaturating index), check)
        Entry.Do _ -> Seq.empty
        Entry.Refuse _ -> Seq.empty
      final index check = (InFinal (Int.toNaturalSaturating index), check)
   in Foldable.fold (Seq.mapWithIndex timed (Scenario.timeline scenario)) <> Seq.mapWithIndex final (Scenario.final scenario)

-- | The scenario with the check at `at` replaced; unchanged when none is there.
replace :: Location -> Check.Check -> Scenario.Scenario -> Scenario.Scenario
replace at check scenario = case at of
  InTimeline n ->
    let swap index entry = case Timed.entry entry of
          Entry.Expect _ | Int.toNaturalSaturating index == n -> entry {Timed.entry = Entry.Expect check}
          _ -> entry
     in scenario {Scenario.timeline = Seq.mapWithIndex swap (Scenario.timeline scenario)}
  InFinal n ->
    let swap index old = if Int.toNaturalSaturating index == n then check else old
     in scenario {Scenario.final = Seq.mapWithIndex swap (Scenario.final scenario)}

-- | The seats in turn order, which a perturbation naming a player draws on.
seatsOf :: Scenario.Scenario -> [Label.Label]
seatsOf scenario = case Scenario.start scenario of
  Start.FromBoard board -> fmap Seat.name (NonEmpty.toList (Board.seats board))
  Start.FromDecks _ entrants -> fmap Entrant.name (NonEmpty.toList entrants)

-- | Every changed expected value tried for a check: a number off by one each
-- way (down only from above zero), a tap state flipped, a set or list with an
-- element dropped and one added, a nothing made something and back. Empty
-- when no change is sensible: a check of no attackers names no attacker to add.
perturb :: [Label.Label] -> Check.Check -> [Check.Check]
perturb seats check = case check of
  Check.Life x -> fmap (\n -> Check.Life x {LifeIs.life = n}) (integers (LifeIs.life x))
  Check.Count x -> fmap (\n -> Check.Count x {CountIs.count = n}) (naturals (CountIs.count x))
  Check.Damage x -> fmap (\n -> Check.Damage x {DamageIs.damage = n}) (naturals (DamageIs.damage x))
  Check.Tapped x -> [Check.Tapped x {TappedIs.tapped = flipped (TappedIs.tapped x)}]
  Check.Counters x -> fmap (\n -> Check.Counters x {CountersAre.count = n}) (naturals (CountersAre.count x))
  Check.Types x -> fmap (\t -> Check.Types x {TypesAre.types = t}) (enumSets (TypesAre.types x))
  Check.Attackers x -> fmap (Check.Attackers . AttackersAre.MkAttackersAre) (dropOne (AttackersAre.attackers x))
  Check.Blockers x ->
    let changed = case BlockersAre.blockers x of
          Nothing -> [Just Set.empty]
          Just set -> Nothing : fmap Just (dropMin set)
     in fmap (\b -> Check.Blockers x {BlockersAre.blockers = b}) changed
  Check.Defenders x ->
    let players = DefendersAre.players x
        added = fmap (\seat -> players <> [seat]) (take 1 (filter (\seat -> notElem seat players) seats))
     in fmap (Check.Defenders . DefendersAre.MkDefendersAre) (initOf players <> added)
  Check.Monarch x ->
    let changed = case MonarchIs.player x of
          Just _ -> [Nothing]
          Nothing -> fmap Just (take 1 seats)
     in fmap (Check.Monarch . MonarchIs.MkMonarchIs) changed
  Check.PowerToughness x ->
    [ Check.PowerToughness x {PowerToughnessIs.power = PowerToughnessIs.power x + 1},
      Check.PowerToughness x {PowerToughnessIs.toughness = PowerToughnessIs.toughness x + 1}
    ]
  Check.PlayerCounters x -> fmap (\n -> Check.PlayerCounters x {PlayerCountersAre.count = n}) (naturals (PlayerCountersAre.count x))
  Check.Subtypes x -> fmap (\t -> Check.Subtypes x {SubtypesAre.subtypes = t}) (enumSets (SubtypesAre.subtypes x))
  Check.Names x ->
    let names = NamesAre.names x
     in fmap (\n -> Check.Names x {NamesAre.names = n}) (dropMin names <> [Set.insert perturbedName names])
  Check.View x -> fmap (\r -> Check.View x {ViewIs.is = r}) (replies (ViewIs.is x))
  Check.Keywords x -> fmap (\n -> Check.Keywords x {KeywordsAre.count = n}) (naturals (KeywordsAre.count x))
  Check.ManaValue x -> fmap (\n -> Check.ManaValue x {ManaValueIs.value = n}) (integers (ManaValueIs.value x))

integers :: Integer -> [Integer]
integers n = [n + 1, n - 1]

naturals :: Natural.Natural -> [Natural.Natural]
naturals n = case n of
  0 -> [1]
  _ -> [n + 1, n - 1]

flipped :: TapState.TapState -> TapState.TapState
flipped state = case state of
  TapState.Tapped -> TapState.Untapped
  TapState.Untapped -> TapState.Tapped

-- | A set of an enumeration with its least member dropped, and with the least
-- value it lacks added.
enumSets :: (Bounded a, Enum a, Ord a) => Set.Set a -> [Set.Set a]
enumSets set = dropMin set <> fmap (\x -> Set.insert x set) (take 1 (filter (\x -> Set.notMember x set) [minBound .. maxBound]))

dropMin :: Set.Set a -> [Set.Set a]
dropMin set = foldMap (\(_, rest) -> [rest]) (Set.minView set)

dropOne :: Map.Map k v -> [Map.Map k v]
dropOne entries = foldMap (\(_, rest) -> [rest]) (Map.minView entries)

-- | A name no card has, so a set holding it is never an object's names.
perturbedName :: CardName.CardName
perturbedName = CardName.MkCardName (Text.pack "(perturbed)")

-- | The expected JSON changed at its top: null made true, a boolean flipped, a
-- number off by one, a string lengthened, an array or object with its last
-- element dropped and with one added.
replies :: Reply.Reply -> [Reply.Reply]
replies reply = case reply of
  Reply.Null -> [Reply.Boolean True]
  Reply.Boolean b -> [Reply.Boolean (not b)]
  Reply.Number n -> fmap Reply.Number (integers n)
  Reply.Text t -> [Reply.Text (t <> Text.pack " (perturbed)")]
  Reply.Array xs -> fmap Reply.Array (initOf xs <> [xs <> [Reply.Null]])
  Reply.Object pairs -> fmap Reply.Object (initOf pairs <> [pairs <> [(Text.pack "(perturbed)", Reply.Null)]])

initOf :: [a] -> [[a]]
initOf xs = foldMap (\(front, _) -> [front]) (List.unsnoc xs)
