module Parun.Types.BecomesTargetedTrigger where

import qualified Data.Set as Set
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.TargetedSubject as TargetedSubject
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.Zone as Zone

-- | CR 601.2c: which targets becoming targets fire the ability, by which
-- announcement road, per target or per batch (CR 603.2c), whether only the
-- first such targeting this turn counts, and whether only a keyword's ability
-- (CR 702.165a's backup) does.
--
-- Fields rather than constructors: every one narrows one rule 601.2c
-- announcement and they differ only in how far.
data BecomesTargetedTrigger = MkBecomesTargetedTrigger
  { -- | The targets the condition watches: the bearer (CR 702.21a's ward), a
    -- permanent the Filter admits, read off CR 603.10's sample of the
    -- battlefield at the announcement (Event.becameTarget) or, under `zones`,
    -- a spell on the stack, a player (Dormant
    -- Gomazoa's "you"), or either (Leyline of Combustion, Loki, God of
    -- Mischief).
    subject :: TargetedSubject.TargetedSubject,
    -- | How the TARGETING object's controller (CR 405.4, recorded on
    -- Parun.Types.BecameTarget) stands to CR 109.5's "you". AnyPlayer is
    -- Venerated Rotpriest's unnarrowed "a spell"; Opponent is ward's, Diffusion
    -- Sliver's and Amulet of Safekeeping's.
    relation :: PlayerRelation.PlayerRelation,
    -- | Which of CR 601.2c's announcement roads. Every kind is "a spell or
    -- ability"; Spell alone is Dormant Gomazoa's, so Ravenous Rats' targeted
    -- discard does not untap it; ActivatedAbility alone is Professor Hojo's;
    -- both abilities are Loki, God of Mischief's "an ability" (CR 113.3).
    kinds :: Set.Set StackObjectKind.StackObjectKind,
    -- | Once per targeted permanent (Venerated Rotpriest) or once for the whole
    -- announcement (Professor Hojo's "one or more").
    batching :: Batching.Batching,
    -- | Where a Matching subject's object target is read (CR 115.1): the
    -- battlefield, or the stack too for Surrak, Elusive Hunter's "or a
    -- creature spell you control". A Self subject compares ids and ignores it.
    zones :: Set.Set Zone.Zone,
    -- | FirstTimeEachTurn: only the first time this turn the target became the
    -- target of a spell or ability the other fields admit, counted per object
    -- (valiant, CR 207.2c).
    frequency :: TriggerFrequency.TriggerFrequency,
    -- | CR 702.165a: Just narrows the targeting object to an ability a keyword
    -- of this family represents, whoever bears it (Mirror-Shield Hoplite's "a
    -- backup ability"); Nothing admits any spell or ability.
    sourceKeyword :: Maybe KeywordFamily.KeywordFamily
  }
  deriving (Eq, Ord, Show)

-- | "Whenever this permanent becomes the target of a spell or ability": the
-- bearer, any player's, either road, once per announcement. The codec's
-- defaults.
defaultValue :: BecomesTargetedTrigger
defaultValue =
  MkBecomesTargetedTrigger
    { subject = TargetedSubject.Permanent ActingPermanent.Self,
      relation = PlayerRelation.AnyPlayer,
      kinds = Set.fromList [minBound .. maxBound],
      batching = Batching.EachOccurrence,
      zones = Set.singleton Zone.Battlefield,
      frequency = TriggerFrequency.EveryTime,
      sourceKeyword = Nothing
    }

-- | "Whenever a [permanent] becomes the target of a spell or ability": a
-- permanent the Filter admits, once per permanent.
matching :: Filter.Filter Keyword.Keyword -> BecomesTargetedTrigger
matching f = defaultValue {subject = TargetedSubject.Permanent (ActingPermanent.Matching f)}
