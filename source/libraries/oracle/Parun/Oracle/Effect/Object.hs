-- Instructions to objects, zones and libraries (Parun.Types.Effect) as the
-- predicates Oracle text words them with (CR 608.2c).
module Parun.Oracle.Effect.Object where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Oracle.Continuous as Continuous
import qualified Parun.Oracle.Cost as Cost
import qualified Parun.Oracle.Face as Face.Oracle
import qualified Parun.Oracle.Filter as Filter
import qualified Parun.Oracle.Predicate as Predicate
import qualified Parun.Oracle.Prose as Prose
import qualified Parun.Oracle.Target as Target
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.AmassKind as AmassKind
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.CopyStackObject as CopyStackObject
import qualified Parun.Types.CopyTargets as CopyTargets
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Create as Create
import qualified Parun.Types.Destroy as Destroy
import qualified Parun.Types.DestructionR as DestructionR
import qualified Parun.Types.DestructionRewrite as DestructionRewrite
import qualified Parun.Types.Discover as Discover
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.ForEach as ForEach
import qualified Parun.Types.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LoopMembers as LoopMembers
import qualified Parun.Types.Mill as Mill
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Replace as Replace
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementOrigin as ReplacementOrigin
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
import qualified Parun.Types.Sacrificer as Sacrificer
import qualified Parun.Types.Search as Search
import qualified Parun.Types.SearchDestination as SearchDestination
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.Uses as Uses
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneScope as ZoneScope

-- | "Counter [object]" (CR 701.6a). Any other counter is not spelled out
-- here.
countered :: Target.Context -> Counter.Counter -> [Predicate.Predicate]
countered context c
  | Maybe.isNothing (Counter.slot c),
    Maybe.isNothing (Counter.sources c),
    Maybe.isNothing (Counter.instead c) = do
      ref <- Target.object context Target.All (Counter.ref c)
      [Predicate.MkPredicate Nothing (Text.pack "counter " <> Target.wording ref) [] (Target.mentions ref) Nothing False]
  | otherwise = []

-- | "Copy [spell]. You may choose new targets for the copy" (CR 707.10c),
-- one copy you make, joined as Twincast, Mica, Reader of Ruins or Reality
-- Is Mine to Control prints it. Any other copy renders nothing.
copied :: Target.Context -> CopyStackObject.CopyStackObject ability -> [Predicate.Predicate]
copied context c
  | CopyStackObject.targets c == CopyTargets.ChosenByController,
    CopyStackObject.quantity c == CopyStackObject.defaultQuantity,
    CopyStackObject.copier c == CopyStackObject.defaultCopier,
    null (CopyStackObject.exceptions c),
    Maybe.isNothing (CopyStackObject.slot c) = do
      ref <- Target.object context Target.All (CopyStackObject.ref c)
      joined <- [". You may", " and you may", ", and you may"]
      [Predicate.MkPredicate Nothing (Text.pack "copy " <> Target.wording ref <> Text.pack joined <> Text.pack " choose new targets for the copy") [] (Target.mentions ref) Nothing False]
  | otherwise = []

-- | "Create [tokens]" (CR 111.1). A token that IS a die (Create.die), or one
-- lent triggered abilities (Create.triggeredAbilitiesOf), has no wording
-- yet, so it renders nothing.
created :: Target.Context -> Create.Create Card.Card Continuous.Ability -> [Predicate.Predicate]
created context c
  | Create.riders c == EntryRiders.defaultValue && Maybe.isNothing (Create.die c) && Maybe.isNothing (Create.triggeredAbilitiesOf c) = do
      (one, many) <- Face.Oracle.token (Target.keyword context) (Create.card c)
      a <- Target.amounts context (Create.quantity c)
      let some q = case q of
            Quantity.Literal 1 -> [Prose.article one <> Text.pack " " <> one]
            Quantity.Literal n | n > 1 -> [Prose.cardinal n <> Text.pack " " <> many]
            _ -> []
      (tokens, led) <- case Target.reading a of
        Target.Stated q -> fmap (\t -> (t, [])) (some q)
        -- Free for a count: "for each" (Endless Swarm), "a number of"
        -- (Tyranid Invasion) or "X ..., where X is" (Krenko, Mob Boss).
        -- "For each" leads the sentence too (Crescent Island Temple).
        Target.ForEach n each' -> fmap (\t -> (t <> Text.pack " for each " <> each', [Text.pack "for each " <> each' <> Text.pack ", create " <> t])) (some (Quantity.Literal n))
        Target.EqualTo equal -> fmap (\t -> (t, [])) [Text.pack "a number of " <> many <> Text.pack " equal to " <> equal, Text.pack "X " <> many <> Text.pack ", where X is " <> equal]
      let create = Text.pack "create " <> tokens
          leading = [p {Predicate.verb = l} | l <- led, p <- Predicate.imperative context (Create.creator c) create creates, Predicate.implied p]
          creates = Text.pack "creates " <> tokens
          -- "You create" too, where a trigger on another player's turn
          -- names the creator (Verdant Kraken).
          spoken = do
            (_, person) <- Target.player context (Create.creator c)
            case person of
              Target.Second -> Predicate.declarative context (Create.creator c) create creates
              Target.Third -> []
              Target.ThirdPlural -> []
      Predicate.naming (Target.names a) (Predicate.imperative context (Create.creator c) create creates <> leading <> spoken)
  | otherwise = []

-- | "Put [counters] on [object]" (CR 122.6). A limit on the total, a choice
-- of kinds and a named putter have no wording yet, so each renders nothing.
placed :: Target.Context -> PutCounters.PutCounters -> [Predicate.Predicate]
placed context p
  | Maybe.isNothing (PutCounters.totalAtMost p),
    Maybe.isNothing (PutCounters.putter p),
    null (PutCounters.alternatives p) = do
      ref <- Target.object context Target.Each (PutCounters.ref p)
      a <- Target.amounts context (PutCounters.quantity p)
      let on = Text.pack " on " <> Target.wording ref
      phrase <- case Target.reading a of
        -- Free for a count: "for each" (Khabal Ghoul), "a number of"
        -- (Rhovanion Rampager) or "X ..., where X is" (Southern Air Temple).
        Target.Stated q -> fmap (\put -> Text.pack "put " <> put <> on) (counters (Target.keyword context) (PutCounters.kind p) q)
        Target.ForEach n each' -> fmap (\put -> Text.pack "put " <> put <> on <> Text.pack " for each " <> each') (counters (Target.keyword context) (PutCounters.kind p) (Quantity.Literal n))
        Target.EqualTo equal -> do
          word <- Cost.counterWord (Target.keyword context) (PutCounters.kind p)
          [ Text.pack "put a number of " <> word <> Text.pack " counters" <> on <> Text.pack " equal to " <> equal,
            Text.pack "put X " <> word <> Text.pack " counters" <> on <> Text.pack ", where X is " <> equal
            ]
      Predicate.MkPredicate Nothing phrase [] (Target.mentions ref <> Target.names a) Nothing False : supported context p
  | otherwise = []

-- | "Tap [objects]" (CR 701.26a). Any other tap is not spelled out here.
tapped :: Target.Context -> Tap.Tap -> [Predicate.Predicate]
tapped context t = case t of
  Tap.MkTap r Nothing Nothing -> acted context (Text.pack "tap ") r
  _ -> []

-- | "Sacrifice this creature" (CR 701.21a): one object, which the
-- instruction's controller sacrifices. A sweep is not spelled out here.
sacrificed :: Target.Context -> SacrificeEffect.SacrificeEffect -> [Predicate.Predicate]
sacrificed context s = case s of
  SacrificeEffect.MkSacrificeEffect r@(ObjectRef.InSlot _) Sacrificer.EffectController Nothing Nothing -> acted context (Text.pack "sacrifice ") r
  _ -> []

-- | "Untap [objects]" (CR 701.26b). Free over several: "all" or "each" (Rally
-- the Troops, Veteran Beastrider).
untapped :: Target.Context -> ObjectRef.ObjectRef -> [Predicate.Predicate]
untapped context r = do
  determiner <- [Target.All, Target.Each]
  ref <- Target.object context determiner r
  [Predicate.MkPredicate Nothing (Text.pack "untap " <> Target.wording ref) [] (Target.mentions ref) Nothing False]

-- | "Regenerate this creature" (CR 701.19a): a shield for the rest of the
-- turn, used up by the one destruction it replaces, on the source or on
-- the object a slot holds.
regenerated :: Target.Context -> Replace.Replace card Continuous.Ability effect -> [Predicate.Predicate]
regenerated context r = case r of
  Replace.MkReplace Duration.UntilEndOfTurn Uses.Once ReplacementOrigin.Other Nothing (ReplacementEffect.DestructionR (DestructionR.MkDestructionR matching DestructionRewrite.Regenerate)) -> do
    ref <- case matching of
      Nothing -> [ObjectRef.InSlot Target.selfSlot]
      Just (Filter.Type.IsBound s) -> [ObjectRef.InSlot s]
      _ -> []
    acted context (Text.pack "regenerate ") ref
  _ -> []

-- | "Regenerate enchanted creature": the same shield on the permanent a loop
-- binds as the instruction resolves. Free over several: "all" or "each"
-- (Dark Dabbling's "each other creature you control").
regeneratedEach :: Target.Context -> ForEach.ForEach (Effect.Effect card Continuous.Ability) -> [Predicate.Predicate]
regeneratedEach context f = case f of
  ForEach.MkForEach r LoopMembers.Every slot body False Nothing Nothing
    | [Effect.Replace (Replace.MkReplace Duration.UntilEndOfTurn Uses.Once ReplacementOrigin.Other Nothing (ReplacementEffect.DestructionR (DestructionR.MkDestructionR (Just (Filter.Type.IsBound bound)) DestructionRewrite.Regenerate)))] <- Foldable.toList body,
      bound == slot -> do
        determiner <- [Target.All, Target.Each]
        ref <- Target.object context determiner r
        [Predicate.MkPredicate Nothing (Text.pack "regenerate " <> Target.wording ref) [] (Target.mentions ref) Nothing False]
  _ -> []

-- | "Target player mills two cards" (CR 701.17a).
milled :: Target.Context -> Mill.Mill -> [Predicate.Predicate]
milled context m
  | Maybe.isNothing (Mill.tally m),
    Maybe.isNothing (Mill.slot m) = do
      n <- Target.cards (Mill.quantity m)
      let mill = Text.pack "mill " <> n
          mills = Text.pack "mills " <> n
          -- "You mill" too, where a trigger names the miller (Festerleech).
          spoken = do
            (_, person) <- Target.player context (Mill.player m)
            case person of
              Target.Second -> Predicate.declarative context (Mill.player m) mill mills
              Target.Third -> []
              Target.ThirdPlural -> []
      Predicate.imperative context (Mill.player m) mill mills <> spoken
  | otherwise = []

-- | "Empower Jace 2" (CR 701.71a). Amassing (CR 701.47a) is not spelled out
-- here.
amassed :: Target.Context -> Amass.Amass -> [Predicate.Predicate]
amassed context a = case a of
  Amass.MkAmass who q AmassKind.EmpowerJace Nothing -> do
    n <- Target.amount q
    Predicate.imperative context who (Text.pack "empower Jace " <> n) (Text.pack "empowers Jace " <> n)
  _ -> []

-- | "Discover 4" (CR 701.57a).
discovered :: Target.Context -> Discover.Discover -> [Predicate.Predicate]
discovered context (Discover.MkDiscover who q _) = do
  n <- Target.amount q
  Predicate.imperative context who (Text.pack "discover " <> n) (Text.pack "discovers " <> n)

-- | "Scry 2" (CR 701.22a).
scried :: Target.Context -> PlayerQuantity.PlayerQuantity -> [Predicate.Predicate]
scried context (PlayerQuantity.MkPlayerQuantity who q) = do
  n <- Target.amount q
  Predicate.imperative context who (Text.pack "scry " <> n) (Text.pack "scries " <> n)

-- | CR 509.1b: "Target creature can't be blocked this turn" (Zhuge Jin, Wu
-- Strategist), the restriction on the objects a slot names. The duration
-- follows the blockers ("by Walls this turn") but comes before an exception
-- (Jovens' Tools' "this turn except by Walls").
unblockable :: Target.Context -> ForbidBeingBlocked.ForbidBeingBlocked -> [Predicate.Predicate]
unblockable context f
  | Filter.Type.IsBound slot <- ForbidBeingBlocked.affected f,
    ForbidBeingBlocked.duration f == Duration.UntilEndOfTurn = do
      who <- Target.object context Target.Affected (ObjectRef.InSlot slot)
      let blockers = ForbidBeingBlocked.blockers f
          thisTurn = Text.pack " this turn"
      by <- Continuous.blockedBy context blockers
      let verb' = case blockers of
            Filter.Type.Not _ -> Text.pack "can't be blocked" <> thisTurn <> by
            _ -> Text.pack "can't be blocked" <> by <> thisTurn
      [Predicate.MkPredicate (Just who) verb' [] [] Nothing False]
  | otherwise = []

-- | "Destroy [objects]" (CR 701.8a), the objects one instruction destroys at
-- once (CR 608.2f) listed in one sentence: "destroy that creature and this
-- creature" (Loyal Sentry). Free for two single objects, one of them the
-- source: listed, or "both creatures" (Alaborn Zealot).
-- Any other destruction is not spelled out here.
destroyed :: Target.Context -> Destroy.Destroy -> [Predicate.Predicate]
destroyed context d
  | Maybe.isJust (Destroy.slot d) || Maybe.isJust (Destroy.buried d) || Maybe.isJust (Destroy.permanents d) = []
  | otherwise = do
      refs <- case Target.object context Target.All (Destroy.ref d) of
        [] | ObjectRef.AllOf members <- Destroy.ref d -> traverse (Target.object context Target.All) (NonEmpty.toList members)
        whole -> fmap pure whole
      let many = length refs > 1 || any ((== Filter.Plural) . Target.number) refs
          -- Free for one object named again: "It" (Terror) or "That creature"
          -- (Nekrataal).
          again = case Target.object context {Target.earlier = [Destroy.ref d]} Target.All (Destroy.ref d) of
            [] -> [Text.pack "it"]
            anaphors -> fmap Target.wording anaphors
      regenerate <- case Destroy.regenerability d of
        Regenerability.Regenerable -> [[]]
        Regenerability.CantBeRegenerated
          | many -> [[Text.pack "They can't be regenerated."]]
          | otherwise -> fmap (\it -> [Prose.sentence (it <> Text.pack " can't be regenerated")]) again
      let both = case fmap Target.wording refs of
            [a, b]
              | all ((== Filter.Singular) . Target.number) refs,
                Just noun <- Text.stripPrefix (Text.pack "that ") a,
                b == Target.self context ->
                  [Text.pack "both " <> Filter.pluralOf noun]
            _ -> []
      objects <- Prose.listing "and" (fmap Target.wording refs) : both
      [Predicate.MkPredicate Nothing (Text.pack "destroy " <> objects) regenerate (concatMap Target.mentions refs) Nothing False]

-- | "Return [objects] to their owners' hands", listed in one sentence (Peel from
-- Reality).
returned :: Target.Context -> [ObjectRef.ObjectRef] -> [Predicate.Predicate]
returned context objects = do
  refs <- traverse (Target.object context Target.All) objects
  [Predicate.MkPredicate Nothing (Text.pack "return " <> Prose.listing "and" (fmap Target.wording refs) <> Text.pack " to their owners' hands") [] (concatMap Target.mentions refs) Nothing False]

-- | "[Return/Exile/Put] [objects] [to/into/on] [zone]": a zone change (CR
-- 400.7) as Oracle text words it by where the objects go. A card returns to a
-- hand, and to the battlefield only from your graveyard; any other move is
-- "put", and exiling is its own verb (CR 701.13a). A move with riders other
-- than entering tapped, a move "until" something happens (CR 610.3), or a
-- move onto the battlefield from anywhere else is not spelled out here.
moved :: Target.Context -> MoveToZone.MoveToZone Continuous.Ability -> [Predicate.Predicate]
moved context m
  | Maybe.isJust (MoveToZone.duration m) = []
  | otherwise = do
      let ref = MoveToZone.ref m
          itself = case ref of
            ObjectRef.InSlot s -> s == Target.selfSlot || Map.lookup s (Target.bound context) == Just (Target.self context)
            _ -> False
          graveyard
            | itself && MoveToZone.origin m == Just Zone.Graveyard = Just PlayerRelation.You
            | ObjectRef.InSlot s <- ref, elem s (Target.owned context) = Just PlayerRelation.You
            | otherwise = graveyardOf context ref
          yours = graveyard == Just PlayerRelation.You
          riders = MoveToZone.riders m
          plain = riders == EntryRiders.defaultValue
      (who, number) <- case ref of
        -- Free for a description: "all", agreeing in the plural (Evacuation),
        -- or "each", in the singular (Denizen of the Deep).
        ObjectRef.EachMatching _ -> do
          everything <- Target.object context Target.All ref
          single <- Target.object context Target.Each ref
          [(everything, Target.number everything), (single, Filter.Singular)]
        _ -> fmap (\r -> (r, Target.number r)) (Target.object context Target.All ref)
      from <- case MoveToZone.origin m of
        Nothing -> [Text.empty]
        Just Zone.Graveyard
          | itself -> [Text.pack " from your graveyard"]
          | Maybe.isJust graveyard -> [Text.empty]
        _ -> []
      let what = Target.wording who <> from
      phrase <- case MoveToZone.zone m of
        Zone.Hand | plain -> do
          zone <- owner graveyard number (Text.pack "hand")
          [Text.pack "return " <> what <> Text.pack " to " <> zone]
        Zone.Battlefield
          | yours,
            plain || riders == EntryRiders.defaultValue {EntryRiders.tapped = TapState.Tapped} ->
              [Text.pack "return " <> what <> Text.pack " to the battlefield" <> (if plain then Text.empty else Text.pack " tapped")]
        Zone.Exile | plain -> [Text.pack "exile " <> what]
        Zone.Graveyard
          | plain,
            Maybe.isNothing graveyard -> do
              zone <- owner Nothing number (Text.pack "graveyard")
              [Text.pack "put " <> what <> Text.pack " into " <> zone]
        Zone.Library | plain -> do
          zone <- owner graveyard number (Text.pack "library")
          case MoveToZone.placement m of
            LibraryPlacement.Stated LibraryPosition.Top -> [Text.pack "put " <> what <> Text.pack " on top of " <> zone]
            LibraryPlacement.Stated LibraryPosition.Bottom -> [Text.pack "put " <> what <> Text.pack " on the bottom of " <> zone]
            LibraryPlacement.Beneath (Quantity.Literal n) -> do
              place <- Prose.ordinal (n + 1)
              [Text.pack "put " <> what <> Text.pack " into " <> zone <> Text.pack " " <> place <> Text.pack " from the top"]
            _ -> []
        _ -> []
      [Predicate.MkPredicate Nothing phrase [] (Target.mentions who) Nothing False]

-- | Whose graveyard the cards the reference names are in, if they are in one
-- player's graveyard: yours, or an opponent's.
graveyardOf :: Target.Context -> ObjectRef.ObjectRef -> Maybe PlayerRelation.PlayerRelation
graveyardOf context ref =
  let whose scope = case scope of
        ZoneScope.Scoped (PlayerScope.Related relation)
          | elem relation [PlayerRelation.You, PlayerRelation.Opponent] -> Just relation
        _ -> Nothing
   in case ref of
        ObjectRef.InSlot s -> case fmap TargetSlot.pool (Map.lookup s (Target.slots context)) of
          Just (Pool.CardsInGraveyard scope) -> whose scope
          _ -> Nothing
        ObjectRef.EachCardInGraveyard e -> whose (EachCardInGraveyard.graveyards e)
        _ -> Nothing

-- | A zone of the given objects' owners (CR 400.3): "your hand" for cards from
-- your graveyard, "their hand" for several from an opponent's, and otherwise
-- "its owner's hand" or, for several, "their owners' hands" or "their owner's
-- hand". Free for several: Evacuation's or Retract's.
owner :: Maybe PlayerRelation.PlayerRelation -> Filter.Number -> Text.Text -> [Text.Text]
owner graveyard number zone = case (graveyard, number) of
  (Just PlayerRelation.You, _) -> [Text.pack "your " <> zone]
  (Just PlayerRelation.Opponent, Filter.Plural) -> [Text.pack "their " <> zone]
  (_, Filter.Singular) -> [Text.pack "its owner's " <> zone]
  (_, Filter.Plural) -> [Text.pack "their owners' " <> Filter.pluralOf zone, Text.pack "their owner's " <> zone]

-- | "a +1/+1 counter", "two -1/-1 counters", "a flying counter" (CR 122.1).
counters :: (Keyword.Keyword -> [Text.Text]) -> CounterKind.CounterKind Keyword.Keyword -> Quantity.Quantity -> [Text.Text]
counters named kind q = do
  word <- Cost.counterWord named kind
  case q of
    Quantity.Literal 1 -> [Prose.article word <> Text.pack " " <> word <> Text.pack " counter"]
    Quantity.Literal n | n > 1 -> [Prose.cardinal n <> Text.pack " " <> word <> Text.pack " counters"]
    Quantity.InSlot s | s == Target.xSlot -> [Text.pack "X " <> word <> Text.pack " counters"]
    _ -> []

-- | "Support N" (CR 701.41a), the rules' name for putting a +1/+1 counter on
-- each of up to N other target creatures, or on a spell up to N target
-- creatures; this tool lets a paragraph break only a spell's instructions
-- ('Target.breaks').
-- Free: the instruction (Travel Preparations) or its name (Lead by Example).
supported :: Target.Context -> PutCounters.PutCounters -> [Predicate.Predicate]
supported context p = case PutCounters.ref p of
  ObjectRef.InSlot s
    | PutCounters.kind p == CounterKind.PlusOnePlusOne,
      PutCounters.quantity p == Quantity.Literal 1,
      Just slot <- Map.lookup s (Target.slots context),
      TargetSlot.pool slot == Pool.Creatures,
      SlotCount.Printed (TargetCount.MkTargetCount 0 (Just n)) <- TargetSlot.count slot,
      TargetSlot.filter slot == (if elem (Text.pack "\n") (Target.breaks context) then Nothing else Just (Filter.Type.Not Filter.Type.IsSource)),
      Maybe.isNothing (TargetSlot.chooser slot),
      Maybe.isNothing (TargetSlot.perPlayer slot),
      Maybe.isNothing (TargetSlot.together slot),
      Maybe.isNothing (TargetSlot.amount slot),
      Maybe.isNothing (TargetSlot.total slot),
      Maybe.isNothing (TargetSlot.divided slot),
      TargetSlot.selection slot == CardSelection.ByChoice ->
        [Predicate.MkPredicate Nothing (Text.pack ("support " <> show n)) [] [s] Nothing False]
  _ -> []

-- | An instruction doing one thing to objects: "Tap target creature",
-- "Untap all creatures you control", "Sacrifice this creature".
acted :: Target.Context -> Text.Text -> ObjectRef.ObjectRef -> [Predicate.Predicate]
acted context word r = do
  ref <- Target.object context Target.All r
  [Predicate.MkPredicate Nothing (word <> Target.wording ref) [] (Target.mentions ref) Nothing False]

-- | "Search your library for a card, put that card into your hand, then
-- shuffle" (CR 701.23a): your search of your own library for one card of a
-- description, and where the card goes, after which the search shuffles
-- unless the card stays (Parun.Types.SearchDestination). Free: "that card"
-- or "it" for the card put into your hand (Diabolic Tutor, Rune-Scarred
-- Demon) or onto the battlefield (Nature's Lore, Three Visits), "it" or "that card" for the card revealed (Summoner's Pact,
-- Goblin Matron), and "that card", "the card" or "it" for the card put on
-- top (Sylvan Tutor, Worldly Tutor, Silver Deputy). Several descriptions
-- searched for at once (CR 701.23h) are listed, one card each, and "them" put into your hand or
-- onto the battlefield tapped: Gem of Becoming, Krosan Verge. Any other search
-- is not spelled out here.
searched :: Target.Context -> Search.Search -> [Predicate.Predicate]
searched context s
  | plain,
    Search.quantity s == Just (Quantity.Literal 1),
    Seq.null (Search.descriptions s) = do
      one <- card (Search.filter s)
      let onto = [Text.pack "put that card onto the battlefield", Text.pack "put it onto the battlefield"]
          revealed = [Text.pack "reveal it", Text.pack "reveal that card"]
          onTop = [Text.pack "that card", Text.pack "the card", Text.pack "it"]
      gone <- case Search.destination s of
        SearchDestination.Hand -> [Text.pack ", put that card into your hand, then shuffle", Text.pack ", put it into your hand, then shuffle"]
        SearchDestination.RevealThenHand -> fmap (\r -> Text.pack ", " <> r <> Text.pack ", put it into your hand, then shuffle") revealed
        -- Free besides: "and" and a sentence of its own for the shuffle
        -- (Spoils of Victory), commas (Nature's Lore), or "shuffle your
        -- library" (Perhaps You've Met My Cohort).
        SearchDestination.Battlefield -> fmap (\o -> Text.pack ", " <> o <> Text.pack ", then shuffle") onto <> [Text.pack " and put that card onto the battlefield. Then shuffle", Text.pack ", put it onto the battlefield, then shuffle your library"]
        SearchDestination.BattlefieldTapped -> fmap (\o -> Text.pack ", " <> o <> Text.pack " tapped, then shuffle") onto
        SearchDestination.Top -> fmap (\c -> Text.pack ", then shuffle and put " <> c <> Text.pack " on top") onTop
        SearchDestination.RevealThenTop -> fmap (\c -> Text.pack ", reveal it, then shuffle and put " <> c <> Text.pack " on top") onTop
        _ -> []
      [Predicate.MkPredicate Nothing (Text.pack "search your library for " <> Prose.article one <> Text.pack " " <> one <> gone) [] [] Nothing False]
  | plain,
    Search.filter s == Filter.Type.And [],
    descriptions@(_ : _ : _) <- Foldable.toList (Search.descriptions s),
    Search.quantity s == Just (Quantity.Literal (List.genericLength descriptions)) = do
      ones <- traverse card descriptions
      gone <- case Search.destination s of
        SearchDestination.RevealThenHand -> [Text.pack ". Reveal those cards, put them into your hand, then shuffle"]
        SearchDestination.BattlefieldTapped -> [Text.pack ", put them onto the battlefield tapped, then shuffle"]
        _ -> []
      [Predicate.MkPredicate Nothing (Text.pack "search your library for " <> Prose.listing "and" [Prose.article one <> Text.pack " " <> one | one <- ones] <> gone) [] [] Nothing False]
  | otherwise = []
  where
    you = PlayerRef.Relative PlayerRelation.You
    plain =
      Search.searcher s == you
        && Search.owner s == you
        && Search.zones s == Set.singleton Zone.Library
        && not (Search.outsideTheGame s)
        && not (Search.upTo s)
        && Map.null (Search.counters s)
        && Maybe.isNothing (Search.subject s)
        && Maybe.isNothing (Search.slot s)
        && Maybe.isNothing (Search.handSlot s)
        && Maybe.isNothing (Search.librarySlot s)
        && Set.null (Search.differentIn s)
        && not (Search.exactly s)
        && not (Search.andOr s)
        && Maybe.isNothing (Search.top s)
        && Search.selection s == CardSelection.ByChoice
    card f =
      described f <> case f of
        -- Free: "card" after each description, with an article on the later
        -- ones (Mystical Teachings) or without (Final Frontier).
        Filter.Type.Or fs@(_ : _ : _) -> do
          first : rest <- traverse described fs
          articled <- [False, True]
          [Prose.listing "or" (first : fmap (\d -> if articled then Prose.article d <> Text.pack " " <> d else d) rest)]
        _ -> []
    described f = do
      p <- Filter.phrase f (Target.keyword context)
      Filter.describe Filter.Singular Target.card (Filter.merge p (Filter.MkPhrase [] [Target.card] []))
