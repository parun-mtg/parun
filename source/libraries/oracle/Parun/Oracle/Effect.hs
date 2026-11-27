-- Instructions (Parun.Types.Effect) as the sentences Oracle text prints them
-- in, and the clauses, modes and modal payloads that hold them (CR 608.2c,
-- CR 700.2).
module Parun.Oracle.Effect where

import qualified Control.Monad as Monad
import qualified Data.Containers.ListUtils as ListUtils
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
import qualified Parun.Oracle.Effect.Object as Object
import qualified Parun.Oracle.Effect.Player as Player
import qualified Parun.Oracle.Filter as Filter
import qualified Parun.Oracle.Keyword as Keyword.Oracle
import qualified Parun.Oracle.Predicate as Predicate
import qualified Parun.Oracle.Prose as Prose
import qualified Parun.Oracle.Target as Target
import qualified Parun.Oracle.Trigger as Trigger
import qualified Parun.Types.AgainstSlot as AgainstSlot
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ChooseBetween as ChooseBetween
import qualified Parun.Types.ChooseInstead as ChooseInstead
import qualified Parun.Types.ChoosePawprints as ChoosePawprints
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.CoinReading as CoinReading
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.DamagePart as DamagePart
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.Destroy as Destroy
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.FlipCoin as FlipCoin
import qualified Parun.Types.IfTaken as IfTaken
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaRetention as ManaRetention
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeMemory as ModeMemory
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Optionality as Optionality
import qualified Parun.Types.OrElse as OrElse
import qualified Parun.Types.PayBranch as PayBranch
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PayObligation as PayObligation
import qualified Parun.Types.PayerGrouping as PayerGrouping
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.PreventNextDamage as PreventNextDamage
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.Zone as Zone

-- | A modal payload's text: its one mode, or the modes as a bulleted list
-- after the instruction to choose (CR 700.2), each mode one line. Modes that
-- each add mana read as one choice of mana: "Add {R} or {W}." (Skyshroud
-- Elf), and modes that each add mana and then do the same thing read as that
-- choice and then the rest: "Add {W} or {U}. This land deals 1 damage to
-- you." (Adarkar Wastes). Not rendered: shared clauses (Modal.shared).
modal :: Target.Context -> Modal.Modal Card.Card Continuous.Ability -> [Text.Text]
modal context m = case (Foldable.toList (Modal.modes m), Modal.selection m) of
  _ | not (null (Modal.shared m)) -> []
  ([only], ModeSelection.ChooseExactly 1) | Modal.memory m == ModeMemory.NoMemory -> mode context only
  (modes@(_ : _ : _), selection)
    | Just added' <- traverse manaMode modes,
      (selection, Modal.memory m) == (ModeSelection.ChooseExactly 1, ModeMemory.NoMemory) ->
        [Prose.sentence (Text.pack "add " <> Prose.listing "or" added')]
    | (selection, Modal.memory m) == (ModeSelection.ChooseExactly 1, ModeMemory.NoMemory),
      Just split <- traverse manaThen modes,
      (_, rest) : others <- split,
      all ((== rest) . snd) others -> do
        following <- mode context rest
        followedBy context (Prose.sentence (Text.pack "add " <> Prose.listing "or" (fmap fst split))) following
    | otherwise -> do
        heading <- choose context (Modal.memory m) selection (length modes)
        bodies <- traverse (mode context {Target.breaks = [Text.pack " "]}) modes
        [heading <> mconcat (zipWith (\b body -> Text.pack "\n" <> b <> body) (bullets selection) bodies)]
  _ -> []

-- | The instruction to choose modes (CR 700.2), given how many there are:
-- "Choose one \8212", "Choose one or both \8212", "Choose three. You may
-- choose the same mode more than once." (CR 700.2d), "Choose up to four. You
-- may choose the same mode more than once.", "Choose up to five {P} worth of
-- modes. You may choose the same mode more than once." (CR 700.2i), "Choose
-- one that hasn't been chosen \8212", "Choose one that hasn't been chosen this
-- turn \8212", and CR 700.2's instruction replaced while a condition holds:
-- "Choose one. If you have exactly 13 life, you may choose both instead."
-- (Disciple of Perdition), a spell's board condition read with or without "as
-- you cast this spell" (Flame of Anor; See Double prints neither).
choose :: Target.Context -> ModeMemory.ModeMemory -> ModeSelection.ModeSelection -> Int -> [Text.Text]
choose context memory selection count = case (memory, selection) of
  (ModeMemory.ObjectLifetime, ModeSelection.ChooseExactly n) -> [Text.pack "Choose " <> Prose.cardinal (toInteger n) <> Text.pack " that hasn't been chosen \8212"]
  (ModeMemory.ObjectLifetime, _) -> []
  (ModeMemory.ThisTurn, ModeSelection.ChooseExactly n) -> [Text.pack "Choose " <> Prose.cardinal (toInteger n) <> Text.pack " that hasn't been chosen this turn \8212"]
  (ModeMemory.ThisTurn, _) -> []
  (ModeMemory.NoMemory, _) -> case selection of
    ModeSelection.ChooseExactly n -> [Text.pack "Choose " <> Prose.cardinal (toInteger n) <> Text.pack " \8212"]
    ModeSelection.ChooseExactlyWithRepeats n -> [Text.pack "Choose " <> Prose.cardinal (toInteger n) <> Text.pack ". You may choose the same mode more than once."]
    ModeSelection.ChooseBetween b
      | ChooseBetween.least b == 1, toInteger (ChooseBetween.most b) == toInteger count -> [Text.pack (if count == 2 then "Choose one or both \8212" else "Choose one or more \8212")]
    ModeSelection.ChooseBetweenWithRepeats b
      | ChooseBetween.least b == 0 -> [Text.pack "Choose up to " <> Prose.cardinal (toInteger (ChooseBetween.most b)) <> Text.pack ". You may choose the same mode more than once."]
    ModeSelection.ChooseUpTo (Quantity.InSlot slot)
      | slot == Target.xSlot -> [Text.pack "Choose up to X \8212"]
    ModeSelection.ChoosePawprintsWithRepeats cp -> [Text.pack "Choose up to " <> Prose.cardinal (toInteger (ChoosePawprints.most cp)) <> Text.pack " {P} worth of modes. You may choose the same mode more than once."]
    ModeSelection.ChooseInstead ci
      | ModeSelection.ChooseExactly n <- ChooseInstead.ordinary ci -> do
          (words', _) <- condition context (ChooseInstead.condition ci)
          casting <- Text.empty : [Text.pack " as you cast this spell" | elem (Text.pack "\n") (Target.breaks context)]
          widened <- case ChooseInstead.instead ci of
            ModeSelection.ChooseExactly k | k > n -> [Text.pack "choose " <> many k]
            ModeSelection.ChooseBetween b | ChooseBetween.least b == 1, n == 1, toInteger (ChooseBetween.most b) == toInteger count, count > 2 -> [Text.pack "choose one or more"]
            ModeSelection.ChooseBetween b | ChooseBetween.least b == n, ChooseBetween.most b > n -> [Text.pack "you may choose " <> many (ChooseBetween.most b)]
            _ -> []
          [Text.pack "Choose " <> Prose.cardinal (toInteger n) <> Text.pack ". If " <> words' <> casting <> Text.pack ", " <> widened <> Text.pack " instead."]
    _ -> []
  where
    many k = if count == 2 && k == 2 then Text.pack "both" else Prose.cardinal (toInteger k)

-- | What precedes each mode, in printed order: a bullet, or under CR 700.2i
-- the mode's pawprints (CR 107.18).
bullets :: ModeSelection.ModeSelection -> [Text.Text]
bullets selection = case selection of
  ModeSelection.ChoosePawprintsWithRepeats cp -> fmap (\n -> Text.concat (List.genericReplicate n (Text.pack "{P}")) <> Text.pack " \8212 ") (Foldable.toList (ChoosePawprints.pawprints cp))
  _ -> repeat (Text.pack "\8226 ")

-- | The symbols a mode adds, when adding mana is all it does and it names no
-- target.
manaMode :: Mode.Mode Card.Card Continuous.Ability -> Maybe Text.Text
manaMode m = case (Foldable.toList (Mode.clauses m), Map.null (Mode.targetSlots m)) of
  ([c], True)
    | c == (unridden c) {Clause.effects = Clause.effects c},
      Just ps <- traverse addition (Foldable.toList (Clause.effects c)),
      [Predicate.MkPredicate {Predicate.mana = Just (Predicate.Symbols symbols)}] <- List.foldr addedTogether [] ps ->
        Just symbols
  _ -> Nothing
  where
    addition e = case e of
      Effect.AddMana a
        | ManaAddition.player a == PlayerRef.Relative PlayerRelation.You,
          ManaAddition.retention a == ManaRetention.Ordinary,
          Maybe.isNothing (ManaAddition.restriction a),
          Maybe.isNothing (ManaAddition.rider a),
          Maybe.isNothing (ManaAddition.whenSpent a),
          [(Predicate.Symbols symbols, _)] <- Player.added (ManaAddition.production a) (ManaAddition.count a) ->
            Just (Predicate.MkPredicate Nothing (Text.pack "add " <> symbols) [] [] (Just (Predicate.Symbols symbols)) False)
      _ -> Nothing

-- | A mode whose first instruction adds mana and which does more after it,
-- split there: the symbols that instruction adds, and the mode without it.
manaThen :: Mode.Mode Card.Card Continuous.Ability -> Maybe (Text.Text, Mode.Mode Card.Card Continuous.Ability)
manaThen m = case Seq.viewl (Mode.clauses m) of
  c Seq.:< cs
    | e Seq.:< es <- Seq.viewl (Clause.effects c),
      Just symbols <- manaMode m {Mode.clauses = Seq.singleton c {Clause.effects = Seq.singleton e}} ->
        let rest = if Seq.null es then cs else c {Clause.effects = es} Seq.<| cs
         in if Seq.null rest then Nothing else Just (symbols, m {Mode.clauses = rest})
  _ -> Nothing

-- | The clause with every rider dropped: mandatory and ungated.
unridden :: Clause.Clause card ability -> Clause.Clause card ability
unridden c = c {Clause.ifTaken = Nothing, Clause.condition = Nothing, Clause.orElse = Nothing, Clause.optionality = Optionality.Mandatory, Clause.payGate = Nothing}

-- | A mode's sentences, each of its target slots named exactly once (CR
-- 601.2c): a slot named twice, or never, means the text is not the mode's.
-- A spell's sentences are separated as the context's breaks allow.
mode :: Target.Context -> Mode.Mode Card.Card Continuous.Ability -> [Text.Text]
mode context m = named' <> chosen
  where
    inner = context {Target.slots = Mode.targetSlots m}
    clauses' = zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses m))
    named' = do
      rendered <- steps inner clauses'
      Monad.guard (List.sort (concatMap snd rendered) == Map.keys (Mode.targetSlots m))
      case fmap fst rendered of
        [] -> [Text.empty]
        first : rest -> Monad.foldM (followedBy context) first rest
    -- A mode's one player target that its first instruction, unconditioned,
    -- does not read is announced first, and "the chosen player" after (Haunt
    -- the Network's "Choose target opponent. Create two ... Then the chosen
    -- player loses X life"). Free: so announced, or named where an
    -- instruction reads it.
    chosen = do
      [(s, slot)] <- [Map.toList (Mode.targetSlots m)]
      (_, c) : _ <- [clauses']
      e Seq.:<| _ <- [Clause.effects c]
      -- The first instruction must render without the slot: one that renders
      -- nothing names no slot only by failing (Archon of Cruelty's "target
      -- opponent sacrifices ...").
      let alone = clause inner c {Clause.effects = Seq.singleton e}
      Monad.guard (TargetSlot.pool slot == Pool.Players && c == unridden c && not (null alone) && notElem s (concatMap snd alone))
      announced <- Target.target inner s
      rendered <- steps inner {Target.bound = Map.insert s (Text.pack "the chosen player") (Target.bound inner)} clauses'
      Monad.guard (all (null . snd) rendered && any (Text.isInfixOf (Text.pack "the chosen player") . fst) rendered)
      first : rest <- [fmap fst rendered]
      body <- Monad.foldM (followedBy context) first rest
      separator <- Target.breaks context
      [Prose.sentence (Text.pack "choose " <> Target.wording announced) <> separator <> body]

-- | A mode's clauses as the steps its text takes, each with the slots it
-- names. A clause hanging off the one before it reads with it (CR 608.2c):
-- "You may discard a card. If you do, draw a card." (Tweeze), "sacrifice it
-- unless you discard a card at random" (Pillaging Horde), and a pair of which
-- you choose one (CR 608.2d) as "You may tap or untap target permanent."
-- (Twiddle). A pair gated on a count at most N and at least N + 1 reads as
-- the first, then "If ..., ... instead." (Leafkin Druid) or "... instead if
-- ...." (Take the Fall).
steps :: Target.Context -> [(ClauseIndex.ClauseIndex, Clause.Clause Card.Card Continuous.Ability)] -> [[(Text.Text, [SlotName.SlotName])]]
steps context cs = case cs of
  [] -> [[]]
  (_, c) : (_, d) : rest
    | Just (Condition.Compares (Compares.MkCompares measured Comparison.AtMost (Quantity.Literal below))) <- Clause.condition c,
      Clause.condition d == Just (Condition.Compares (Compares.MkCompares measured Comparison.AtLeast (Quantity.Literal (below + 1)))) -> do
        (first, ms) <- clause context c {Clause.condition = Nothing}
        -- The second replaces the first, so its pump is not "an additional"
        -- one (You're Not Alone).
        let replacing = (noted context [c]) {Target.pumped = Target.pumped context}
            led = do
              (body, ns) <- clause replacing d
              body' <- Foldable.toList (Text.stripSuffix (Text.pack ".") body)
              [(body' <> Text.pack " instead.", ns)]
            -- Free: "instead if" and the condition trail the instructions
            -- (Take the Fall).
            trailed = do
              gate <- Foldable.toList (Clause.condition d)
              (words', itself) <- condition replacing gate
              -- Free: an ability word heads it (Join the Dead's descend 4).
              word <- Trigger.abilityWord words'
              (body, ns) <- clause (if itself then replacing {Target.self = Text.pack "it"} else replacing) d {Clause.condition = Nothing}
              body' <- Foldable.toList (Text.stripSuffix (Text.pack ".") body)
              [(word <> body' <> Text.pack " instead if " <> words' <> Text.pack ".", ns)]
        second <- led <> trailed
        fmap ([(first, ms), second] <>) (steps (noted context [c, d]) rest)
  (i, c) : (j, d) : rest
    | Clause.ifTaken d == Just (IfTaken.AnyTaken (NonEmpty.singleton i)) -> do
        (first, ms) <- clause context c
        (body, ns) <- clause (noted context [c]) d {Clause.ifTaken = Nothing}
        fmap ((first <> Text.pack " If you do, " <> lowered context body, ms <> ns) :) (steps (noted context [c, d]) rest)
    | Clause.ifTaken d == Just (IfTaken.NoneTaken (NonEmpty.singleton i)),
      Optionality.Optional (PlayerRef.Relative PlayerRelation.You) _ <- Clause.optionality c,
      c == (unridden c) {Clause.optionality = Clause.optionality c},
      d == (unridden d) {Clause.ifTaken = Clause.ifTaken d} -> do
        p <- predicate context c
        q <- predicate context d
        Monad.guard (maybe True ((== Text.pack "you") . Target.wording) (Predicate.subject p) && null (Predicate.after q) && Maybe.isNothing (Predicate.mana q))
        let step = (Prose.sentence ((if Predicate.implied q then Predicate.verb q else clauseText q) <> Text.pack " unless you " <> Predicate.verb p), allMentions q <> Predicate.mentions p)
        fmap (step :) (steps (noted context [c, d]) rest)
    | Just o <- Clause.orElse c,
      Just o' <- Clause.orElse d,
      OrElse.siblings o == Set.singleton j,
      OrElse.chooser o == PlayerRef.Relative PlayerRelation.You,
      not (OrElse.villainous o),
      OrElse.selection o == CardSelection.ByChoice,
      OrElse.count o == 1,
      o' == o {OrElse.siblings = Set.singleton i},
      Clause.optionality c == Clause.optionality d,
      c == (unridden c) {Clause.optionality = Clause.optionality c, Clause.orElse = Clause.orElse c},
      d == (unridden d) {Clause.optionality = Clause.optionality d, Clause.orElse = Clause.orElse d} -> do
        p <- predicate context c
        q <- predicate context d
        let (one, object) = Text.breakOn (Text.pack " ") (Predicate.verb p)
            (other, object') = Text.breakOn (Text.pack " ") (Predicate.verb q)
        Monad.guard (Maybe.isNothing (Predicate.subject p) && Maybe.isNothing (Predicate.subject q) && object == object' && Predicate.mentions p == Predicate.mentions q && null (Predicate.after p) && null (Predicate.after q))
        let either' = one <> Text.pack " or " <> other <> object
        text <- case Clause.optionality c of
          Optionality.Mandatory -> [Prose.sentence either']
          Optionality.Optional (PlayerRef.Relative PlayerRelation.You) _ -> [Prose.sentence (Text.pack "you may " <> either')]
          _ -> []
        fmap ((text, Predicate.mentions p) :) (steps (noted context [c, d]) rest)
  (_, c) : rest -> do
    step <- clause context c
    fmap (step :) (steps (noted context [c]) rest)

-- | The context once these clauses have named their objects.
noted :: Target.Context -> [Clause.Clause card ability] -> Target.Context
noted context = noting context . concatMap (Foldable.toList . Clause.effects)

-- | A clause's one instruction, its riders aside.
predicate :: Target.Context -> Clause.Clause Card.Card Continuous.Ability -> [Predicate.Predicate]
predicate context c = do
  predicates <- instructions context (Foldable.toList (Clause.effects c))
  case List.foldr addedTogether [] predicates of
    [p] -> [p]
    _ -> []

-- | A sentence as it reads after a word that opens it ("If you do, draw a
-- card."), the card's name left capitalised.
lowered :: Target.Context -> Text.Text -> Text.Text
lowered context text
  | Target.self context /= Text.pack "it" && Text.isPrefixOf (Target.self context) text = text
  | otherwise = Prose.decapitalise text

-- | A clause's sentences and the slots they name: its instructions, "you may"
-- and the one instruction you may follow (CR 603.5), its instructions with a
-- cost paid on resolution (CR 118.12), "If [condition], [instructions].",
-- "[Instructions] if [condition]." or "[Instructions] unless [complement]."
-- (CR 608.2c). A clause gated by an "if you do" reads with its neighbour
-- ('steps'), and a "may" another player decides or one over more than one
-- instruction is not spelled out here.
clause :: Target.Context -> Clause.Clause Card.Card Continuous.Ability -> [(Text.Text, [SlotName.SlotName])]
clause context c
  | Maybe.isJust (Clause.ifTaken c) || Maybe.isJust (Clause.orElse c) = []
  | Just gate <- Clause.condition c = do
      (plain, itself) <- condition context gate
      -- Free: a plural instant-or-sorcery count reads "and/or", and an
      -- ability word may head the sentence (Calculated Dismissal's spell
      -- mastery).
      words' <- ListUtils.nubOrd [plain, Text.replace (Text.pack "instant or sorcery cards") (Text.pack "instant and/or sorcery cards") plain]
      word <- Trigger.abilityWord words'
      -- Free: a source the sentences before called "it" is named again
      -- after a condition about something else (Charging Hooligan).
      let again = ObjectRef.InSlot Target.selfSlot
      renamed <- if itself then [context {Target.self = Text.pack "it"}] else context : [context {Target.self = Target.ownName context, Target.earlier = filter (/= again) (Target.earlier context)} | Target.self context == Text.pack "it" || elem again (Target.earlier context)]
      (body, ms) <- clause renamed c {Clause.condition = Nothing}
      -- Free: "also" before instructions that add to the ones before them
      -- (Dark Dabbling).
      opened <- [lowered context body, Text.pack "also " <> lowered context body]
      -- Free: the condition trails the instructions instead (Old-Growth
      -- Educator's "put two +1/+1 counters on it if you gained life this
      -- turn"), with no ability word, which the triggered ability then heads.
      let trailed = [(Text.dropEnd 1 body <> Text.pack " if " <> words' <> Text.pack ".", ms) | Text.null word, Text.isSuffixOf (Text.pack ".") body]
      -- Free: "Then if" after the instructions before it (Tend the Sprigs).
      let thenIf = [(Text.pack "Then if " <> words' <> Text.pack ", " <> opened, ms) | Text.null word]
      -- Free: "unless" and the complement trail the instructions instead (The
      -- Spot's Portal's "You lose 2 life unless you control a Villain"), and
      -- "they control" where the instructions close on that player (Furnace
      -- Punisher).
      let unlessed =
            [ (Text.dropEnd 1 body <> Text.pack " unless " <> complement' <> Text.pack ".", ms)
            | Text.null word,
              Text.isSuffixOf (Text.pack ".") body,
              held <- foldMap pure (Trigger.unless gate),
              (complement, False) <- condition context held,
              complement' <- complement : [Text.pack "they control " <> rest | Text.isSuffixOf (Text.pack " that player.") body, rest <- Maybe.maybeToList (Text.stripPrefix (Text.pack "that player controls ") complement)]
            ]
      (word <> Text.pack "If " <> words' <> Text.pack ", " <> opened, ms) : trailed <> thenIf <> unlessed
  | otherwise =
      perColor context c <> do
        predicates <- instructions context (Foldable.toList (Clause.effects c))
        case (Clause.payGate c, Clause.optionality c, List.foldr addedTogether [] predicates) of
          (Just g, Optionality.Mandatory, together) -> paid context False g together
          -- The "may" of the instruction the payment gates, after it (Wandering
          -- Archaic's "If they don't, you may copy that spell").
          (Just g, Optionality.Optional (PlayerRef.Relative PlayerRelation.You) _, together) -> paid context True g together
          (Nothing, Optionality.Mandatory, together) -> joined context together
          -- A continuous change names its objects, never "you", so its
          -- duration-first reading is no imperative to follow "you may"
          -- (Blightcaster's "you may have target creature get -2/-2").
          (Nothing, Optionality.Optional (PlayerRef.Relative PlayerRelation.You) _, [p])
            | maybe True ((== Text.pack "you") . Target.wording) (Predicate.subject p),
              not (any (Maybe.isJust . modifyTarget) (Clause.effects c)) ->
                [(Prose.sentence (Text.pack "you may " <> Predicate.verb p) <> trailing p, Predicate.mentions p)]
          _ -> []

-- | "For each color, [instruction]": one move per color (CR 105.1), each of
-- its own slot, the slots alike but for the color their filters add (Rogues'
-- Gallery).
perColor :: Target.Context -> Clause.Clause Card.Card Continuous.Ability -> [(Text.Text, [SlotName.SlotName])]
perColor context c = do
  Nothing <- [Clause.payGate c]
  Optionality.Mandatory <- [Clause.optionality c]
  let moves = [(s, m) | Effect.MoveToZone m <- Foldable.toList (Clause.effects c), ObjectRef.InSlot s <- [MoveToZone.ref m]]
  Monad.guard (length moves == Seq.length (Clause.effects c))
  (s, m) : _ <- [moves]
  slots <- Maybe.maybeToList (traverse ((`Map.lookup` Target.slots context) . fst) moves)
  first : _ <- [slots]
  bases <- Maybe.maybeToList (traverse colorless (zip [minBound ..] slots))
  base : _ <- [bases]
  Monad.guard (length slots == length [minBound .. maxBound :: Color.Color] && all (== base) bases)
  Monad.guard (all (\slot -> slot {TargetSlot.filter = Nothing} == first {TargetSlot.filter = Nothing}) slots)
  Monad.guard (and [m' == m {MoveToZone.ref = ObjectRef.InSlot s'} | (s', m') <- moves])
  let context' = context {Target.slots = Map.insert s first {TargetSlot.filter = Just base} (Target.slots context)}
  predicates <- instructions context' [Effect.MoveToZone m]
  (text, ms) <- joined context' (List.foldr addedTogether [] predicates)
  reference <- Target.object context' Target.All (ObjectRef.InSlot s)
  -- The color qualifies the noun, ahead of the zone it is found in.
  let wording = Target.wording reference
      (noun, zone) = Text.breakOn (Text.pack " from ") wording
      text' = Text.replace wording (noun <> Text.pack " of that color" <> zone) text
  Monad.guard (text' /= text)
  [(Text.pack "For each color, " <> lowered context text', ms <> fmap fst (drop 1 moves))]
  where
    colorless (color, slot) = case TargetSlot.filter slot of
      Just (Filter.Type.And [base, Filter.Type.HasColor color']) | color' == color -> Just base
      _ -> Nothing

-- | A condition a clause's instructions wait on, and whether its words name
-- the source, which the instructions then call "it": "this spell was kicked"
-- (CR 702.33d, Jilt), "this spell's prowl cost was paid" (CR 702.76a, Morsel
-- Theft), "it was a token" (Sunpearl Kirin), or any condition an intervening
-- "if" clause reads ('Trigger.condition'). A condition on a spell is about
-- the spell only in a spell's instructions, the ones this tool lets a paragraph
-- break ('Target.breaks').
condition :: Target.Context -> Condition.Condition -> [(Text.Text, Bool)]
condition context c = case c of
  Condition.Compares (Compares.MkCompares measured Comparison.AtLeast (Quantity.Literal 1)) -> case measured of
    Quantity.TimesPaid (KeywordDesignator.Printed KeywordFamily.Kicker) | spell -> [(Text.pack "this spell was kicked", True)]
    Quantity.CastUsing (KeywordDesignator.OfFamily f) | spell -> do
      name <- Keyword.Oracle.family f
      [(Text.pack "this spell's " <> name <> Text.pack " cost was paid", False)]
    Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot s Quantity.WasToken) -> do
      it <- Target.object context Target.All (ObjectRef.InSlot s)
      Monad.guard (null (Target.mentions it) && Target.number it == Filter.Singular)
      [(Target.wording it <> Text.pack " was a token", False)]
    _ -> stated
  -- CR 705.2: an earlier instruction's flip, won or lost.
  Condition.Compares (Compares.MkCompares (Quantity.InSlot s) Comparison.Exactly (Quantity.Literal n))
    | elem s (Target.flips context), n == 1 -> [(Text.pack "you win the flip", False)]
    | elem s (Target.flips context), n == 0 -> [(Text.pack "you lose the flip", False)]
  _ -> stated
  where
    spell = elem (Text.pack "\n") (Target.breaks context)
    stated = fmap (\words' -> (words', False)) (Trigger.condition context c)

-- | Instructions with a cost paid on resolution (CR 118.12): "You may pay
-- {1}{U}. If you do, draw a card." (Merfolk Seer), "Counter target spell
-- unless its controller pays {3}." (Mana Leak, CR 118.12a), "unless any
-- player pays {2}" (Nakaya Shade). A choice of more than two costs, one offered
-- by an earlier clause or reckoned from an object's mana cost, one the payer
-- is not asked about, or one a team pays together, is not spelled out here.
paid :: Target.Context -> Bool -> PayGate.PayGate -> [Predicate.Predicate] -> [(Text.Text, [SlotName.SlotName])]
paid context mayYou g predicates
  | Maybe.isJust (PayGate.basis g) || Maybe.isJust (PayGate.offeredAt g) || PayGate.obligation g /= PayObligation.Optional || PayGate.grouping g /= PayerGrouping.Severally = []
  | otherwise = do
      (second, third) <- Cost.paymentChoice (Target.self context) (Target.keyword context) (CostChoice.unwrap (PayGate.cost g))
      (each', counted) <- case PayGate.perEach g of
        Nothing -> [(Text.empty, [])]
        Just q -> do
          a <- Target.amounts context q
          case Target.reading a of
            Target.ForEach 1 what -> [(Text.pack " for each " <> what, Target.names a)]
            _ -> []
      case PayGate.branch g of
        PayBranch.IfPaid -> do
          Monad.guard (not mayYou)
          (_, person) <- Target.player context (PayGate.payer g)
          Monad.guard (person == Target.Second)
          (body, ms) <- joined context predicates
          [(Prose.sentence (Text.pack "you may " <> second <> each') <> Text.pack " If you do, " <> lowered context body, ms <> counted)]
        branch -> do
          (who, person) <- case (branch, PayGate.payer g) of
            (PayBranch.IfNonePaid, PlayerRef.Relative PlayerRelation.AnyPlayer) -> [(Target.MkReference (Text.pack "any player") [] Filter.Singular [], Target.Third)]
            (PayBranch.IfNotPaid, payer) -> Target.player context payer
            _ -> []
          p <- case predicates of
            [p] | null (Predicate.after p), Maybe.isNothing (Predicate.mana p) -> [p]
            _ -> []
          let pays = if person == Target.Third then third else second
              instruction
                | mayYou = Text.pack "you may " <> Predicate.verb p
                | Predicate.implied p = Predicate.verb p
                | otherwise = clauseText p
              payment' = Target.wording who <> Text.pack " " <> pays <> each'
          -- Free: "unless" after the instruction (Soul Strings) or before it
          -- (Rhystic Tutor), and "unless they" where the instruction opens with
          -- the payer (Painful Quandary) or closes on it (Soul Barrier); or
          -- the payer's "may" first, the instruction naming its subject after
          -- "If they don't" (Rakdos, Patron of Chaos).
          let mentioned = allMentions p <> Target.mentions who <> counted
              unless' = do
                text <-
                  [instruction <> Text.pack " unless " <> payment', Text.pack "unless " <> payment' <> Text.pack ", " <> instruction]
                    <> [instruction <> Text.pack " unless they " <> second <> each' | Target.wording who `Text.isPrefixOf` instruction || Target.wording who `Text.isSuffixOf` instruction]
                    -- A payment closing on the source the instruction already
                    -- named calls it "it" (Magmatic Sprinter).
                    <> [ instruction <> Text.pack " unless " <> Target.wording who <> Text.pack " " <> stem <> Text.pack "it" <> each'
                       | Target.self context `Text.isInfixOf` instruction,
                         Just stem <- [Text.stripSuffix (Target.self context) pays]
                       ]
                Monad.guard (not mayYou)
                [(Prose.sentence text, mentioned)]
              afterNo = if mayYou then instruction else clauseText p
              mayNot = [(Prose.sentence (Target.wording who <> Text.pack " may " <> second <> each') <> Text.pack " If they don't, " <> afterNo <> Text.pack ".", mentioned) | branch == PayBranch.IfNotPaid, person == Target.Third, not mayYou || maybe True ((== Text.pack "you") . Target.wording) (Predicate.subject p)]
          unless' <> mayNot

-- | A clause's instructions as predicates, consecutive changes to the same
-- objects for the same duration as one: "gets +2/+2 and gains flying until
-- end of turn".
instructions :: Target.Context -> [Effect.Effect Card.Card Continuous.Ability] -> [[Predicate.Predicate]]
instructions context es = case es of
  [] -> [[]]
  e@(Effect.ModifyTarget m) : rest
    | Maybe.isNothing (ModifyTarget.each m) -> do
        let alike n = Maybe.isNothing (ModifyTarget.each n) && ModifyTarget.ref n == ModifyTarget.ref m && ModifyTarget.duration n == ModifyTarget.duration m
            (run, others) = span (maybe False alike . modifyTarget) rest
        p <- modified context m (Maybe.mapMaybe (fmap ModifyTarget.modification . modifyTarget) run)
        fmap (p :) (instructions (noting context (e : run)) others)
  -- Placements on one object, as Oracle text puts them in one: "Put a +1/+1
  -- counter and a trample counter on target creature" (Champion of Dusan),
  -- "Put a flying counter, a deathtouch counter, and a lifelink counter on
  -- target creature" (Qarsi Revenant). Free: the first apart, below.
  e@(Effect.PutCounters p) : rest
    | Maybe.isNothing (PutCounters.totalAtMost p),
      Maybe.isNothing (PutCounters.putter p),
      null (PutCounters.alternatives p),
      (run, others) <- span (placedOn p) rest,
      not (null run) ->
        let together = do
              ref <- Target.object context Target.Each (PutCounters.ref p)
              each' <- traverse (\q -> Object.counters (Target.keyword context) (PutCounters.kind q) (PutCounters.quantity q)) (p : Maybe.mapMaybe putCounters run)
              let put = Text.pack "put " <> Prose.listing "and" each' <> Text.pack " on " <> Target.wording ref
              pure (Predicate.MkPredicate Nothing put [] (Target.mentions ref) Nothing False)
            apart = do
              pe <- effect context e
              fmap (pe :) (instructions (noting context [e]) rest)
         in ( do
                pt <- together
                fmap (pt :) (instructions (noting context (e : run)) others)
            )
              <> apart
  -- Placements on two or more objects, the verb shared: "put a +1/+1 counter
  -- on each creature you control and a loyalty counter on each planeswalker
  -- you control" (Brokers Ascendancy), "put a -1/-1 counter on target
  -- creature, two -1/-1 counters on another target creature, and three -1/-1
  -- counters on a third target creature" (Raven's Run). Free: the first two
  -- or more together, the most first, or the first apart, below.
  e@(Effect.PutCounters p) : rest
    | unlimited p,
      run@(_ : _) <- spread p rest ->
        let together k = do
              let placing = e : fmap Effect.PutCounters (take k run)
              (items, mentioned) <- placements context (p : take k run)
              let put = Text.pack "put " <> Prose.listing "and" items
              pt <- [Predicate.MkPredicate Nothing put [] mentioned Nothing False]
              fmap (pt :) (instructions (noting context placing) (drop (length placing) (e : rest)))
            apart = do
              pe <- effect context e
              fmap (pe :) (instructions (noting context [e]) rest)
         in concatMap together [length run, length run - 1 .. 1] <> apart
  e : rest
    | Just refs <- returnOf context e -> do
        p <- Object.returned context refs
        fmap (p :) (instructions (noting context [e]) rest)
  e : rest -> do
    p <- effect context e
    fmap (p :) (instructions (noting context [e]) rest)

-- | The context once these instructions have named their objects.
noting :: Target.Context -> [Effect.Effect card ability] -> Target.Context
noting context es = context {Target.earlier = Target.earlier context <> concatMap objectsOf es, Target.pumped = Target.pumped context <> concatMap pumpedOf es, Target.flips = Target.flips context <> concatMap flipsOf es}

-- | The slot an instruction binds one coin flip of yours at, won or lost (CR
-- 705.2), if it is one: "Flip a coin. If you win the flip, ..."
flipsOf :: Effect.Effect card ability -> [SlotName.SlotName]
flipsOf e = case e of
  Effect.FlipCoin f
    | f == FlipCoin.MkFlipCoin {FlipCoin.count = FlipCoin.defaultCount, FlipCoin.reading = CoinReading.Wins, FlipCoin.slot = FlipCoin.slot f, FlipCoin.misses = Nothing, FlipCoin.flipper = FlipCoin.defaultFlipper} -> [FlipCoin.slot f]
  _ -> []

-- | The objects an instruction changes the power and toughness of: "Untap
-- target creature. It gets +2/+4" (Inspirit) names it again without one.
pumpedOf :: Effect.Effect card ability -> [ObjectRef.ObjectRef]
pumpedOf e = case e of
  Effect.ModifyTarget m | Modification.ModifyPowerToughness _ <- ModifyTarget.modification m -> [ModifyTarget.ref m]
  _ -> []

-- | The objects an instruction names, which a later one calls "it".
objectsOf :: Effect.Effect card ability -> [ObjectRef.ObjectRef]
objectsOf e = case e of
  Effect.ModifyTarget m -> [ModifyTarget.ref m]
  Effect.Destroy d -> [Destroy.ref d]
  Effect.Counter c -> [Counter.ref c]
  Effect.MoveToZone m -> [MoveToZone.ref m]
  Effect.PutCounters p -> [PutCounters.ref p]
  Effect.Tap t -> [Tap.ref t]
  Effect.Untap r -> [r]
  Effect.Sacrifice s -> [SacrificeEffect.ref s]
  Effect.DealDamage d -> maybe (fmap ObjectRef.InSlot (Maybe.maybeToList (DealDamage.dealer d))) pure (DealDamage.dealers d) <> fmap DamagePart.ref (Foldable.toList (DealDamage.parts d))
  Effect.PreventNextDamage p -> Maybe.maybeToList (PreventNextDamage.ref p)
  _ -> []

-- | The objects, if the instruction returns several targeted objects to their
-- owners' hands from the battlefield, as one action (CR 608.2f), and nothing
-- more.
returnOf :: Target.Context -> Effect.Effect card Continuous.Ability -> Maybe [ObjectRef.ObjectRef]
returnOf context e = case e of
  Effect.MoveToZone m
    | MoveToZone.zone m == Zone.Hand,
      MoveToZone.riders m == EntryRiders.defaultValue,
      Maybe.isNothing (MoveToZone.duration m),
      Maybe.isNothing (MoveToZone.origin m),
      ObjectRef.AllOf refs <- MoveToZone.ref m,
      all targeted refs ->
        Just (NonEmpty.toList refs)
  _ -> Nothing
  where
    targeted ref = case ref of
      ObjectRef.InSlot s -> case Map.lookup s (Target.slots context) of
        Just slot -> TargetSlot.pool slot `elem` [Pool.Creatures, Pool.Permanents]
        Nothing -> False
      _ -> False

-- | The instruction's continuous effect, if it is one.
-- | Whether an instruction is an unlimited placement of counters on the
-- object this placement puts them on.
placedOn :: PutCounters.PutCounters -> Effect.Effect card ability -> Bool
placedOn p e = case e of
  Effect.PutCounters q -> PutCounters.ref q == PutCounters.ref p && Maybe.isNothing (PutCounters.totalAtMost q) && Maybe.isNothing (PutCounters.putter q) && null (PutCounters.alternatives q)
  _ -> False

-- | Whether a placement is unlimited, of one kind, by the instruction's
-- controller.
unlimited :: PutCounters.PutCounters -> Bool
unlimited p = Maybe.isNothing (PutCounters.totalAtMost p) && Maybe.isNothing (PutCounters.putter p) && null (PutCounters.alternatives p)

-- | The plain placements following one, each on another object than the
-- one before it.
spread :: PutCounters.PutCounters -> [Effect.Effect card ability] -> [PutCounters.PutCounters]
spread p es = case es of
  f@(Effect.PutCounters q) : rest | unlimited q, not (placedOn p f) -> q : spread q rest
  _ -> []

-- | Each placement as "[counters] on [object]", each object named in the
-- context of those before it, and every object mentioned.
placements :: Target.Context -> [PutCounters.PutCounters] -> [([Text.Text], [SlotName.SlotName])]
placements context ps = case ps of
  [] -> [([], [])]
  p : rest -> do
    ref <- Target.object context Target.Each (PutCounters.ref p)
    these <- Object.counters (Target.keyword context) (PutCounters.kind p) (PutCounters.quantity p)
    (items, mentioned) <- placements (noting context [Effect.PutCounters p]) rest
    pure (these <> Text.pack " on " <> Target.wording ref : items, Target.mentions ref <> mentioned)

putCounters :: Effect.Effect card ability -> Maybe PutCounters.PutCounters
putCounters e = case e of
  Effect.PutCounters q -> Just q
  _ -> Nothing

modifyTarget :: Effect.Effect card ability -> Maybe (ModifyTarget.ModifyTarget ability)
modifyTarget e = case e of
  Effect.ModifyTarget m -> Just m
  _ -> Nothing

-- | "[Objects] get(s) [change] [duration]": the change an instruction makes to
-- objects for a while (CR 611.2a), and the ones following it to the same
-- objects for the same while.
modified :: Target.Context -> ModifyTarget.ModifyTarget Continuous.Ability -> [Modification.Modification Continuous.Ability] -> [Predicate.Predicate]
modified context m more = do
  -- Free for a description: the objects together (Trumpet Blast's
  -- "attacking creatures get") or each of them (Zhang He, Wei General's
  -- "each other creature you control gets").
  who <- case ModifyTarget.ref m of
    ref@(ObjectRef.EachMatching _) -> Target.object context Target.Affected ref <> fmap (\r -> r {Target.number = Filter.Singular}) (Target.object context Target.Each ref)
    ref -> Target.object context Target.Affected ref
  let distributed = case ModifyTarget.ref m of
        ObjectRef.InSlot _ -> Target.number who == Filter.Plural
        _ -> False
  (change', defined) <- Continuous.modifications Continuous.Created (Target.number who) distributed context (ModifyTarget.modification m : more)
  -- Objects a change to power and toughness reaches again get "an
  -- additional" one: "That creature gets an additional +4/+4" (Wild Might).
  -- Free for some of the objects an earlier change reached: "Creatures that
  -- are green and/or white get an additional -2/-2" (Glistening Deluge), or
  -- "Citizens you control get an additional +1/+1" after "Creatures you
  -- control get" (Take to the Streets), a pump reaching only creatures (CR
  -- 208.3).
  let pumping = case ModifyTarget.modification m of
        Modification.ModifyPowerToughness _ -> True
        _ -> False
      again = pumping && elem (ModifyTarget.ref m) (Target.pumped context)
      narrower = case ModifyTarget.ref m of
        ObjectRef.EachMatching f ->
          let within r = case r of
                ObjectRef.EachMatching g -> g /= f && all (`elem` (Filter.Type.HasCardType CardType.Creature : conjuncts f)) (conjuncts g)
                _ -> False
           in pumping && any within (Target.pumped context)
        _ -> False
      additional = (\(v, rest) -> v <> Text.pack " an additional" <> rest) (Text.breakOn (Text.pack " ") change')
      -- Some of the objects an earlier instruction changed "also" change:
      -- "White creatures you control also gain first strike" (Sanctified
      -- Charge), or all of them: "those creatures also gain vigilance"
      -- (Kytheon's Tactics), or the target it named: "that creature also
      -- gains trample" (Efflorescence). Free.
      conjuncts f = case f of
        Filter.Type.And fs -> fs
        _ -> [f]
      among = case ModifyTarget.ref m of
        ObjectRef.EachMatching f ->
          let within r = case r of
                ObjectRef.EachMatching g -> all (`elem` conjuncts f) (conjuncts g)
                _ -> False
           in any within (Target.earlier context)
        ref@(ObjectRef.InSlot _) -> elem ref (Target.earlier context)
        _ -> False
  changed <- if again then [additional] else change' : [additional | narrower]
  change <- if among then [changed, Text.pack "also " <> changed] else [changed]
  lasting <- Continuous.duration (ModifyTarget.duration m)
  case lasting of
    Nothing -> [Predicate.MkPredicate (Just who) (change <> defined) [] [] Nothing False]
    -- Free: the duration ends the sentence (Giant Growth) or opens it
    -- (Aspirant's Ascent).
    Just period ->
      [ Predicate.MkPredicate (Just who) (change <> Text.pack " " <> period <> defined) [] [] Nothing False,
        Predicate.MkPredicate Nothing (period <> Text.pack ", " <> Target.wording who <> Text.pack " " <> change <> defined) [] (Target.mentions who) Nothing False
      ]

-- | One sentence and the next, as the context's breaks allow, or the next
-- opened by "Then", which leaves the card's name capitalised. Free: "Then"
-- (Return of the Nightstalkers) or not (Soul Shred).
followedBy :: Target.Context -> Text.Text -> Text.Text -> [Text.Text]
followedBy context text next =
  let named = Text.isPrefixOf (Target.self context) next
   in [text <> separator <> next | separator <- Target.breaks context] <> [text <> Text.pack " Then " <> (if named then next else Prose.decapitalise next)]

-- | Mana added by consecutive instructions, as Oracle text adds it in one:
-- "Add {C}{C}." (Sol Ring).
addedTogether :: Predicate.Predicate -> [Predicate.Predicate] -> [Predicate.Predicate]
addedTogether p rest = case (Predicate.mana p, rest) of
  (Just (Predicate.Symbols a), q : others)
    | Just (Predicate.Symbols b) <- Predicate.mana q -> p {Predicate.verb = Text.pack "add " <> a <> b, Predicate.mana = Just (Predicate.Symbols (a <> b))} : others
  _ -> p : rest

-- | The instructions of one clause as Oracle text strings them: one sentence
-- with "and" or two sentences, broken as the context allows. An instruction
-- that adds mana, or that must be followed by a sentence of its own, ends its
-- sentence (Ancient Tomb). An imperative of "you" beside another instruction
-- keeps its "you" when joined to it by "and" (Bad Deal). When two share a
-- subject the second drops it ("draws two cards and loses 2 life"). Three or
-- more read one or two at a time: "Draw two cards, then discard two cards.
-- Untap up to three lands." (Frantic Search), or, sharing one subject, as one
-- series.
joined :: Target.Context -> [Predicate.Predicate] -> [(Text.Text, [SlotName.SlotName])]
joined context predicates = case predicates of
  -- Free for an amount of damage or life: "equal to" or an X defined after
  -- (Sanctum of Shattered Heights's "deals X damage to target creature or
  -- planeswalker, where X is the number of Shrines you control", Blight Pile's
  -- "loses X life, where X is").
  [p] ->
    (sentences p, allMentions p)
      : [ (Prose.sentence (clauseText p {Predicate.verb = vx} <> Text.pack ", where X is " <> e) <> trailing p, allMentions p)
        | not (Predicate.implied p),
          (vx, e) <- equalToX (Predicate.verb p),
          Text.pack " X damage to " `Text.isInfixOf` vx || Text.pack " X life" `Text.isSuffixOf` vx
        ]
  -- Free: one subject's series, whose last step alone keeps "then"
  -- (Faithful Mending's "You gain 2 life, draw two cards, then discard two
  -- cards.") or "and" (Demogorgon's Clutches's "discards two cards, mills two
  -- cards, and loses 2 life").
  p : rest@(_ : _ : _) -> ListUtils.nubOrd (apart <> series)
    where
      apart = do
        (first, rest') <- [splitAt 1 predicates, splitAt 2 predicates]
        (text, ms) <- joined context first
        (next, ns) <- joined context rest'
        sentence' <- followedBy context text next <> thenJoined text next
        [(sentence', ms <> ns)]
      -- Free: one sentence, its second part opened by ", then" (Krydle of
      -- Baldur's Gate's "that player loses 1 life and mills a card, then you
      -- gain 1 life and scry 1").
      thenJoined text next =
        [ body <> Text.pack ", then " <> (if Text.isPrefixOf (Target.self context) next then next else Prose.decapitalise next)
        | Just body <- [Text.stripSuffix (Text.pack ".") text],
          not (Text.isInfixOf (Text.pack ". ") body)
        ]
      series =
        [ (Prose.sentence (Text.intercalate (Text.pack ", ") (opening : middle') <> Text.pack last' <> q') <> trailing q, allMentions p <> foldMap Predicate.mentions rest)
        | all (sameSubject p) rest,
          -- Free: an imperative series drops its "you" (Mascot Exhibition).
          opening <- clauseText p : [Predicate.verb p | all Predicate.implied predicates],
          Just (middle, q) <- [List.unsnoc rest],
          all (null . Predicate.after) (p : middle),
          all (Maybe.isNothing . Predicate.mana) predicates,
          (middle', q') <- (fmap Predicate.verb middle, Predicate.verb q) : sharedVerbElided (Predicate.verb p) (fmap Predicate.verb middle) (Predicate.verb q),
          last' <- [", then ", ", and "]
        ]
      -- Free: a verb every step shares, said once (Mascot Exhibition's "Create
      -- a 2/1 ... token, a 3/2 ... token, and a 4/4 ... token.").
      sharedVerbElided vp vms vq =
        [ (fmap (Text.drop (Text.length word)) vms, Text.drop (Text.length word) vq)
        | let word = fst (Text.breakOn (Text.pack " ") vp) <> Text.pack " ",
          all (Text.isPrefixOf word) (vq : vms)
        ]
  [p, q]
    | not (null (Predicate.after p)) || Maybe.isJust (Predicate.mana p) || Maybe.isJust (Predicate.mana q) -> apart
    -- Free for "you": repeated (Ancient Craving) or not (Ambition's Cost).
    -- Free: "and" (Ancient Craving) or ", then" (Owl Familiar).
    | sameSubject p q ->
        [(Prose.sentence (clauseText p <> Text.pack " and " <> clauseText q) <> trailing q, allMentions p <> allMentions q) | fmap Target.wording (Predicate.subject q) == Just (Text.pack "you")]
          <> [(Prose.sentence (clauseText p <> Text.pack " and " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q)]
          <> [(Prose.sentence ((if Predicate.implied p then Predicate.verb p else clauseText p) <> Text.pack ", then " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q)]
          -- Free: two imperatives joined by "and" (Make It So's "create a 1/1
          -- red Officer creature token and draw two cards").
          <> [(Prose.sentence (Predicate.verb p <> Text.pack " and " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q) | Predicate.implied p, Predicate.implied q]
          -- Free: ", then you" (Wall Crawl).
          <> [(Prose.sentence ((if Predicate.implied p then Predicate.verb p else clauseText p) <> Text.pack ", then " <> clauseText q) <> trailing q, allMentions p <> allMentions q) | fmap Target.wording (Predicate.subject q) == Just (Text.pack "you")]
          -- Free for one count read twice: "equal to" each time, above, or one
          -- X defined once (Graveborn Muse's "you draw X cards and lose X life,
          -- where X is the number of Zombies you control").
          <> [ (Prose.sentence (clauseText p {Predicate.verb = vp} <> Text.pack " and " <> vq <> Text.pack ", where X is " <> e) <> trailing q, allMentions p <> Predicate.mentions q)
             | (vp, vq, e) <- sharedX
             ]
          -- Free: two sentences (Vraska's Final Mercy's "You lose 2 life.
          -- Empower Jace 6.").
          <> apart
          -- Free: an imperative joined to "you" restated (Shinra
          -- Reinforcements's "mill three cards and you gain 3 life").
          <> imperativeAnd
    -- Free: two sentences (Afterlife Insurance), "and" (Bad Deal) or, after
    -- an imperative, ", then" (Macabre Waltz) or "and" (Sorcerer's Strongbox).
    | Predicate.implied p || Predicate.implied q -> apart <> apartRestated <> together <> togetherX <> thenImperative <> andImperative <> imperativeAnd <> imperativeThen <> eachDoes
    -- Free: "and" (Vampiric Feast), two sentences (Soul Shred) or, after an
    -- instruction naming no subject, ", then" (Rust Harvester).
    | otherwise -> together <> apart <> togetherX <> durationFirst <> durationFirstX <> subjectlessThen <> sourceAgain
    where
      -- Free: the source, named, then called "it" as the next instruction's
      -- subject, which drops it (Penregon Strongbull's "This creature gets
      -- +1/+1 until end of turn and deals 1 damage to each opponent").
      sourceAgain =
        [ (Prose.sentence (clauseText p <> Text.pack " and " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q)
        | fmap Target.wording (Predicate.subject p) == Just (Target.self context),
          fmap (\r -> (Target.wording r, Target.antecedents r)) (Predicate.subject q) == Just (Text.pack "it", [])
        ]
      -- Free for one duration read twice: after each verb, above, or once ahead
      -- of both (Rookie Mistake).
      durationFirst =
        [ (Prose.sentence (Text.pack "until end of turn, " <> clauseText p {Predicate.verb = vp} <> Text.pack " and " <> clauseText q {Predicate.verb = vq}) <> trailing q, allMentions p <> allMentions q)
        | Just vp <- [Text.stripSuffix (Text.pack " until end of turn") (Predicate.verb p)],
          Just vq <- [Text.stripSuffix (Text.pack " until end of turn") (Predicate.verb q)]
        ]
      -- Free: that duration ahead of both and one X defined once after them
      -- (Monoist Circuit-Feeder).
      durationFirstX =
        [ (Prose.sentence (Text.pack "until end of turn, " <> clauseText p {Predicate.verb = vp} <> Text.pack " and " <> clauseText q {Predicate.verb = vq} <> whereX <> e) <> trailing q, allMentions p <> allMentions q)
        | let whereX = Text.pack ", where X is "
              defined v = [(before, Text.drop (Text.length whereX) rest) | let (before, rest) = Text.breakOn whereX v, not (Text.null rest)],
          (wp, e) <- defined (Predicate.verb p),
          (wq, e') <- defined (Predicate.verb q),
          e == e',
          Just vp <- [Text.stripSuffix (Text.pack " until end of turn") wp],
          Just vq <- [Text.stripSuffix (Text.pack " until end of turn") wq]
        ]
      together = [(Prose.sentence (clauseText p <> Text.pack " and " <> clauseText q) <> trailing q, allMentions p <> allMentions q)]
      subjectlessThen = [(Prose.sentence (clauseText p <> Text.pack ", then " <> clauseText q) <> trailing q, allMentions p <> allMentions q) | Maybe.isNothing (Predicate.subject p)]
      apart = [(text, allMentions p <> allMentions q) | text <- followedBy context (sentences p) (sentences q)]
      -- Free: after another player's sentence, an imperative of "you" keeps
      -- its "you" (Test of Conviction's "Target opponent draws a card. You
      -- draw two cards.").
      apartRestated = [(text, allMentions p <> allMentions q) | Maybe.isJust (Predicate.subject p), not (Predicate.implied p), Predicate.implied q, text <- followedBy context (sentences p) (Prose.sentence (clauseText q) <> trailing q)]
      -- Free: a series, whose last step alone keeps "then" (Curse-Marred
      -- Demon's "put it into your hand, shuffle, then discard a card at random").
      thenImperative = [(Prose.sentence (first <> Text.pack ", then " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q) | Maybe.isNothing (Predicate.subject p), Predicate.implied q, first <- clauseText p : serial (clauseText p)]
      serial t = [Text.dropEnd 5 before <> rest | let (before, rest) = Text.breakOnEnd (Text.pack ", then ") t, not (Text.null before)]
      andImperative = [(Prose.sentence (clauseText p <> Text.pack " and " <> Predicate.verb q) <> trailing q, allMentions p <> Predicate.mentions q) | Maybe.isNothing (Predicate.subject p), Predicate.implied q]
      -- Free: an imperative joined to a stated subject (Vampire Gourmand).
      imperativeAnd = [(Prose.sentence (Predicate.verb p <> Text.pack " and " <> clauseText q) <> trailing q, allMentions p <> allMentions q) | Predicate.implied p, not (Predicate.implied q)]
      -- Free: "you" and another player given one instruction, "each" (Commander
      -- William Riker's "You and target opponent each draw two cards").
      eachDoes =
        [ (Prose.sentence (Text.pack "you and " <> Target.wording s <> Text.pack " each " <> Predicate.verb p) <> trailing q, allMentions p <> allMentions q)
        | Predicate.implied p,
          null (Predicate.after p),
          let (word, rest) = Text.breakOn (Text.pack " ") (Predicate.verb p),
          Predicate.verb q == word <> Text.pack "s" <> rest,
          Just s <- [Predicate.subject q]
        ]
      -- Free: an imperative followed by a stated subject after ", then" (Hire a
      -- Crew).
      imperativeThen = [(Prose.sentence (Predicate.verb p <> Text.pack ", then " <> clauseText q) <> trailing q, allMentions p <> allMentions q) | Predicate.implied p, not (Predicate.implied q)]
      -- Free for one count read by two subjects: "equal to" each time, above,
      -- or one X defined once (Tendrils of Corruption's "deals X damage to
      -- target creature and you gain X life, where X is the number of Swamps
      -- you control").
      togetherX =
        [ (Prose.sentence (clauseText p {Predicate.verb = vp} <> Text.pack " and " <> clauseText q {Predicate.verb = vq} <> Text.pack ", where X is " <> e) <> trailing q, allMentions p <> allMentions q)
        | (vp, vq, e) <- sharedX
        ]
      sharedX = ListUtils.nubOrd [(vp, vq, e) | (vp, e) <- equalToX (Predicate.verb p), (vq, e') <- equalToX (Predicate.verb q), e == e']
  _ -> []

-- | A verb ending "NOUN equal to E" as "X NOUN" and E: "draw cards equal to
-- the number of ..." is "draw X cards" where X is that number (CR 107.3). A
-- verb "deals damage equal to E to R" is "deals X damage to R", for each way
-- of reading the words after "equal to" as E and R.
equalToX :: Text.Text -> [(Text.Text, Text.Text)]
equalToX v =
  let (before, rest) = Text.breakOn (Text.pack " equal to ") v
   in case (Text.stripPrefix (Text.pack " equal to ") rest, reverse (Text.words before)) of
        (Just e, noun : verbs@(_ : _))
          | not (Text.null e) ->
              (Text.unwords (reverse verbs <> [Text.pack "X", noun]), e)
                : [ (Text.unwords (reverse verbs <> [Text.pack "X", noun]) <> Text.pack " to " <> recipients, amount)
                  | noun == Text.pack "damage",
                    (amount, recipients) <- splits e,
                    not (Text.null amount),
                    not (Text.null recipients)
                  ]
        _ -> []
  where
    splits e = [(Text.intercalate (Text.pack " to ") a, Text.intercalate (Text.pack " to ") b) | let parts = Text.splitOn (Text.pack " to ") e, n <- [1 .. length parts - 1], let (a, b) = splitAt n parts]

-- | Whether two predicates have one subject, so the second can drop it: the
-- same words, or a second naming the first's target again ("Target creature
-- gains vigilance until end of turn and can't be blocked this turn", Pym
-- Particles).
sameSubject :: Predicate.Predicate -> Predicate.Predicate -> Bool
sameSubject p q = case (Predicate.subject p, Predicate.subject q) of
  (Just a, Just b) -> a == b || (not (null (Target.antecedents b)) && Target.antecedents b == Target.mentions a)
  _ -> False

-- | The sentences that must follow a predicate, each after a space.
trailing :: Predicate.Predicate -> Text.Text
trailing p = Text.concat (fmap (Text.pack " " <>) (Predicate.after p))

-- | The predicate as a sentence of its own, followed by the ones that must
-- follow it.
sentences :: Predicate.Predicate -> Text.Text
sentences p = Prose.sentence (if Predicate.implied p then Predicate.verb p else clauseText p) <> trailing p

-- | The predicate as an independent clause, without a full stop.
clauseText :: Predicate.Predicate -> Text.Text
clauseText p = case Predicate.subject p of
  Nothing -> Predicate.verb p
  Just s -> Target.wording s <> Text.pack " " <> Predicate.verb p

-- | Every slot the predicate names, its subject's included.
allMentions :: Predicate.Predicate -> [SlotName.SlotName]
allMentions p = foldMap Target.mentions (Predicate.subject p) <> Predicate.mentions p

-- | One instruction's predicate; none for an instruction not spelled out here
-- yet. The arms are grouped by the module wording them, which is the lane
-- of the renderer's totality work (#9868) that grows them.
effect :: Target.Context -> Effect.Effect Card.Card Continuous.Ability -> [Predicate.Predicate]
effect context e = case e of
  -- Objects, zones and libraries: Parun.Oracle.Effect.Object.
  Effect.ModifyTarget {} -> []
  Effect.ChangeText {} -> []
  Effect.Search s -> Object.searched context s
  Effect.ExileAllGraveyards {} -> []
  Effect.Destroy d -> Object.destroyed context d
  Effect.Sacrifice s -> Object.sacrificed context s
  Effect.Attach {} -> []
  Effect.AttachAsThoughCreature {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.AttachAll {} -> []
  Effect.Unattach {} -> []
  Effect.MoveToZone m -> Object.moved context m
  Effect.Mill m -> Object.milled context m
  Effect.Reveal {} -> []
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell {} -> []
  Effect.LookAt {} -> []
  Effect.ArrangeInLibrary {} -> []
  Effect.Scry s -> Object.scried context s
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Clash {} -> []
  Effect.AsYouCascade {} -> []
  Effect.Explore {} -> []
  Effect.Connive {} -> []
  Effect.ExchangeZones {} -> []
  Effect.ExchangeCardZones {} -> []
  Effect.SetOwner {} -> []
  Effect.ExchangeOwnership {} -> []
  Effect.Create c -> Object.created context c
  Effect.Conjure {} -> []
  Effect.CreateCopy {} -> []
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject c -> Object.copied context c
  Effect.SetX {} -> []
  Effect.Replace r -> Object.regenerated context r
  Effect.Counter c -> Object.countered context c
  Effect.PutCounters p -> Object.placed context p
  Effect.Divide {} -> []
  Effect.RemoveCounters {} -> []
  Effect.RemoveCountersAmong {} -> []
  Effect.MoveCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.PutCounterOfEachKind {} -> []
  Effect.Tap t -> Object.tapped context t
  Effect.Untap r -> Object.untapped context r
  Effect.Detain {} -> []
  Effect.Goad {} -> []
  Effect.Pair {} -> []
  Effect.DoesNotUntapNext {} -> []
  Effect.Transform {} -> []
  Effect.Convert {} -> []
  Effect.Flip {} -> []
  Effect.Meld {} -> []
  Effect.PhaseOut {} -> []
  Effect.TurnFaceDown {} -> []
  Effect.TurnFaceUp {} -> []
  Effect.RemoveFromCombat {} -> []
  Effect.BecomesBlocked {} -> []
  Effect.BlockAttacker {} -> []
  Effect.JoinAttack {} -> []
  Effect.ReselectAttack {} -> []
  Effect.SwitchBlockers {} -> []
  Effect.ExchangeBlocks {} -> []
  Effect.GainControl {} -> []
  Effect.ExchangeControl {} -> []
  Effect.RequireBlock {} -> []
  Effect.RequireAttack {} -> []
  Effect.Prohibit {} -> []
  Effect.ForbidAttack {} -> []
  Effect.ForbidBeingBlocked f -> Object.unblockable context f
  Effect.BecomeProtector {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Undesignate {} -> []
  Effect.SetHalfLocked {} -> []
  Effect.CounterAndMark {} -> []
  Effect.Mentor {} -> []
  Effect.Exploit {} -> []
  Effect.ExileHaunting {} -> []
  Effect.ChoosePermanents {} -> []
  Effect.ExileHandThenDraw {} -> []
  Effect.Proliferate {} -> []
  Effect.Bolster {} -> []
  Effect.Amass a -> Object.amassed context a
  Effect.Blight {} -> []
  Effect.Earthbend {} -> []
  Effect.Airbend {} -> []
  Effect.Assimilate {} -> []
  Effect.Forage {} -> []
  Effect.Populate {} -> []
  Effect.Learn {} -> []
  Effect.TimeTravel {} -> []
  Effect.Recruit {} -> []
  Effect.Discover d -> Object.discovered context d
  Effect.Cloak {} -> []
  Effect.ManifestDread {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Ante {} -> []
  Effect.PutSticker {} -> []
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.OfferNamedCopy {} -> []
  Effect.OfferNotedCopy {} -> []
  Effect.GrantPlayFromExile {} -> []
  Effect.GrantCopyCast {} -> []
  Effect.GrantLookAtExiled {} -> []
  Effect.MakePlotted {} -> []
  Effect.MakeForetold {} -> []
  Effect.MakeWarped {} -> []
  Effect.ForEach f -> Object.regeneratedEach context f
  Effect.Heal {} -> []
  Effect.ChooseNewTargets {} -> []
  Effect.ChangeTargets {} -> []
  -- Players, damage and life: Parun.Oracle.Effect.Player.
  Effect.DealDamage d -> Player.dealDamage context d
  Effect.Fight {} -> []
  Effect.AddMana a -> Player.addedMana context a
  Effect.ActivateManaAbilities {} -> []
  Effect.MoveMana {} -> []
  Effect.RestartGame {} -> []
  Effect.ControlPlayerNext {} -> []
  Effect.ControlPlayerThisResolution {} -> []
  Effect.Draw d -> Player.drew context d
  Effect.Discard d -> Player.discarded context d
  Effect.LoseLife l -> Player.lost context l
  Effect.GainLife g -> Player.gained context g
  Effect.ExchangeLifeTotals {} -> []
  Effect.ExchangeValues {} -> []
  Effect.SetLifeTotal {} -> []
  Effect.LoseGame who -> Player.lostGame context who
  Effect.WinGame {} -> []
  Effect.DrawGame {} -> []
  Effect.RedistributeLifeTotals {} -> []
  Effect.IncreaseSpeed {} -> []
  Effect.DecreaseSpeed {} -> []
  Effect.SkipPhase {} -> []
  Effect.PreventNextDamage p -> Player.prevented context p
  Effect.LaterPayment {} -> []
  Effect.PreventAllDamage {} -> []
  Effect.NextDamageInstance {} -> []
  Effect.RedirectDamage {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAny {} -> []
  Effect.ChooseNumber {} -> []
  Effect.BindAmount {} -> []
  Effect.BindPlayers {} -> []
  Effect.BindLeaders {} -> []
  Effect.AddPhases {} -> []
  Effect.AddStep {} -> []
  Effect.EndTurn {} -> []
  Effect.EndCombatPhase {} -> []
  Effect.AffectPlayers {} -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Firebend {} -> []
  Effect.GiveGift {} -> []
  Effect.ItBecomes {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChoosePlayer {} -> []
  Effect.ChoosePlayerAtRandom {} -> []
  Effect.ChooseGroup {} -> []
  Effect.RollDie {} -> []
  Effect.Rethrow {} -> []
  Effect.RerollStoredResults {} -> []
  Effect.FlipCoin f -> Player.flipped context f
  Effect.Guess {} -> []
  Effect.Note {} -> []
  Effect.ChooseCardName {} -> []
  Effect.ChooseQuality {} -> []
  Effect.TemptWithTheRing {} -> []
  Effect.OpenAttraction {} -> []
  Effect.RollToVisit {} -> []
  Effect.Assemble {} -> []
  Effect.Reassemble {} -> []
  Effect.Crank {} -> []
  Effect.Planeswalk {} -> []
  Effect.ChaosEnsues {} -> []
  Effect.Abandon -> Player.abandoned context
  Effect.SetInMotion {} -> []
  Effect.ClaimPrize {} -> []
  Effect.Venture {} -> []
  Effect.PlayerSacrifices s -> Player.sacrificedBy context s
  Effect.Vote {} -> []
  Effect.Bid {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ReverseTurnOrder {} -> []
  -- Instructions that run others, which an arm module cannot word without
  -- this function.
  Effect.ArmDelayedTrigger {} -> []
  Effect.ForEachNumber {} -> []
  Effect.Repeat {} -> []
  Effect.RepeatIf {} -> []
