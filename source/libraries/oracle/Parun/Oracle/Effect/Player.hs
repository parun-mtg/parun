-- Instructions to players, and of damage and life (Parun.Types.Effect), as the
-- predicates Oracle text words them with (CR 608.2c).
module Parun.Oracle.Effect.Player where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Oracle.Cost as Cost
import qualified Parun.Oracle.Filter as Filter
import qualified Parun.Oracle.Predicate as Predicate
import qualified Parun.Oracle.Prose as Prose
import qualified Parun.Oracle.Target as Target
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamagePart as DamagePart
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.Discard as Discard
import qualified Parun.Types.Draw as Draw
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.FlipCoin as FlipCoin
import qualified Parun.Types.HiddenZone as HiddenZone
import qualified Parun.Types.LifeLoss as LifeLoss
import qualified Parun.Types.LifeLossCause as LifeLossCause
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaRetention as ManaRetention
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerSacrifices as PlayerSacrifices
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.PreventNextDamage as PreventNextDamage
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TheseDiscard as TheseDiscard

-- | "[Player] draws [cards]" (CR 121.1). Any other draw is not spelled out
-- here.
drew :: Target.Context -> Draw.Draw -> [Predicate.Predicate]
drew context d
  | Maybe.isNothing (Draw.slot d) = do
      a <- Target.amounts context (Draw.quantity d)
      count <- case Target.reading a of
        Target.Stated q -> Target.cards q
        -- Free for a count: "a card for each" (Theft of Dreams) or "cards
        -- equal to the number of" (Dream Salvage).
        Target.ForEach n each' -> fmap (<> Text.pack " for each " <> each') (Target.cards (Quantity.Literal n))
        Target.EqualTo equal -> [Text.pack "cards equal to " <> equal]
      Predicate.naming (Target.names a) (Predicate.imperative context (Draw.player d) (Text.pack "draw " <> count) (Text.pack "draws " <> count))
  | otherwise = []

-- | "[Player] gains [life]" (CR 119.3).
gained :: Target.Context -> PlayerQuantity.PlayerQuantity -> [Predicate.Predicate]
gained context g = do
  a <- Target.amounts context (PlayerQuantity.quantity g)
  n <- life a
  Predicate.naming (Target.names a) (Predicate.declarative context (PlayerQuantity.player g) (Text.pack "gain " <> n) (Text.pack "gains " <> n))

-- | "Flip a coin" (CR 705.1): one coin, whose outcome a later clause reads
-- (Parun.Oracle.Effect.condition).
flipped :: Target.Context -> FlipCoin.FlipCoin -> [Predicate.Predicate]
flipped context f
  | FlipCoin.count f == FlipCoin.defaultCount,
    Maybe.isNothing (FlipCoin.misses f) =
      Predicate.imperative context (FlipCoin.flipper f) (Text.pack "flip a coin") (Text.pack "flips a coin")
  | otherwise = []

-- | "[Player] loses the game" (CR 104.3e).
lostGame :: Target.Context -> PlayerRef.PlayerRef -> [Predicate.Predicate]
lostGame context who = Predicate.declarative context who (Text.pack "lose the game") (Text.pack "loses the game")

-- | "[Player] loses [life]" (CR 119.3), by an effect and untallied.
lost :: Target.Context -> LifeLoss.LifeLoss -> [Predicate.Predicate]
lost context l
  | LifeLoss.cause l == LifeLossCause.ByEffect,
    Maybe.isNothing (LifeLoss.tally l) = do
      a <- Target.amounts context (LifeLoss.quantity l)
      n <- life a
      Predicate.naming (Target.names a) (Predicate.declarative context (LifeLoss.player l) (Text.pack "lose " <> n) (Text.pack "loses " <> n))
  | otherwise = []

-- | "Add [mana]" (CR 106.1): your ordinary mana, unrestricted.
addedMana :: Target.Context -> ManaAddition.ManaAddition -> [Predicate.Predicate]
addedMana context a
  | ManaAddition.player a == PlayerRef.Relative PlayerRelation.You,
    ManaAddition.retention a == ManaRetention.Ordinary,
    Maybe.isNothing (ManaAddition.restriction a),
    Maybe.isNothing (ManaAddition.rider a),
    Maybe.isNothing (ManaAddition.whenSpent a) = do
      amount' <- Target.amounts context (ManaAddition.count a)
      (symbols, phrase) <- case Target.reading amount' of
        Target.Stated q -> added (ManaAddition.production a) q
        -- Mana for each object counted stands alone: "Add {B} for each
        -- Swamp you control." (Cabal Coffers).
        Target.ForEach n each' -> do
          (_, phrase) <- added (ManaAddition.production a) (Quantity.Literal n)
          [(Predicate.Described, phrase <> Text.pack " for each " <> each')]
        -- One colour for every unit: "Add X mana of any one color, where X
        -- is ..." (Deathbloom Ritualist, CR 105.4).
        Target.EqualTo equal
          | ManaAddition.production a == ManaProduction.AnyColor ColorSpread.OneColor ->
              [(Predicate.Described, Text.pack "X mana of any one color, where X is " <> equal)]
          | otherwise -> []
      [Predicate.MkPredicate Nothing (Text.pack "add " <> phrase) [] (Target.names amount') (Just symbols) False]
  | otherwise = []

-- | "That player sacrifices a creature of their choice" (CR 701.21a): a
-- stated number of permanents a player a slot names chooses. Free: "of their
-- choice" or not, Oracle wording edicts both ways. A player named otherwise
-- is not spelled out here.
sacrificedBy :: Target.Context -> PlayerSacrifices.PlayerSacrifices -> [Predicate.Predicate]
sacrificedBy context s = case s of
  PlayerSacrifices.MkPlayerSacrifices who Nothing f (Quantity.Literal n) Nothing
    | slotted who -> do
        p <- Filter.phrase f (Target.keyword context)
        noun <- Cost.some n (Filter.MkNoun (Text.pack "permanent") (Just (Text.pack "permanents"))) p
        choice <- [Text.pack " of their choice", Text.empty]
        Predicate.imperative context who (Text.pack "sacrifice " <> noun) (Text.pack "sacrifices " <> noun <> choice)
  _ -> []

-- | "Prevent the next [N] damage that would be dealt to [object] this turn"
-- (CR 615.7).
prevented :: Target.Context -> PreventNextDamage.PreventNextDamage effect -> [Predicate.Predicate]
prevented context p
  | PreventNextDamage.duration p == Duration.UntilEndOfTurn,
    Maybe.isNothing (PreventNextDamage.whatRecipient p),
    Maybe.isNothing (PreventNextDamage.whoRecipient p),
    Maybe.isNothing (PreventNextDamage.chosenSource p),
    null (PreventNextDamage.riders p),
    Just r <- PreventNextDamage.ref p = do
      who <- Target.object context Target.Each r
      n <- Target.amount (PreventNextDamage.quantity p)
      let kind = case PreventNextDamage.kind p of
            Nothing -> Text.empty
            Just DamageKind.Combat -> Text.pack "combat "
            Just DamageKind.Noncombat -> Text.pack "noncombat "
      [Predicate.MkPredicate Nothing (Text.pack "prevent the next " <> n <> Text.pack " " <> kind <> Text.pack "damage that would be dealt to " <> Target.wording who <> Text.pack " this turn") [] (Target.mentions who) Nothing False]
  | otherwise = []

-- | Whether a reference names the players a slot holds.
slotted :: PlayerRef.PlayerRef -> Bool
slotted ref = case ref of
  PlayerRef.InSlot _ -> True
  PlayerRef.EachInSlot _ -> True
  _ -> False

-- | "[Player] discards [cards]", of the player's choice or at random (CR
-- 701.9). Cards another player chooses, or a set the instruction names, are
-- not spelled out here.
discarded :: Target.Context -> Discard.Discard -> [Predicate.Predicate]
discarded context d = case d of
  Discard.Counted c -> do
    n <- Target.cards (CountedDiscard.quantity c)
    Predicate.imperative context (CountedDiscard.player c) (Text.pack "discard " <> n) (Text.pack "discards " <> n)
  Discard.These t
    | ObjectRef.RandomCard r <- TheseDiscard.cards t,
      RandomCard.zone r == HiddenZone.Hand,
      RandomCard.filter r == Filter.Type.And [] -> do
        n <- Target.cards (RandomCard.count r)
        let random = n <> Text.pack " at random"
        Predicate.imperative context (RandomCard.player r) (Text.pack "discard " <> random) (Text.pack "discards " <> random)
  _ -> []

-- | The mana an instruction adds: "{G}", "{C}{C}", "one mana of any color",
-- "two mana in any combination of colors", "two mana of different colors"
-- (CR 106.1).
added :: ManaProduction.ManaProduction -> Quantity.Quantity -> [(Predicate.Mana, Text.Text)]
added production count = case (production, count) of
  (ManaProduction.OfType t, Quantity.Literal n)
    | n >= 1 ->
        let symbols = Text.concat (List.genericReplicate n (Text.pack "{" <> Cost.manaType t <> Text.pack "}"))
         in [(Predicate.Symbols symbols, symbols)]
  (ManaProduction.AnyColor _, Quantity.Literal 1) -> [(Predicate.Described, Text.pack "one mana of any color")]
  (ManaProduction.InCommanderIdentity, Quantity.Literal 1) -> [(Predicate.Described, Text.pack "one mana of any color in your commander's color identity")]
  (ManaProduction.AnyColor ColorSpread.AnyCombination, Quantity.Literal n)
    | n >= 2 -> [(Predicate.Described, Prose.cardinal n <> Text.pack " mana in any combination of colors")]
  (ManaProduction.AnyColor ColorSpread.DifferentColors, Quantity.Literal n)
    | n >= 2 -> [(Predicate.Described, Prose.cardinal n <> Text.pack " mana of different colors")]
  _ -> []

-- | "[Source] deals [amount] damage to [recipient]", one part after another
-- ("4 damage to any target and 2 damage to you"), or damage divided among a
-- slot's targets (CR 601.2d). Parts naming groups share an amount they share
-- ("X damage to each creature and each player", Earthquake); any other part
-- states its own ("1 damage to any target and 1 damage to you", Brothers of
-- Fire). The source is the card, or an object a slot names ("Target creature
-- you control deals damage", Soul's Fire).
dealDamage :: Target.Context -> DealDamage.DealDamage -> [Predicate.Predicate]
dealDamage context d
  | Maybe.isJust (DealDamage.dealers d) || Maybe.isJust (DealDamage.excess d) || Maybe.isJust (DealDamage.tally d) || Maybe.isJust (DealDamage.damaged d) || Maybe.isJust (DealDamage.excessTally d) = []
  | otherwise = do
      source <- Target.object context Target.All (ObjectRef.InSlot (Maybe.fromMaybe Target.selfSlot (DealDamage.dealer d)))
      -- The source is the sentence's subject, so the amount calls it "it":
      -- "deals damage equal to its power" (Ghitu Fire-Eater).
      let measured = context {Target.self = Text.pack "it"}
      case Foldable.toList (DealDamage.parts d) of
        [DamagePart.MkDamagePart (ObjectRef.InSlot s) (Quantity.Share t)]
          | s == t -> do
              (ref, total) <- Target.among context s
              n <- Target.amount total
              [Predicate.MkPredicate (Just source) (Text.pack "deals " <> n <> Text.pack " damage divided as you choose among " <> Target.wording ref) [] (Target.mentions ref) Nothing False]
        parts -> do
          let shared = all (each . DamagePart.ref) parts
              groups
                | shared = List.groupBy (\a b -> DamagePart.quantity a == DamagePart.quantity b) parts
                | otherwise = fmap pure parts
          -- Free when the card deals damage to itself: its own name or "itself"
          -- (Reckless Embermage).
          let itself = Maybe.isNothing (DealDamage.dealer d) && any ((== ObjectRef.InSlot Target.selfSlot) . DamagePart.ref) parts
          recipients <- context : [context {Target.self = Text.pack "itself"} | itself]
          rendered <- traverse (damagePart recipients measured) groups
          [Predicate.MkPredicate (Just source) (Text.pack "deals " <> Text.intercalate (Text.pack " and ") (fmap fst rendered)) [] (concatMap snd rendered) Nothing False]

-- | Whether the reference names a group rather than one object or player:
-- "each creature", "each player".
each :: ObjectRef.ObjectRef -> Bool
each ref = case ref of
  ObjectRef.EachMatching _ -> True
  ObjectRef.Players (PlayersWho.MkPlayersWho (PlayerRef.Relative PlayerRelation.AnyPlayer) Nothing Nothing) -> True
  ObjectRef.Players (PlayersWho.MkPlayersWho (PlayerRef.Relative PlayerRelation.Opponent) Nothing Nothing) -> True
  _ -> False

-- | Damage parts sharing an amount: "2 damage to each creature and each
-- player", or damage equal to a number the second context reads. Free:
-- "equal to" after the recipients (Rockslide Ambush) or before them (Soul's
-- Fire), after first for a count and before first for anything else. Damage
-- "for each" object counted is not spelled out here: grep "deals [0-9X]+
-- damage to [^.]* for each" over data/cards, 2026-10-22, finds no count.
damagePart :: Target.Context -> Target.Context -> [DamagePart.DamagePart] -> [(Text.Text, [SlotName.SlotName])]
damagePart context measured group = case group of
  first : _ -> do
    a <- Target.amounts measured (DamagePart.quantity first)
    refs <- traverse (Target.object context Target.Each . DamagePart.ref) group
    let listed = Prose.listing "and" (fmap Target.wording refs)
        theirs kinds = Text.pack ("each opponent and each " <> kinds)
        -- Free: the opponents' permanents after "each opponent" are "they
        -- control" (Tectonic Hazard), "creature or planeswalker" then
        -- "creature and planeswalker" (End the Festivities).
        free =
          [ (theirs "creature an opponent controls", theirs "creature they control"),
            (theirs "creature or planeswalker an opponent controls", theirs "creature and planeswalker they control")
          ]
    recipients <- listed : [Text.replace from to listed | (from, to) <- free, Text.isInfixOf from listed]
    let to = Text.pack "damage to " <> recipients
        counted = case DamagePart.quantity first of
          Quantity.Count _ -> True
          _ -> False
    text <- case Target.reading a of
      Target.Stated q -> fmap (<> Text.pack " " <> to) (Target.amount q)
      Target.ForEach _ _ -> []
      Target.EqualTo e ->
        let later = to <> Text.pack " equal to " <> e
            before = Text.pack "damage equal to " <> e <> Text.pack " to " <> recipients
         in if counted then [later, before] else [before, later]
    [(text, concatMap Target.mentions refs <> Target.names a)]
  [] -> []

-- | An amount of life: "4 life", "2 life for each creature you control",
-- "life equal to its power". Free for a count: "for each" (Fruition) or
-- "equal to the number of" (Clearwater Goblet).
life :: Target.Amount -> [Text.Text]
life a = case Target.reading a of
  Target.Stated q -> fmap (<> Text.pack " life") (Target.amount q)
  Target.ForEach n each' -> [Text.pack (show n <> " life for each ") <> each']
  Target.EqualTo e -> [Text.pack "life equal to " <> e]
