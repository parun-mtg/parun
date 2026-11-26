-- | A game's things in a player's words: seats, objects, mana, counters,
-- witnessed entries and results, as the web client and the line protocol say
-- them. Reads nothing but the decoded view, and a card's printed text by a
-- name the view shows.
module Parun.Play.Wording where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Keyword as Engine.Keyword
import qualified Parun.Engine.Sba as Sba
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Oracle as Oracle
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Types.AbilityFacts as AbilityFacts
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityTriggered as AbilityTriggered
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.AttackerDeclared as AttackerDeclared
import qualified Parun.Types.Ballot as Ballot
import qualified Parun.Types.BallotChoice as BallotChoice
import qualified Parun.Types.BecameAttached as BecameAttached
import qualified Parun.Types.BecameBlocking as BecameBlocking
import qualified Parun.Types.BecameDesignated as BecameDesignated
import qualified Parun.Types.BecameTarget as BecameTarget
import qualified Parun.Types.BecameUnattached as BecameUnattached
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ChoseSecretly as ChoseSecretly
import qualified Parun.Types.ClassLevel as ClassLevel
import qualified Parun.Types.ClassLevelChange as ClassLevelChange
import qualified Parun.Types.CoinFlipped as CoinFlipped
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.CommittedCrime as CommittedCrime
import qualified Parun.Types.ControlChanged as ControlChanged
import qualified Parun.Types.Convoking as Convoking
import qualified Parun.Types.CountedAct as CountedAct
import qualified Parun.Types.CounterChange as CounterChange
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterName as CounterName
import qualified Parun.Types.Countering as Countering
import qualified Parun.Types.CountersPlaced as CountersPlaced
import qualified Parun.Types.Crewing as Crewing
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamagePrevented as DamagePrevented
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.Defense as Defense
import qualified Parun.Types.Departure as Departure
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.DieResult as DieResult
import qualified Parun.Types.Discarded as Discarded
import qualified Parun.Types.Drew as Drew
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Enlisted as Enlisted
import qualified Parun.Types.Expended as Expended
import qualified Parun.Types.Exploited as Exploited
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FacedDilemma as FacedDilemma
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.HalfUnlocked as HalfUnlocked
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordExpansion as KeywordExpansion
import qualified Parun.Types.LandPlayed as LandPlayed
import qualified Parun.Types.LeftTheGame as LeftTheGame
import qualified Parun.Types.LifeChange as LifeChange
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Looked as Looked
import qualified Parun.Types.LossCause as LossCause
import qualified Parun.Types.Loyalty as Loyalty
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ManifestedDread as ManifestedDread
import qualified Parun.Types.Mentored as Mentored
import qualified Parun.Types.Milled as Milled
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.MulliganDecision as MulliganDecision
import qualified Parun.Types.MulliganDeclared as MulliganDeclared
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlanarDieRolled as PlanarDieRolled
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerCountersPlaced as PlayerCountersPlaced
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Revealed as Revealed
import qualified Parun.Types.RollReading as RollReading
import qualified Parun.Types.Saddling as Saddling
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.SecretChoice as SecretChoice
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SpellWasCast as SpellWasCast
import qualified Parun.Types.SpellWasCopied as SpellWasCopied
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.StackedAbility as StackedAbility
import qualified Parun.Types.Status as Status
import qualified Parun.Types.StickerKind as StickerKind
import qualified Parun.Types.StickerPut as StickerPut
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Transformed as Transformed
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TurnedFaceUp as TurnedFaceUp
import qualified Parun.Types.TurnedUpOffDeck as TurnedUpOffDeck
import qualified Parun.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Parun.Types.Voted as Voted
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange

-- | Seats are numbered from one on screen.
player :: PlayerId.PlayerId -> Text.Text
player pid = Text.pack ("Player " <> show (PlayerId.unwrap pid + 1))

-- | A team's name (CR 808.1), "Team 1" for the first, as the lobby names it.
team :: TeamId.TeamId -> Text.Text
team t = Text.pack ("Team " <> show (TeamId.unwrap t + 1))

-- | A hand's cards, lands first, then spells, each by `ordered`.
handOrder :: PlayerView.PlayerView -> [ObjectView.ObjectView] -> [ObjectView.ObjectView]
handOrder view held =
  let (lands, spells) = List.partition (isA CardType.Land) held
   in ordered view lands <> ordered view spells

-- | Whether the seat sees the object as having a card type.
isA :: CardType.CardType -> ObjectView.ObjectView -> Bool
isA t = maybe False (Set.member t . ProjectedCharacteristics.cardTypes) . ObjectView.characteristics

-- | By mana value, then name, ties in the view's order.
ordered :: PlayerView.PlayerView -> [ObjectView.ObjectView] -> [ObjectView.ObjectView]
ordered view = List.sortOn (\o -> (Maybe.fromMaybe 0 (ObjectView.characteristics o >>= ProjectedCharacteristics.manaValue), Text.toLower (nameOf view o)))

-- | An object's name, power and toughness, status, which ability it is (CR
-- 113.7a), and what it targets (CR 601.2c / 602.2b / 603.3d).
describe :: PlayerView.PlayerView -> ObjectView.ObjectView -> Text.Text
describe view o =
  let pt = do
        c <- ObjectView.characteristics o
        p <- ProjectedCharacteristics.power c
        t <- ProjectedCharacteristics.toughness c
        pure (Text.pack (" " <> show p <> "/" <> show t))
      status =
        [Text.pack "tapped" | ObjectView.tapped o == TapState.Tapped]
          <> [Text.pack (show (ObjectView.damage o) <> " damage") | ObjectView.damage o > 0]
          <> [counters n kind | (kind, n) <- Map.toList (ObjectView.counters o), n > 0]
      targeting = case ObjectView.targets o of
        [] -> Text.empty
        targets -> Text.pack " targeting " <> listing (fmap (recipientName view) targets)
      which = foldMap (\text -> Text.pack " \"" <> text <> Text.pack "\"") (ObjectView.ability o >>= abilityText view)
   in nameOf view o <> Maybe.fromMaybe Text.empty pt <> (if null status then Text.empty else Text.pack " (" <> Text.intercalate (Text.pack ", ") status <> Text.pack ")") <> which <> targeting

-- | CR 113.7a: an ability on the stack's Oracle text, which tells apart two
-- abilities of one source, its source called by the name the seat knows it
-- by, else "this object"; Nothing for one Parun.Oracle does not render yet.
abilityText :: PlayerView.PlayerView -> AbilityView.AbilityView -> Maybe Text.Text
abilityText view a = stackedText view (AbilityView.source a) (AbilityView.ability a)

-- | An ability's Oracle text, its source as abilityText calls it.
stackedText :: PlayerView.PlayerView -> Maybe ObjectId.ObjectId -> StackedAbility.StackedAbility -> Maybe Text.Text
stackedText view source stacked = Maybe.listToMaybe $ case stacked of
  StackedAbility.Activated activated -> Oracle.activated context activated <> keyworded activated
  StackedAbility.Triggered triggered -> Oracle.triggered context creature triggered
  where
    name = Maybe.fromMaybe (Text.pack "this object") (source >>= knownName view)
    context = Oracle.contextOf name []
    -- CR 702.1: a keyword's ability as the keyword prints, "Cycling {2}",
    -- where that keyword mints no other activated ability; reconfigure's two
    -- (CR 702.151a) are not told apart by it.
    keyworded activated =
      [ text
      | Just k <- [ActivatedAbility.keyword activated],
        let minted = Engine.Keyword.expansionOf k,
        length (KeywordExpansion.fromHand minted <> KeywordExpansion.fromGraveyard minted <> KeywordExpansion.onBattlefield minted 1) == 1,
        text <- Oracle.Keyword.printed name k
      ]
    creature = any (isA CardType.Creature) (source >>= (`Map.lookup` objects view))

-- | CR 708.2a: a face-down object has no name, and a hidden one shows none;
-- CR 406.3 / 708.5: a face-down card the seat may look at names its card too.
-- CR 113.7a / 602.2a: an ability has no name, so it is named by its source.
nameOf :: PlayerView.PlayerView -> ObjectView.ObjectView -> Text.Text
nameOf view o = case ObjectView.ability o of
  Just a -> maybe (Text.pack "an ability") (<> Text.pack "'s ability") (AbilityView.source a >>= knownName view)
  Nothing -> cardName o

-- | A card's name as the seat sees it.
cardName :: ObjectView.ObjectView -> Text.Text
cardName o =
  let shown = case ObjectView.characteristics o of
        Nothing | Maybe.isNothing (ObjectView.face o) -> Text.pack "a hidden card"
        Just c | names <- Set.toList (ProjectedCharacteristics.names c), not (null names) -> joined names
        -- A Room with both doors locked has neither half's name (CR 709.5).
        Just c | Just whole <- ProjectedCharacteristics.halves c -> joined (fmap Face.name (Foldable.toList (Card.faces whole))) <> Text.pack " (locked)"
        _ -> Text.pack "a face-down card"
      joined = Text.intercalate (Text.pack " // ") . fmap CardName.unwrap
      card = foldMap (\f -> Text.pack " (" <> joined (Set.toList (ProjectedCharacteristics.names f)) <> Text.pack ")") (ObjectView.face o)
   in shown <> card

-- | The names a seat may read an object's card by: the card behind a face-down
-- object it may look at (CR 708.5), else the object's own, which a face-down
-- one has none of (CR 708.2a).
cardNames :: ObjectView.ObjectView -> Set.Set CardName.CardName
cardNames o = maybe (foldMap readable (ObjectView.characteristics o)) ProjectedCharacteristics.names (ObjectView.face o)
  where
    -- A Room's locked halves still print their text (CR 709.5).
    readable c = ProjectedCharacteristics.names c <> foldMap (Set.fromList . fmap Face.name . Foldable.toList . Card.faces) (ProjectedCharacteristics.halves c)

-- | An object in full, as the seat sees it: its summary, its characteristics
-- now, the card behind it when face down and the seat may look, a foretell
-- cost it was given (CR 702.143e), its counters, and the printed Oracle text
-- of the faces it is named for; or why the card data could not say.
details :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerView.PlayerView -> ObjectView.ObjectView -> Either InvalidCorpus.InvalidCorpus [Text.Text]
details lookUp view o = do
  looked <- traverse lookUp (Set.toList names)
  let faces = concatMap (filter ((`Set.member` names) . Face.name) . Foldable.toList . Card.faces) (ListUtils.nubOrd (Maybe.catMaybes looked))
      oracle f = [CardName.unwrap (Face.name f) <> Text.pack ":" | length faces > 1] <> foldMap Text.lines (Face.oracleText f)
  pure $
    [describe view o]
      <> foldMap characteristicLines (ObjectView.characteristics o)
      <> foldMap (\f -> Text.pack "Face down. You may look at it:" : characteristicLines f) (ObjectView.face o)
      <> foldMap (\amount -> [Text.pack "Foretell cost: its mana cost reduced by " <> manaCost amount]) (ObjectView.foretellCostReduction o)
      <> [Text.pack "Counters: " <> Text.intercalate (Text.pack ", ") (fmap counted (Map.toList (ObjectView.counters o))) | not (Map.null (ObjectView.counters o))]
      <> foldMap oracle faces
  where
    names = cardNames o
    counted (kind, n) = Text.pack (show n <> " ") <> counterName kind

-- | CR 201.1 / 202.1 / 205.1 / 208.1: the name line with its mana cost, the type line, and
-- power and toughness, loyalty or defense; the subtypes in printed order
-- (Parun.Engine.Subtype.inPrintedOrder).
characteristicLines :: ProjectedCharacteristics.ProjectedCharacteristics -> [Text.Text]
characteristicLines c =
  [Text.unwords (title <> foldMap (pure . manaCost) (ProjectedCharacteristics.manaCost c)) | not (null title) || Maybe.isJust (ProjectedCharacteristics.manaCost c)]
    <> [Text.unwords (supertypes <> types <> if null subtypes then [] else Text.pack "\8212" : subtypes)]
    <> foldMap (\(p, t) -> [Text.pack (show p <> "/" <> show t)]) ((,) <$> ProjectedCharacteristics.power c <*> ProjectedCharacteristics.toughness c)
    <> foldMap (\l -> [Text.pack "Loyalty " <> loyalty l]) (ProjectedCharacteristics.loyalty c)
    <> foldMap (\d -> [Text.pack ("Defense " <> show (Defense.unwrap d))]) (ProjectedCharacteristics.defense c)
  where
    title = [Text.intercalate (Text.pack " // ") (fmap CardName.unwrap (Set.toList (ProjectedCharacteristics.names c))) | not (Set.null (ProjectedCharacteristics.names c))]
    supertypes = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.supertypes c))
    types = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.cardTypes c))
    subtypes = fmap (Text.pack . show) (Subtype.inPrintedOrder c)
    loyalty l = case l of
      Loyalty.Literal n -> Text.pack (show n)
      Loyalty.Variable -> Text.pack "X"

-- | CR 122.1: a counter's kind as a player says it.
counterName :: CounterKind.CounterKind Keyword.Keyword -> Text.Text
counterName kind = case kind of
  CounterKind.PlusOnePlusOne -> Text.pack "+1/+1"
  CounterKind.MinusOneMinusOne -> Text.pack "-1/-1"
  -- CR 122.1b: a keyword counter by its keyword, as Oracle text prints it.
  CounterKind.Keyword k | word : _ <- Oracle.Keyword.keywordWord Text.empty k -> word
  CounterKind.UnsafePowerToughness p t -> Text.pack (signed p <> "/" <> signed t)
  -- CR 122.1: a named kind by the name the card prints.
  CounterKind.Named name -> CounterName.unwrap name
  other -> Text.unwords (Text.words (spaced (Text.pack (show other))))
  where
    signed n = (if n < 0 then "-" else "+") <> show (abs n)

-- | CR 107.4's symbols as the cost is printed.
manaCost :: ManaCost.ManaCost -> Text.Text
manaCost cost = case ManaCost.unwrap cost of
  [] -> Text.pack "{0}"
  symbols -> foldMap symbol symbols
  where
    symbol s = Text.pack "{" <> inside s <> Text.pack "}"
    inside s = case s of
      ManaSymbol.Generic n -> Text.pack (show n)
      ManaSymbol.OfType t -> manaType t
      ManaSymbol.Hybrid h -> manaType (Hybrid.left h) <> Text.pack "/" <> manaType (Hybrid.right h)
      ManaSymbol.MonocoloredHybrid t -> Text.pack "2/" <> manaType t
      ManaSymbol.Phyrexian c -> manaType (ManaType.Colored c) <> Text.pack "/P"
      ManaSymbol.HybridPhyrexian h -> manaType (ManaType.Colored (HybridPhyrexian.left h)) <> Text.pack "/" <> manaType (ManaType.Colored (HybridPhyrexian.right h)) <> Text.pack "/P"
      ManaSymbol.Snow -> Text.pack "S"
      ManaSymbol.Variable -> Text.pack "X"

-- | CR 107.4: a mana type's symbol letter.
manaType :: ManaType.ManaType -> Text.Text
manaType t = Text.pack $ case t of
  ManaType.Colored Color.White -> "W"
  ManaType.Colored Color.Blue -> "U"
  ManaType.Colored Color.Black -> "B"
  ManaType.Colored Color.Red -> "R"
  ManaType.Colored Color.Green -> "G"
  ManaType.Colorless -> "C"

-- | Every object the view shows, by id.
objects :: PlayerView.PlayerView -> Map.Map ObjectId.ObjectId ObjectView.ObjectView
objects view =
  Map.fromList . fmap (\o -> (ObjectView.object o, o)) $
    PlayerView.battlefield view
      <> PlayerView.phasedOut view
      <> PlayerView.command view
      <> PlayerView.stack view
      <> concat (PlayerView.parkedStacks view)
      <> concatMap (\s -> SeatView.hand s <> SeatView.library s <> SeatView.graveyard s <> SeatView.exile s <> SeatView.ante s) (PlayerView.seats view)

-- | An object by id as the view shows it, or else as the last witnessed move,
-- look or reveal of it showed it: its characteristics then and no status, so
-- a card looked at in a library can be read. A snapshot names no owner, so
-- the viewer stands in, which no reader of a recalled object shows.
recalled :: PlayerView.PlayerView -> ObjectId.ObjectId -> Maybe ObjectView.ObjectView
recalled view oid = case Map.lookup oid (objects view) of
  Just o -> Just o
  Nothing -> flip fmap (Map.lookup oid (snapshots view)) $ \c ->
    ObjectView.MkObjectView
      { ObjectView.object = oid,
        ObjectView.owner = PlayerView.viewer view,
        ObjectView.controller = Nothing,
        ObjectView.tapped = TapState.Untapped,
        ObjectView.damage = 0,
        ObjectView.counters = Map.empty,
        ObjectView.attachedTo = Nothing,
        ObjectView.characteristics = Just c,
        ObjectView.face = Nothing,
        ObjectView.foretellCostReduction = Nothing,
        ObjectView.ownEffects = Nothing,
        ObjectView.merged = Seq.empty,
        ObjectView.beneathCopy = Nothing,
        ObjectView.ability = Nothing,
        ObjectView.targets = [],
        ObjectView.summoningSick = False,
        ObjectView.ringBearerFor = Nothing,
        ObjectView.protector = Nothing,
        ObjectView.classLevel = Nothing,
        ObjectView.unlockedHalves = Set.empty,
        ObjectView.designations = Set.empty,
        ObjectView.onAdventure = False,
        ObjectView.ventureRoom = Nothing,
        ObjectView.chosenColors = Map.empty,
        ObjectView.chosenColorless = Map.empty,
        ObjectView.chosenSubtype = Map.empty,
        ObjectView.chosenCardType = Map.empty,
        ObjectView.chosenNames = Map.empty,
        ObjectView.chosenPlayers = Map.empty,
        ObjectView.chosenNumber = Map.empty,
        ObjectView.chosenObjects = Map.empty,
        ObjectView.commanderTax = 0
      }

-- | An object by id, named as the view shows it, or else as the last witnessed
-- move, look or reveal of it showed it, so one that has since left or is
-- hidden again still has a name.
named :: PlayerView.PlayerView -> ObjectId.ObjectId -> Text.Text
named view oid = Maybe.fromMaybe (Text.pack ("object " <> show (ObjectId.unwrap oid))) (knownName view oid)

-- | An object as a prompt lists it, told apart from others of its name: on
-- the battlefield or the stack, by its controller and its status (CR 108.4);
-- in a graveyard, exile or the ante, by its owner and that zone; elsewhere by
-- name.
choosable :: PlayerView.PlayerView -> ObjectId.ObjectId -> Text.Text
choosable view oid = case located view oid of
  Just (Zone.Battlefield, o) -> whose (ObjectView.controller o) <> describe view o
  Just (Zone.Stack, o) -> whose (ObjectView.controller o) <> describe view o <> Text.pack ", on the stack"
  Just (Zone.Graveyard, o) -> whose (Just (ObjectView.owner o)) <> describe view o <> Text.pack ", in a graveyard"
  Just (Zone.Exile, o) -> whose (Just (ObjectView.owner o)) <> describe view o <> Text.pack ", in exile"
  Just (Zone.Ante, o) -> whose (Just (ObjectView.owner o)) <> describe view o <> Text.pack ", in the ante"
  _ -> named view oid
  where
    whose = maybe Text.empty (\pid -> if pid == PlayerView.viewer view then Text.pack "Your " else player pid <> Text.pack "'s ")

-- | The seat an object is listed under: its controller on the battlefield or
-- the stack, else its owner (CR 108.4a).
seatOf :: PlayerView.PlayerView -> ObjectId.ObjectId -> Maybe PlayerId.PlayerId
seatOf view oid = case located view oid of
  Just (z, o) | z `elem` [Zone.Battlefield, Zone.Stack] -> ObjectView.controller o
  Just (_, o) -> Just (ObjectView.owner o)
  Nothing -> Nothing

-- | The zone the view shows an object in, and the object.
located :: PlayerView.PlayerView -> ObjectId.ObjectId -> Maybe (Zone.Zone, ObjectView.ObjectView)
located view oid =
  List.find ((== oid) . ObjectView.object . snd) $
    fmap ((,) Zone.Battlefield) (PlayerView.battlefield view)
      <> fmap ((,) Zone.Stack) (PlayerView.stack view <> concat (PlayerView.parkedStacks view))
      <> concatMap
        ( \s ->
            fmap ((,) Zone.Hand) (SeatView.hand s)
              <> fmap ((,) Zone.Library) (SeatView.library s)
              <> fmap ((,) Zone.Graveyard) (SeatView.graveyard s)
              <> fmap ((,) Zone.Exile) (SeatView.exile s)
              <> fmap ((,) Zone.Ante) (SeatView.ante s)
        )
        (PlayerView.seats view)

-- | A zone as an entry names it to its reader: a library, hand or graveyard
-- by its owner where the entry knows them (CR 400.3), else as
-- PromptOffer.zoneName does.
ownedZone :: Maybe PlayerId.PlayerId -> Maybe PlayerId.PlayerId -> Zone.Zone -> Text.Text
ownedZone reader owner z = case (owner, z) of
  (Just p, Zone.Library) -> possessive reader p <> Text.pack " library"
  (Just p, Zone.Hand) -> possessive reader p <> Text.pack " hand"
  (Just p, Zone.Graveyard) -> possessive reader p <> Text.pack " graveyard"
  _ -> PromptOffer.zoneName z

-- | A seat as a sentence's object, to the sentence's reader: "you" for the
-- reader, else as `player` names it. The reader is the seat a sentence
-- addresses, if any: the web client's viewer; nobody for the line protocol.
objective :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> Text.Text
objective reader pid = if reader == Just pid then Text.pack "you" else player pid

-- | A seat's, to the sentence's reader: "your", else "Player 1's".
possessive :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> Text.Text
possessive reader pid = if reader == Just pid then Text.pack "your" else player pid <> Text.pack "'s"

-- | What a seat does, to the sentence's reader, the predicate given in the
-- third person: "Player 1 casts", or "You cast" for the reader.
acts :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> String -> Text.Text
acts reader pid predicate = subject reader pid <> Text.pack " " <> conjugated reader pid predicate

-- | A seat as a sentence's subject, to the sentence's reader: "You", else as
-- `player` names it.
subject :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> Text.Text
subject reader pid = if reader == Just pid then Text.pack "You" else player pid

-- | A third-person predicate as said of the seat: its first word in the
-- second person for the reader ("has" as "have", "draws" as "draw").
conjugated :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> String -> Text.Text
conjugated reader pid predicate
  | reader == Just pid, (verb, rest) <- Text.break (== ' ') (Text.pack predicate), (word, punctuation) <- Text.span Char.isLetter verb = secondPerson word <> punctuation <> rest
  | otherwise = Text.pack predicate
  where
    secondPerson word
      | word == Text.pack "has" = Text.pack "have"
      | word == Text.pack "is" = Text.pack "are"
      | Just stem <- Text.stripSuffix (Text.pack "ies") word = stem <> Text.pack "y"
      | Just stem <- Text.stripSuffix (Text.pack "es") word, any ((`Text.isSuffixOf` stem) . Text.pack) ["ch", "sh", "ss", "x"] = stem
      | otherwise = Maybe.fromMaybe word (Text.stripSuffix (Text.pack "s") word)

-- | The seat's own, to the sentence's reader: "your", else "their".
their :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> Text.Text
their reader pid = if reader == Just pid then Text.pack "your" else Text.pack "their"

-- | An object's name, if the view shows it or a witnessed move, look or
-- reveal of it did.
knownName :: PlayerView.PlayerView -> ObjectId.ObjectId -> Maybe Text.Text
knownName view oid = case Map.lookup oid (objects view) of
  Just o -> Just (nameOf view o)
  Nothing -> fmap PromptOffer.snapshotName (Map.lookup oid (snapshots view))

-- | What each witnessed move, look (CR 701.20e) or reveal (CR 701.20a)
-- showed its object as, a move by either end's id, the latest winning: a
-- library card looked at keeps its name while the view hides it.
snapshots :: PlayerView.PlayerView -> Map.Map ObjectId.ObjectId ProjectedCharacteristics.ProjectedCharacteristics
snapshots view =
  Map.fromList
    [ shown
    | happened <- fmap LoggedEvent.event (witnessed view),
      shown <- case happened of
        GameEvent.Moved m -> [(oid, Moved.characteristics m) | oid <- ZoneChange.departed (Moved.change m) : Foldable.toList (Moved.arrivals m)]
        GameEvent.Looked l -> [(Looked.card l, Looked.characteristics l)]
        GameEvent.Revealed r -> [(Revealed.card r, Revealed.characteristics r)]
        _ -> []
    ]

-- | The entries the view says the seat witnessed, last turn's and this
-- turn's, oldest first.
witnessed :: PlayerView.PlayerView -> [LoggedEvent.LoggedEvent]
witnessed view = PlayerView.witnessedLastTurn view <> PlayerView.witnessed view

-- | Witnessed entries as sentences to a reader (objective), each of
-- `runs` told as one.
events :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> [LoggedEvent.LoggedEvent] -> [Text.Text]
events reader view = concatMap (sentences reader view) . runs reader view

-- | Witnessed entries as `kept` keeps them, a run of one player's draws,
-- looks or mana abilities (those paying for a spell, CR 601.2g) together.
runs :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> [LoggedEvent.LoggedEvent] -> [[LoggedEvent.LoggedEvent]]
runs reader view = List.groupBy (\a b -> Maybe.isJust (runner a) && runner a == runner b) . kept . filter (Maybe.isJust . event reader view)
  where
    -- The player and the predicate a run of their entries shares.
    runner logged = case LoggedEvent.event logged of
      GameEvent.Drew d -> Just (Drew.player d, "draws")
      GameEvent.Looked l -> Just (Looked.player l, "looks at")
      GameEvent.AbilityActivated a | isManaAbility a -> Just (AbilityWasActivated.player a, "gets mana from")
      _ -> Nothing

-- | One of `runs` as sentences to a reader: a run of draws, looks or mana
-- abilities as one, anything else a sentence an entry.
sentences :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> [LoggedEvent.LoggedEvent] -> [Text.Text]
sentences reader view run = case run of
  _ | Just (pid, sources) <- manaRun view run -> [gotMana reader pid sources]
  first : _ : _
    | Just (pid, predicate, what) <- together first ->
        [acts reader pid predicate <> Text.pack " " <> what <> Text.pack "."]
  _ -> concatMap (\logged -> foldMap (: announced reader view logged) (event reader view logged)) run
  where
    together logged = case LoggedEvent.event logged of
      GameEvent.Drew d -> Just (Drew.player d, "draws", count (List.genericLength run) "card")
      GameEvent.Looked l -> Just (Looked.player l, "looks at", listing (Maybe.mapMaybe looked run))
      _ -> Nothing
    looked logged = case LoggedEvent.event logged of
      GameEvent.Looked l -> Just (PromptOffer.snapshotName (Looked.characteristics l))
      _ -> Nothing

-- | A cast spell's (CR 601.2c) or activated ability's (CR 602.2b) targets, to
-- a reader, a sentence each in the order announced: told after its cast or
-- activation, logged once the spell becomes cast (CR 601.2i).
announced :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> LoggedEvent.LoggedEvent -> [Text.Text]
announced reader view logged = case LoggedEvent.event logged of
  GameEvent.SpellCast c -> toldOf (SpellWasCast.spell c) (PromptOffer.snapshotName (SpellWasCast.characteristics c))
  GameEvent.AbilityActivated a
    | Just ability <- AbilityWasActivated.ability a -> toldOf ability (named view (AbilityWasActivated.source a) <> Text.pack "'s ability")
  _ -> []
  where
    toldOf oid what =
      [ Text.unwords [what, Text.pack "targets", recipientTo reader view (BecameTarget.targeted b)] <> Text.pack "."
      | GameEvent.BecameTarget b <- fmap LoggedEvent.event (witnessed view),
        BecameTarget.source b == oid,
        BecameTarget.kind b /= StackObjectKind.TriggeredAbility
      ]

-- | A run of one player's mana abilities (CR 605.1a): the player and the
-- names of the abilities' sources.
manaRun :: PlayerView.PlayerView -> [LoggedEvent.LoggedEvent] -> Maybe (PlayerId.PlayerId, [Text.Text])
manaRun view run = do
  activations <- traverse activation run
  pid : rest <- Just (fmap fst activations)
  if all (== pid) rest then Just (pid, fmap snd activations) else Nothing
  where
    activation logged = case LoggedEvent.event logged of
      GameEvent.AbilityActivated a | isManaAbility a -> Just (AbilityWasActivated.player a, named view (AbilityWasActivated.source a))
      _ -> Nothing

-- | A player getting mana from these sources (manaRun), to a reader.
gotMana :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> [Text.Text] -> Text.Text
gotMana reader pid sources = acts reader pid "gets mana from" <> Text.pack " " <> listing sources <> Text.pack "."

-- | The entries a log tells, less those another entry beside them already
-- tells or no seat acts on: a search's looks at the cards it searches (CR
-- 701.23a), told by the search; a drawn card's move to its owner's hand,
-- which a seat sees where every player sees that hand (Visibility.witnessed),
-- told by the draw after it (CR 121.1); the life a player loses to damage just
-- dealt them (CR 120.3a), told by the damage; and the crime committed (CR
-- 700.13) and mana spent on spells (CR 700.14), which only an ability
-- reading them asks after.
kept :: [LoggedEvent.LoggedEvent] -> [LoggedEvent.LoggedEvent]
kept = go Nothing Map.empty
  where
    -- The player whose search the looks that follow are part of, and the
    -- damage each player was just dealt that no life lost has told yet.
    go searcher dealt entries = case entries of
      [] -> []
      logged : rest -> case LoggedEvent.event logged of
        GameEvent.Expended _ -> go searcher dealt rest
        GameEvent.CommittedCrime _ -> go searcher dealt rest
        GameEvent.Looked l | searcher == Just (Looked.player l) -> go searcher dealt rest
        GameEvent.Moved m
          | ZoneChange.from (Moved.change m) == Zone.Library,
            ZoneChange.to (Moved.change m) == Zone.Hand,
            next : _ <- rest,
            GameEvent.Drew d <- LoggedEvent.event next,
            fmap ObjectSnapshot.owner (Moved.arrived m) == Just (Drew.player d) ->
              go searcher dealt rest
        GameEvent.LifeLost l
          | Just n <- Map.lookup (LifeChange.player l) dealt,
            n >= LifeChange.amount l ->
              go Nothing (Map.insert (LifeChange.player l) (n - LifeChange.amount l) dealt) rest
        GameEvent.DamageDealt d
          | Recipient.ToPlayer pid <- DamageEvent.target d -> logged : go Nothing (Map.insertWith (+) pid (DamageEvent.amount d) dealt) rest
          | otherwise -> logged : go Nothing dealt rest
        GameEvent.PlayerActed a | PlayerActed.action a == PlayerAction.SearchLibrary -> logged : go (Just (PlayerActed.player a)) Map.empty rest
        _ -> logged : go Nothing Map.empty rest

-- | A witnessed entry as a sentence naming its cards, players and amounts, or
-- nothing for the bookkeeping a seat can read off the board (a step, a tap,
-- mana) or another entry already tells (a die roll, a spell's move
-- to the stack, a land play's, discard's or mill's move, a block's
-- per-attacker records, a mana ability's tap and mana, told by its
-- activation); to a reader, as objective says.
event :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> LoggedEvent.LoggedEvent -> Maybe Text.Text
event reader view logged = case LoggedEvent.event logged of
  GameEvent.SpellCast c -> said [acts reader (SpellWasCast.player c) "casts", PromptOffer.snapshotName (SpellWasCast.characteristics c)]
  GameEvent.AbilityActivated a
    | isManaAbility a -> said [acts reader (AbilityWasActivated.player a) "gets mana from", name (AbilityWasActivated.source a)]
    | otherwise -> said [acts reader (AbilityWasActivated.player a) "activates", name (AbilityWasActivated.source a) <> Text.pack "'s ability"]
  GameEvent.LandPlayed l -> said [acts reader (LandPlayed.player l) "plays", name (LandPlayed.land l)]
  GameEvent.AttackerDeclared a -> said [name (AttackerDeclared.attacker a), Text.pack "attacks", attacked (AttackerDeclared.target a)]
  GameEvent.BecameBlocking b -> said [name (BecameBlocking.blocker b), Text.pack "blocks", name (BecameBlocking.attacker b)]
  GameEvent.DamageDealt d -> said [name (DamageEvent.source d), Text.pack "deals", Text.pack (show (DamageEvent.amount d)), Text.pack "damage to", recipient (DamageEvent.target d)]
  GameEvent.LifeLost l -> said [acts reader (LifeChange.player l) "loses", Text.pack (show (LifeChange.amount l)), Text.pack "life"]
  GameEvent.LifeGained l -> said [acts reader (LifeChange.player l) "gains", Text.pack (show (LifeChange.amount l)), Text.pack "life"]
  GameEvent.Moved m
    | any (`Set.member` told view) (Moved.arrivals m) -> Nothing
    -- CR 601.2a: a spell's move to the stack begins its cast, which the
    -- SpellCast entry tells once the cast is complete.
    | to == Zone.Stack -> Nothing
    -- CR 111.1: a token's creation is logged as a move from the battlefield
    -- to it.
    | from == Zone.Battlefield && to == Zone.Battlefield -> said [PromptOffer.snapshotName (Moved.characteristics m), Text.pack "is created"]
    | to == Zone.Graveyard -> said [PromptOffer.snapshotName (Moved.characteristics m), Text.pack "is put into", zoneOf to, Text.pack "from", zoneOf from]
    | otherwise -> said [PromptOffer.snapshotName (Moved.characteristics m), Text.pack "moves from", zoneOf from, Text.pack "to", zoneOf to]
    where
      -- CR 400.3: a library, hand or graveyard a card is in is its owner's.
      owner = Monad.mplus (fmap ObjectSnapshot.owner (Moved.arrived m)) (fmap (ObjectView.owner . snd) (located view (ZoneChange.object (Moved.change m))))
      zoneOf = ownedZone reader owner
      from = ZoneChange.from (Moved.change m)
      to = ZoneChange.to (Moved.change m)
  GameEvent.Discarded d -> said [acts reader (Discarded.player d) "discards", name (Discarded.card d)]
  GameEvent.Milled m -> said [acts reader (Milled.player m) "mills", Text.intercalate (Text.pack ", ") (fmap name (Foldable.toList (Milled.cards m)))]
  GameEvent.Drew d -> said [acts reader (Drew.player d) "draws a card"]
  GameEvent.Revealed r -> said [acts reader (Revealed.player r) "reveals", PromptOffer.snapshotName (Revealed.characteristics r)]
  GameEvent.Looked l -> said [acts reader (Looked.player l) "looks at", PromptOffer.snapshotName (Looked.characteristics l)]
  GameEvent.SpellCountered c -> said [name (Countering.countered c), Text.pack "is countered"]
  GameEvent.MulliganDeclared d -> case MulliganDeclared.decision d of
    MulliganDecision.Keep -> said [acts reader (MulliganDeclared.player d) "keeps"]
    MulliganDecision.Mulligan -> said [acts reader (MulliganDeclared.player d) "mulligans, bottoming", count (MulliganDeclared.bottomCount d) "card"]
  GameEvent.StepBegan {} -> Nothing
  GameEvent.GameBegan -> Nothing
  GameEvent.CardArrived {} -> Nothing
  GameEvent.AttackersDeclared {} -> Nothing
  GameEvent.BecameAttacked {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.BecameTapped {} -> Nothing
  GameEvent.TappedToPay {} -> Nothing
  GameEvent.BecameUntapped {} -> Nothing
  GameEvent.TappedForMana {} -> Nothing
  GameEvent.ManaAdded {} -> Nothing
  GameEvent.ManaAbilityResolved {} -> Nothing
  -- CR 603.3: a triggered ability put on the stack, with what it says; a
  -- sourceless one (TriggerSource.Sourceless) is told by what it does.
  GameEvent.AbilityTriggered t
    | TriggerSource.OfObject source <- AbilityTriggered.source t,
      Just _ <- AbilityTriggered.stackObject t ->
        Just $ case stackedText view (Just source) (StackedAbility.Triggered (AbilityTriggered.ability t)) of
          Just text -> name source <> Text.pack "'s ability triggers: \"" <> text <> Text.pack "\""
          Nothing -> name source <> Text.pack "'s ability triggers."
    | otherwise -> Nothing
  GameEvent.ActivatedAbilityResolved {} -> Nothing
  GameEvent.TriggeredAbilityResolved {} -> Nothing
  GameEvent.TriggeredAbilityFinished _ -> Nothing
  -- CR 603.3d: a triggered ability's targets, chosen as it is put on the
  -- stack, which no other entry tells; a spell's and an activated ability's
  -- are told after its cast or activation (announced).
  GameEvent.BecameTarget b
    | BecameTarget.kind b == StackObjectKind.TriggeredAbility -> said [triggeredBy (BecameTarget.source b), Text.pack "targets", recipient (BecameTarget.targeted b)]
    | otherwise -> Nothing
  GameEvent.PermanentActed {} -> Nothing
  GameEvent.PlayerActed a -> playerActed reader (PlayerActed.player a) (PlayerActed.action a)
  GameEvent.CountedAct d -> case CountedAct.action d of
    PlayerAction.Discover -> said [acts reader (CountedAct.player d) "discovers", Text.pack (show (CountedAct.value d))]
    PlayerAction.ScryToBottom -> said [acts reader (CountedAct.player d) "puts", Text.pack (show (CountedAct.value d)), Text.pack "scried cards on the bottom"]
    action -> playerActed reader (CountedAct.player d) action
  -- CR 702.26b / 702.26n: a phased-out card's arrival in exile, which no seat sees.
  GameEvent.ArrivedUnseen {} -> Nothing
  GameEvent.DamagePrevented p ->
    said
      [ Text.intercalate (Text.pack " and ") [Text.pack (show n <> " damage ") <> name source <> Text.pack " would deal to " <> recipient r | (source, toward) <- Map.toList (DamagePrevented.amounts p), (r, n) <- Map.toList toward],
        Text.pack "is prevented"
      ]
  GameEvent.SpellCopied c -> said [acts reader (SpellWasCopied.player c) "copies", name (SpellWasCopied.copy c)]
  GameEvent.BecameMonarch pid -> said [acts reader pid "becomes the monarch"]
  GameEvent.TookInitiative pid -> said [acts reader pid "takes the initiative"]
  GameEvent.Voted v -> said [ballot (Voted.ballot v)]
  GameEvent.VotesRevealed ballots -> said [Text.pack "The votes are revealed:", listing (fmap ballot (Foldable.toList ballots))]
  GameEvent.ChoseSecretly c -> said [subject reader (SecretChoice.player (ChoseSecretly.choice c)), Text.pack "secretly", conjugated reader (SecretChoice.player (ChoseSecretly.choice c)) "chooses", Maybe.fromMaybe (Text.pack (show (SecretChoice.subtype (ChoseSecretly.choice c)))) (Subtype.creatureTypeWord (SecretChoice.subtype (ChoseSecretly.choice c)))]
  GameEvent.ChoiceRevealed c -> said [acts reader (SecretChoice.player c) "reveals the chosen", Maybe.fromMaybe (Text.pack (show (SecretChoice.subtype c))) (Subtype.creatureTypeWord (SecretChoice.subtype c))]
  GameEvent.AbilityCountered c -> said [name (Countering.countered c), Text.pack "is countered"]
  GameEvent.CountersPut c -> said [acts reader (CountersPlaced.putter c) "puts", counters (CountersPlaced.after c `less` CountersPlaced.before c) (CountersPlaced.kind c), Text.pack "on", name (CountersPlaced.object c)]
  GameEvent.PlayerCountersPut c -> said [acts reader (PlayerCountersPlaced.putter c) "puts", count (PlayerCountersPlaced.after c `less` PlayerCountersPlaced.before c) (Char.toLower <$> show (PlayerCountersPlaced.kind c) <> " counter"), Text.pack "on", if PlayerCountersPlaced.player c == PlayerCountersPlaced.putter c && reader == Just (PlayerCountersPlaced.putter c) then Text.pack "yourself" else objective reader (PlayerCountersPlaced.player c)]
  GameEvent.CountersRemoved c ->
    let n = CounterChange.before c `less` CounterChange.after c
     in said [counters n (CounterChange.kind c), Text.pack (if n == 1 then "is removed from" else "are removed from"), name (CounterChange.object c)]
  GameEvent.HalfUnlocked h -> said [acts reader (HalfUnlocked.actor h) "unlocks", CardName.unwrap (HalfUnlocked.name h)]
  GameEvent.TurnedFaceUp t -> said [name (TurnedFaceUp.object t), Text.pack "is turned face up"]
  GameEvent.Transformed t -> said [name (Transformed.object t), Text.pack "transforms"]
  GameEvent.BecameDesignated d -> said [name (BecameDesignated.object d), designation (BecameDesignated.designation d)]
  GameEvent.Mentored m -> said [name (Mentored.mentor m), Text.pack "mentors", name (Mentored.mentored m)]
  GameEvent.Exploited e -> said [name (Exploited.exploiter e), Text.pack "exploits", name (Exploited.exploited e)]
  GameEvent.Convoked c -> by (Convoking.convokedBy c) "convokes" "convoke" (Convoking.spell c)
  GameEvent.Crewed c -> by (Crewing.crewedBy c) "crews" "crew" (Crewing.vehicle c)
  GameEvent.BecameCrewed c -> said [name (Crewing.vehicle c), Text.pack "becomes crewed"]
  GameEvent.Saddled c -> by (Saddling.saddledBy c) "saddles" "saddle" (Saddling.mount c)
  GameEvent.Enlisted e -> said [name (Enlisted.enlister e), Text.pack "enlists", name (Enlisted.enlisted e)]
  GameEvent.PermanentSacrificed p -> said [acts reader (PermanentWasSacrificed.player p) "sacrifices", name (PermanentWasSacrificed.permanent p)]
  GameEvent.ControlChanged c -> said [acts reader (ControlChanged.after c) "gains control of", name (ControlChanged.object c)]
  GameEvent.VentureMarkerEntered v -> said [acts reader (VentureMarkerEntered.player v) "moves", their reader (VentureMarkerEntered.player v), Text.pack "venture marker in", name (VentureMarkerEntered.dungeon v)]
  GameEvent.BecameAttached a -> said [name (BecameAttached.attachment a), Text.pack "becomes attached to", recipient (BecameAttached.host a)]
  GameEvent.BecameUnattached a -> said [name (BecameUnattached.attachment a), Text.pack "becomes unattached from", recipient (BecameUnattached.host a)]
  GameEvent.LeftTheGame l -> said [name (LeftTheGame.object l), Text.pack "leaves the game"]
  GameEvent.LostTheGame pid -> said [acts reader pid "loses the game"]
  GameEvent.ManifestedDread d -> said [acts reader (ManifestedDread.player d) "manifests dread"]
  GameEvent.FacedDilemma d -> said [acts reader (FacedDilemma.player d) "faces a dilemma over", name (FacedDilemma.object d)]
  GameEvent.CommittedCrime c -> said [acts reader (CommittedCrime.player c) "commits a crime with", name (CommittedCrime.object c)]
  GameEvent.Expended e -> said [acts reader (Expended.player e) "has spent", Text.pack (show (Expended.after e)), Text.pack "mana on spells this turn"]
  -- A roll is told by its result; its natural results beside it are not.
  GameEvent.DieResultSettled r -> case DieResult.reading r of
    RollReading.FinalResult -> said [acts reader (DieResult.roller r) "rolls", Text.pack (show (DieResult.result r))]
    RollReading.NaturalResult -> Nothing
  GameEvent.RolledToVisit r -> said [acts reader (DieResult.roller r) "rolls", Text.pack (show (DieResult.result r)), Text.pack "to visit", their reader (DieResult.roller r), Text.pack "Attractions"]
  GameEvent.PlanarDieRolled r -> said [acts reader (PlanarDieRolled.roller r) "rolls", planarFace (PlanarDieRolled.face r), Text.pack "on the planar die"]
  GameEvent.TurnedUpOffDeck t -> said [acts reader (TurnedUpOffDeck.player t) "turns up", name (TurnedUpOffDeck.card t), Text.pack "from", their reader (TurnedUpOffDeck.player t), sideDeck (TurnedUpOffDeck.deck t)]
  GameEvent.ChaosEnsued _ -> said [Text.pack "Chaos ensues"]
  GameEvent.DaytimeChanged Daytime.Day -> said [Text.pack "Night becomes day"]
  GameEvent.DaytimeChanged Daytime.Night -> said [Text.pack "Day becomes night"]
  GameEvent.ClassLevelSet c -> said [name (ClassLevelChange.object c), Text.pack "becomes level", Text.pack (show (ClassLevel.unwrap (ClassLevelChange.after c)))]
  GameEvent.Plotted oid -> said [name oid, Text.pack "becomes plotted"]
  GameEvent.CoinFlipped c ->
    said
      ( [acts reader (CoinFlipped.flipper c) "flips a coin"]
          <> foldMap (\won -> [Text.pack "and", conjugated reader (CoinFlipped.flipper c) (if won then "wins the flip" else "loses the flip")]) (CoinFlipped.won c)
      )
  GameEvent.StickerPut p -> said [acts reader (StickerPut.placer p) "puts", sticker (StickerPut.kind p), Text.pack "on", name (StickerPut.object p)]
  where
    name = named view
    -- A triggered ability on the stack by its source, which outlasts it.
    triggeredBy oid = case [source | GameEvent.AbilityTriggered t <- fmap LoggedEvent.event (witnessed view), AbilityTriggered.stackObject t == Just oid, TriggerSource.OfObject source <- [AbilityTriggered.source t]] of
      source : _ -> name source <> Text.pack "'s ability"
      [] -> name oid
    said parts = Just (Text.unwords parts <> Text.pack ".")
    less a b = if a > b then a - b else 0
    by actors one many object =
      let names = fmap name (Set.toList actors)
       in said [listing names, Text.pack (if length names == 1 then one else many), name object]
    ballot b =
      acts reader (Ballot.voter b) "votes for" <> Text.pack " " <> case Ballot.choice b of
        BallotChoice.ForObject o -> PromptOffer.snapshotName (ObjectSnapshot.characteristics o)
        BallotChoice.ForWord w -> SlotName.unwrap w
        BallotChoice.ForPlayer pid
          | pid == Ballot.voter b && reader == Just pid -> Text.pack "yourself"
          | otherwise -> objective reader pid
    attacked target = case target of
      AttackTarget.OfPlayer pid -> objective reader pid
      AttackTarget.OfPlaneswalker oid -> name oid
      AttackTarget.OfBattle oid -> name oid
    recipient = recipientTo reader view

-- | A player or object a damage event, an attachment or a target names, to
-- a reader, as objective says.
recipientTo :: Maybe PlayerId.PlayerId -> PlayerView.PlayerView -> Recipient.Recipient -> Text.Text
recipientTo reader view r = case r of
  Recipient.ToPlayer pid -> objective reader pid
  _ -> recipientName view r

-- | Whether an activation is of a mana ability (CR 605.1a).
isManaAbility :: AbilityWasActivated.AbilityWasActivated -> Bool
isManaAbility a = AbilityFacts.kind (AbilityWasActivated.facts a) == AbilityKind.ManaAbility

-- | A player or object a damage event, an attachment or a target names.
recipientName :: PlayerView.PlayerView -> Recipient.Recipient -> Text.Text
recipientName view r = case r of
  Recipient.ToPlayer pid -> player pid
  Recipient.ToCreature oid -> named view oid
  Recipient.ToPlaneswalker oid -> named view oid
  Recipient.ToBattle oid -> named view oid
  Recipient.ToObject oid -> named view oid
  Recipient.ToPile _ -> Text.pack "a pile"

-- | A player action as a sentence, or nothing for a die roll (CR 706.1), whose
-- DieResultSettled or PlanarDieRolled entries tell it.
playerActed :: Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> PlayerAction.PlayerAction -> Maybe Text.Text
playerActed reader pid action = case action of
  PlayerAction.Scry -> does "scries"
  PlayerAction.Surveil -> does "surveils"
  PlayerAction.Proliferate -> does "proliferates"
  PlayerAction.RollDice -> Nothing
  PlayerAction.OpenAttraction -> does "opens an Attraction"
  PlayerAction.ClaimPrize -> does "claims the prize"
  PlayerAction.TemptedByRing -> does "is tempted by the Ring"
  PlayerAction.Blight -> does "blights"
  PlayerAction.Forage -> does "forages"
  PlayerAction.Foretell -> does "foretells a card"
  PlayerAction.CollectEvidence -> does "collects evidence"
  PlayerAction.GiveGift -> does "gives a gift"
  PlayerAction.Earthbend -> does "earthbends"
  PlayerAction.Waterbend -> does "waterbends"
  PlayerAction.Airbend -> does "airbends"
  PlayerAction.Firebend -> does "firebends"
  PlayerAction.CompleteDungeon -> does "completes a dungeon"
  -- Logged under the library's owner, who need not be the shuffler.
  PlayerAction.Shuffle -> Just (if reader == Just pid then Text.pack "Your library is shuffled." else player pid <> Text.pack "'s library is shuffled.")
  PlayerAction.SearchLibrary -> Just (acts reader pid "searches" <> Text.pack " " <> their reader pid <> Text.pack " library.")
  PlayerAction.Discover -> does "discovers"
  PlayerAction.ScryToBottom -> does "puts scried cards on the bottom"
  where
    does predicate = Just (acts reader pid predicate <> Text.pack ".")

-- | The objects whose arrival another witnessed entry already tells: a spell
-- cast, a land played, a card discarded or milled. Only the move that made
-- one is told; a later move of it is not.
told :: PlayerView.PlayerView -> Set.Set ObjectId.ObjectId
told view =
  Set.fromList . concatMap (telling . LoggedEvent.event) $ witnessed view
  where
    telling e = case e of
      GameEvent.SpellCast c -> [SpellWasCast.spell c]
      GameEvent.LandPlayed l -> [LandPlayed.land l]
      GameEvent.Discarded d -> [Discarded.card d]
      GameEvent.Milled m -> Foldable.toList (Milled.cards m)
      _ -> []

-- | "1 +1/+1 counter", "2 stun counters".
counters :: Natural -> CounterKind.CounterKind Keyword.Keyword -> Text.Text
counters n kind = Text.pack (show n) <> Text.pack " " <> counterName kind <> Text.pack (if n == 1 then " counter" else " counters")

-- | Names joined as a list: "A", "A and B", "A, B and C".
listing :: [Text.Text] -> Text.Text
listing names = case reverse names of
  [] -> Text.empty
  [only] -> only
  final : rest -> Text.intercalate (Text.pack ", ") (reverse rest) <> Text.pack " and " <> final

-- | What gaining a designation does to a permanent, as a predicate.
designation :: Designation.Designation -> Text.Text
designation d = Text.pack $ case d of
  Designation.Renowned -> "becomes renowned"
  Designation.Monstrous -> "becomes monstrous"
  Designation.Suspected -> "becomes suspected"
  Designation.Solved -> "becomes solved"
  Designation.Saddled -> "becomes saddled"
  Designation.Prepared -> "becomes prepared"
  Designation.Harnessed -> "becomes harnessed"
  Designation.Embalmed -> "becomes embalmed"
  Designation.AlphaSector -> "is assigned to the alpha sector"
  Designation.BetaSector -> "is assigned to the beta sector"
  Designation.GammaSector -> "is assigned to the gamma sector"
  Designation.Sprocket1 -> "is assigned to sprocket 1"
  Designation.Sprocket2 -> "is assigned to sprocket 2"
  Designation.Sprocket3 -> "is assigned to sprocket 3"

planarFace :: PlanarDieFace.PlanarDieFace -> Text.Text
planarFace f = Text.pack $ case f of
  PlanarDieFace.Blank -> "a blank"
  -- CR 107.12
  PlanarDieFace.Chaos -> "{CHAOS}"
  -- CR 107.11
  PlanarDieFace.Planeswalker -> "{PW}"

sideDeck :: SideDeck.SideDeck -> Text.Text
sideDeck d = Text.pack $ case d of
  SideDeck.Attraction -> "Attraction deck"
  SideDeck.Contraption -> "Contraption deck"
  SideDeck.Planar -> "planar deck"
  SideDeck.Scheme -> "scheme deck"

sticker :: StickerKind.StickerKind -> Text.Text
sticker k = Text.pack $ case k of
  StickerKind.Name -> "a name sticker"
  StickerKind.Ability -> "an ability sticker"
  StickerKind.PowerToughness -> "a power and toughness sticker"
  StickerKind.Art -> "an art sticker"

-- | How the game came out, to the sentence's reader.
result :: Maybe PlayerId.PlayerId -> Result.Result -> Text.Text
result reader r = case r of
  Result.Won pid -> acts reader pid "wins" <> Text.pack "."
  Result.TeamWon t -> team t <> Text.pack " wins."
  Result.Drawn -> Text.pack "The game is a draw."

-- | How each player who left the game left, to the sentence's reader, in
-- seating order: by conceding (CR 104.3a), a draw (CR 104.4), or losing, to
-- each state-based action they meet (Sba.lossCauses) or, meeting none, to an
-- effect (CR 104.3e).
departures :: Maybe PlayerId.PlayerId -> GameState.GameState -> [Text.Text]
departures reader gs =
  [ subject reader pid <> how pid departure p
  | pid <- GameState.seating gs,
    Just p <- [Map.lookup pid (GameState.players gs)],
    Status.Departed departure <- [Player.status p]
  ]
  where
    how pid departure p = case departure of
      Departure.Conceded -> Text.pack " conceded."
      Departure.Drew -> Text.pack " drew the game."
      Departure.Lost -> case Sba.lossCauses gs pid of
        [] -> Text.pack " lost the game to an effect."
        causes -> Text.pack " lost the game: " <> Text.intercalate (Text.pack " and ") (fmap (cause p) causes) <> Text.pack "."
    cause p c = case c of
      LossCause.NoLife -> Text.pack ("life total " <> show (Player.life p))
      LossCause.EmptyLibrary -> Text.pack "drew from an empty library"
      LossCause.Poison -> count (Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters p)) "poison counter"
      LossCause.CommanderDamage -> Text.pack "21 or more combat damage from one commander"

phase :: Phase.Phase -> Text.Text
phase p = Text.pack $ case p of
  Phase.Beginning step -> case step of
    BeginningStep.Untap -> "untap step"
    BeginningStep.Upkeep -> "upkeep"
    BeginningStep.DrawStep -> "draw step"
  Phase.PrecombatMain -> "precombat main phase"
  Phase.Combat step -> case step of
    CombatStep.BeginningOfCombat -> "beginning of combat"
    CombatStep.DeclareAttackers -> "declare attackers step"
    CombatStep.DeclareBlockers -> "declare blockers step"
    CombatStep.CombatDamage -> "combat damage step"
    CombatStep.EndOfCombat -> "end of combat"
  Phase.PostcombatMain -> "postcombat main phase"
  Phase.Ending step -> case step of
    EndingStep.EndStep -> "end step"
    EndingStep.Cleanup -> "cleanup step"

count :: Natural -> String -> Text.Text
count n noun = Text.pack (show n <> " " <> noun <> if n == 1 then "" else "s")

-- | "SpellCast" as "spell cast".
spaced :: Text.Text -> Text.Text
spaced = Text.strip . Text.toLower . Text.concatMap (\c -> if Char.isUpper c then Text.pack [' ', c] else Text.singleton c)
