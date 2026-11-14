-- Answering one question: does this deck obey its format's construction rules,
-- and if not, every way it does not?
--
-- Validation before a game, never part of one: Parun.Engine.Setup.newGame
-- takes any deck, and a caller that wants a legal one asks here first. Beside
-- Parun.DeckList because a deck list is the other half of what a player hands
-- over.
module Parun.Legality where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Numeric.Natural as Natural
import qualified Parun.Codec.Format as Format.Codec
import qualified Parun.Codec.Legality as Legality.Codec
import qualified Parun.Codec.LegalityTable as LegalityTable.Codec
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.Ante as Ante
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Commander as Commander
import qualified Parun.Engine.Filter as Filter
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PregameExile as PregameExile.Engine
import qualified Parun.Engine.Projection.View as Projection
import qualified Parun.Engine.RuleFamily as RuleFamily.Engine
import qualified Parun.Engine.Subtype as Subtype.Engine
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Ingest as Ingest
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.DeckLimit as DeckLimit
import qualified Parun.Types.DraftBooster as DraftBooster
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Legality as Legality
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.PregameExile as PregameExile
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.StickerSheet as StickerSheet
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.Teams as Teams
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.Violation as Violation
import qualified Paths_parun as Paths

-- | Every violation of the format's rules and pool, for a game played with the
-- settings given, from every rule: none stops at the first failure. A limited
-- format's deck is checked against the card pool given (see 'fromPool'), any
-- other format's ignores it.
check :: LegalityTable.LegalityTable -> GameSettings.GameSettings -> Format.Format -> Maybe CardPool -> Deck.Deck -> [Violation.Violation]
check table settings format cardPool deck = concatMap ($ deck) (rules table settings format cardPool)

-- | CR 100.2b / 100.4b: a limited player's card pool, how many cards of each
-- English name it holds.
type CardPool = Map.Map CardName.CardName Natural.Natural

-- | The card pool a deck list holds, by name, counting the sections 'fromPool'
-- counts: a pool written as a list.
poolOf :: Deck.Deck -> CardPool
poolOf deck = Map.fromListWith (+) [(Face.name (Card.combined (Printing.card printing)), count) | (printing, count) <- Map.toList (fromPoolCards deck)]

-- | The construction rules a format imposes, each a predicate over a deck.
rules :: LegalityTable.LegalityTable -> GameSettings.GameSettings -> Format.Format -> Maybe CardPool -> [Deck.Deck -> [Violation.Violation]]
rules table settings format cardPool =
  let standing = fmap (poolStanding table) (Format.pool format)
      restricted card = any (\standingOf -> standingOf card == Just (Just Legality.Restricted)) standing
      -- CR 903.5a / 903.12d: exactly so many cards, commanders included. CR
      -- 903.5b: one of each name but a basic land, unless a card's own
      -- deckLimit says otherwise (CR 101.1). The format, and not the game's
      -- settings, says which of CR 903.3 and CR 903.12c judges the commander,
      -- and whether CR 903.13f(3) gives a card partner.
      --
      -- CR 903.13f: a Commander Draft deck is Commander's with no maximum and
      -- no copy limit but a card's own or its card pool's (CR 903.13f(2)), and
      -- is limited play (CR 903.13a), so CR 315.1's and CR 123.2a's
      -- constructed rules stay out.
      commander family =
        let brawl = family == RuleFamily.Brawl
            draft = family == RuleFamily.CommanderDraft
            size = RuleFamily.Engine.minimumDeckSize family
         in [atLeast Nothing size withCommander]
              <> [commanderAtMost size | not draft]
              <> [copiesAtMost Nothing restricted (if draft then Nothing else Just 1) withCommander, noSideboard, commanders settings {GameSettings.brawl = brawl, GameSettings.format = Just format}, inIdentity brawl]
              <> foldMap (pure . inPool) standing
              <> [\deck -> fromPool (addedCommanders (Format.boosters format) held deck) deck | draft, Just held <- [cardPool]]
              <> [anteCards settings]
              <> (if draft then [] else [conspiracies, stickerSheets])
              <> [circledColors, supplementary family settings, vanguardCard, exiledBeforeGame (RuleFamily.Engine.drafted family)]
   in case Format.rules format of
        -- CR 100.2a / 100.4a.
        RuleFamily.Constructed -> [atLeast Nothing (RuleFamily.Engine.minimumDeckSize RuleFamily.Constructed) Deck.cards, copiesAtMost Nothing restricted (Just 4) bothSections, sideboardAtMost 15] <> foldMap (pure . inPool) standing <> [anteCards settings, conspiracies, stickerSheets, circledColors, supplementary RuleFamily.Constructed settings, vanguardCard, exiledBeforeGame (RuleFamily.Engine.drafted RuleFamily.Constructed)]
        -- CR 100.2b: no copy limit beyond the product's own, but a card's own
        -- "up to N" still caps the deck: Nazgûl's ruling of 2023-06-16 keeps a
        -- tenth in the sideboard.
        RuleFamily.Limited -> [atLeast Nothing (RuleFamily.Engine.minimumDeckSize RuleFamily.Limited) Deck.cards, copiesAtMost Nothing restricted Nothing Deck.cards] <> foldMap (pure . inPool) standing <> foldMap (pure . fromPool) cardPool <> [anteCards settings, circledColors, supplementary RuleFamily.Limited settings, vanguardCard, exiledBeforeGame (RuleFamily.Engine.drafted RuleFamily.Limited)]
        -- CR 903.5.
        RuleFamily.Commander -> commander RuleFamily.Commander
        -- CR 903.12a: Commander's rules, with CR 903.12c's commander, CR
        -- 903.12d's size and CR 903.12e's basic lands.
        RuleFamily.Brawl -> commander RuleFamily.Brawl
        -- CR 903.13f.
        RuleFamily.CommanderDraft -> commander RuleFamily.CommanderDraft

-- Deck and sideboard together: CR 100.4a counts the sideboard, and a card
-- barred from the deck is barred from both (CR 407.3).
bothSections :: Deck.Deck -> Map.Map Printing.Printing Natural.Natural
bothSections = Map.unionWith (+) <$> Deck.cards <*> Deck.sideboard

-- CR 100.2a / 100.2b: a minimum deck size, counting the cards `counted`
-- picks: the deck and not the sideboard (CR 100.4), and its commanders where
-- CR 903.5a counts them, or the supplementary deck `pile` names.
atLeast :: Maybe SideDeck.SideDeck -> Natural.Natural -> (Deck.Deck -> Map.Map Printing.Printing Natural.Natural) -> Deck.Deck -> [Violation.Violation]
atLeast pile least counted deck =
  let size = sum (counted deck)
   in [Violation.TooFewCards pile size least | size < least]

-- CR 903.5a / 903.12d: a maximum deck size, counting as `atLeast` does.
atMost :: Natural.Natural -> (Deck.Deck -> Map.Map Printing.Printing Natural.Natural) -> Deck.Deck -> [Violation.Violation]
atMost most counted deck =
  let size = sum (counted deck)
   in [Violation.TooManyCards size most | size > most]

-- CR 903.5a / 903.12d: atMost, unless a card the deck designates as its
-- commander lifts the maximum (CR 903.5e), legal commander or not, as
-- inIdentity reads them. Read off the front face, as
-- Parun.Engine.Commander.soleCommander reads CR 903.3a's ability.
commanderAtMost :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
commanderAtMost most deck
  | any (Set.member CommanderDeckRule.NoMaximumDeckSize . Face.commanderDeckRules . Card.frontFace . Printing.card) (Map.keys (Deck.commander deck)) = []
  | otherwise = atMost most withCommander deck

-- CR 100.2d: each supplementary deck brought holds only its kind of card, as
-- many as its rule asks, of as few names. A Commander or Brawl deck is built
-- ahead of time, so in constructed play (CR 100.2a), and a Commander Draft
-- deck in limited play (CR 903.13a); and a Brawl or Commander Draft game is a
-- Commander game (CR 903.12a, 903.13g), so CR 904.13a's option covers it.
supplementary :: RuleFamily.RuleFamily -> GameSettings.GameSettings -> Deck.Deck -> [Violation.Violation]
supplementary family settings deck = foldMap judge (Map.toList (Map.filter (not . Map.null) (Deck.sideDecks deck)))
  where
    limited = family == RuleFamily.Limited || family == RuleFamily.CommanderDraft
    commanderGame = family == RuleFamily.Commander || family == RuleFamily.Brawl || family == RuleFamily.CommanderDraft
    judge (pile, held) =
      let sized least limit = atLeast (Just pile) least (const held) deck <> copiesAtMost (Just pile) (const False) limit (const held) deck
          (belongs, rule) = case pile of
            -- CR 718.2b / 718.2a.
            SideDeck.Attraction -> (\card -> Game.sideDeckBackOf card == Just SideDeck.Attraction, if limited then sized 3 Nothing else sized 10 (Just 1))
            -- The Unstable FAQ's rulings of 2018-01-19 (Applied Aeronautics):
            -- in limited play any number, duplicates allowed; in constructed
            -- at least fifteen, no two of a name.
            SideDeck.Contraption -> (\card -> Game.sideDeckBackOf card == Just SideDeck.Contraption, if limited then [] else sized 15 (Just 1))
            -- CR 901.3. Under CR 901.15a's communal deck every seat's cards
            -- are one deck, so one seat's share is not sized alone: checkGame
            -- sizes the whole.
            SideDeck.Planar -> (\card -> hasType CardType.Plane card || hasType CardType.Phenomenon card, if GameSettings.singlePlanarDeck settings then [] else planarDeck 1 held)
            -- CR 904.13d / 904.3.
            SideDeck.Scheme -> (hasType CardType.Scheme, if commanderGame then sized 10 (Just 1) else sized 20 (Just 2))
       in [Violation.MisplacedCard pile (Face.name (Card.combined card)) | card <- Set.toList (Set.fromList (fmap Printing.card (Map.keys held))), not (belongs card)] <> rule

-- CR 902.3: the vanguard a deck brings is a vanguard card. Deck.vanguard holds
-- one, which is CR 902.1's limit.
vanguardCard :: Deck.Deck -> [Violation.Violation]
vanguardCard deck = [Violation.NotVanguardCard (Face.name face) | printing <- Maybe.maybeToList (Deck.vanguard deck), let face = Card.combined (Printing.card printing), not (Set.member CardType.Vanguard (TypeLine.types (Face.typeLine face)))]

-- | Every violation of the construction rules a game states of every
-- player's deck together, one deck a seat: CR 901.15a's communal planar deck
-- under the single planar deck option, and CR 902.3's vanguard card for each
-- player once any brings one. Parun.Legality.check judges each deck alone.
checkGame :: GameSettings.GameSettings -> [Deck.Deck] -> [Violation.Violation]
checkGame settings decks = communal <> vanguards
  where
    players = Natural.length decks
    pooled = Map.unionsWith (+) [Map.findWithDefault Map.empty SideDeck.Planar (Deck.sideDecks deck) | deck <- decks]
    communal = if GameSettings.singlePlanarDeck settings && not (Map.null pooled) then planarDeck players pooled else []
    missing = Natural.length (filter (Maybe.isNothing . Deck.vanguard) decks)
    vanguards = [Violation.MissingVanguard missing players | missing > 0, missing < players]

-- CR 901.3 / 901.15a: a planar deck's size and names, and its phenomena, for
-- a deck shared by this many players: one for a player's own.
planarDeck :: Natural.Natural -> Map.Map Printing.Printing Natural.Natural -> [Violation.Violation]
planarDeck players held =
  let phenomena = sum (Map.filterWithKey (\printing _ -> hasType CardType.Phenomenon (Printing.card printing)) held)
      most = 2 * players
      whole = Deck.fromCards held
   in atLeast (Just SideDeck.Planar) (min 40 (10 * players)) Deck.cards whole
        <> copiesAtMost (Just SideDeck.Planar) (const False) (Just 1) Deck.cards whole
        <> [Violation.TooManyPhenomena phenomena most | phenomena > most]

-- Whether a card's combined type line has the card type.
hasType :: CardType.CardType -> Card.Type.Card -> Bool
hasType cardType card = Set.member cardType (TypeLine.types (Face.typeLine (Card.combined card)))

-- Deck, sideboard and commanders together.
everySection :: Deck.Deck -> Map.Map Printing.Printing Natural.Natural
everySection deck = Map.unionWith (+) (bothSections deck) (Deck.commander deck)

-- CR 100.2b / 100.4b: the cards a limited deck draws from its card pool: deck,
-- sideboard and commanders (CR 903.13f(2)), the conspiracies taken from the
-- sideboard (CR 315.2), the cards exiled before the game (CR 607.2n), and the
-- Attraction (CR 718.2b) and Contraption (the Unstable FAQ) decks.
fromPoolCards :: Deck.Deck -> Map.Map Printing.Printing Natural.Natural
fromPoolCards deck = Map.unionsWith (+) ([everySection deck, Deck.conspiracies deck] <> Map.elems (Deck.exiledBeforeGame deck) <> [Map.findWithDefault Map.empty pile (Deck.sideDecks deck) | pile <- [SideDeck.Attraction, SideDeck.Contraption]])

-- CR 100.2b: each card 'fromPoolCards' counts but a basic land card is one
-- the card pool holds, and no more of a name than the pool holds. Names are
-- compared as copiesAtMost compares them.
fromPool :: CardPool -> Deck.Deck -> [Violation.Violation]
fromPool held deck =
  let tallies = Map.fromListWith (+) [(Face.name (Card.combined card), count) | (printing, count) <- Map.toList (fromPoolCards deck), let card = Printing.card printing, not (basicLand card)]
      judge (written, count) = case Map.lookup written held of
        Nothing -> [Violation.NotInCardPool written]
        Just most -> [Violation.TooManyCopies Nothing written count most | count > most]
   in foldMap judge (Map.toList tallies)

-- CR 903.13e: a Commander Draft card pool, with the cards named The Prismatic
-- Piper or Faceless One its boosters let the deck add, up to two of each,
-- counting only those the deck designates as its commanders.
addedCommanders :: Set.Set DraftBooster.DraftBooster -> CardPool -> Deck.Deck -> CardPool
addedCommanders boosters held deck =
  let designated = poolOf (Deck.fromCards (Deck.commander deck))
      added name from = [(written, min 2 count) | any (`Set.member` boosters) from, let written = CardName.MkCardName (Text.pack name), Just count <- [Map.lookup written designated]]
   in Map.unionWith (+) held (Map.fromList (added "The Prismatic Piper" [DraftBooster.CommanderLegends, DraftBooster.CommanderMasters] <> added "Faceless One" [DraftBooster.BattleForBaldursGate]))

-- CR 903.5a: the deck with its commanders, which it counts among its cards.
withCommander :: Deck.Deck -> Map.Map Printing.Printing Natural.Natural
withCommander deck = Map.unionWith (+) (Deck.cards deck) (Deck.commander deck)

-- CR 100.2a / 100.4a: at most so many cards of one English name, among the
-- cards `counted` picks: one for a card the pool restricts, else the card's
-- own deckLimit where it prints one, else the format's limit, and none for a
-- basic land card; `pile` names the supplementary deck counted, if one is.
-- Names are compared as Parun.Engine.Card.combined has them;
-- no card in the pool has an interchangeable name (CR 201.3b), so equal names
-- are the whole of it.
copiesAtMost :: Maybe SideDeck.SideDeck -> (Card.Type.Card -> Bool) -> Maybe Natural.Natural -> (Deck.Deck -> Map.Map Printing.Printing Natural.Natural) -> Deck.Deck -> [Violation.Violation]
copiesAtMost pile restricted limit counted deck =
  let tallies = Map.fromListWith (\(_, new) (card, old) -> (card, old + new)) [(Face.name (Card.combined card), (card, count)) | (printing, count) <- Map.toList (counted deck), let card = Printing.card printing, not (basicLand card)]
   in [Violation.TooManyCopies pile written count most | (written, (card, count)) <- Map.toList tallies, Just most <- [limitOf card], count > most]
  where
    limitOf card
      | restricted card = Just 1
      | otherwise = case Face.deckLimit (Card.combined card) of
          Nothing -> limit
          Just (DeckLimit.UpTo most) -> Just most
          Just DeckLimit.AnyNumber -> Nothing

-- CR 100.4a: a maximum sideboard size.
sideboardAtMost :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
sideboardAtMost most deck =
  let size = sum (Deck.sideboard deck)
   in [Violation.SideboardTooLarge size most | size > most]

-- CR 903.5f: no sideboard, but for one card with companion, which CR
-- 702.139a reveals from outside the game and Parun.DeckList's Companion
-- section brings in the sideboard.
noSideboard :: Deck.Deck -> [Violation.Violation]
noSideboard deck =
  let companion = any (any isCompanion . Face.keywordSet . Card.combined . Printing.card) (Map.keys (Deck.sideboard deck))
      isCompanion keyword = case keyword of
        Keyword.Companion _ -> True
        _ -> False
   in sideboardAtMost (if companion then 1 else 0) deck

-- CR 903.3 / 903.12c / 702.124: a commander, designated as
-- Parun.Engine.Commander.designations allows; each card of a refused
-- designation is reported.
commanders :: GameSettings.GameSettings -> Deck.Deck -> [Violation.Violation]
commanders settings deck
  | Map.null (Deck.commander deck) = [Violation.NoCommander]
  | Map.null (Commander.designations settings deck) = [Violation.IllegalCommander (Face.name (Card.combined (Printing.card printing))) | printing <- Map.keys (Deck.commander deck)]
  | otherwise = []

-- CR 903.5c / 903.5d: each card of the deck inside the combined colour
-- identity of the cards it designates (CR 702.124c), legal commanders or not.
-- Nothing without one, which CR 903.4f leaves undefined. Deck.cards alone: CR
-- 903.5c asks it of a card in the deck, which a companion is not.
--
-- CR 903.5e: a card the deck designates as its commander exempts the cards its
-- AnyColorIdentity filters match, read off its front face as commanderAtMost
-- reads it, each candidate judged by its printed characteristics with nothing
-- in play (Parun.Engine.Companion.fulfilled's posture).
--
-- CR 903.5e: a ChosenExtraColor filter admits the cards it matches whose
-- identity the commander's plus one colour covers, the same colour for every
-- such card. A deck list states no colour, so for each filter the colour
-- admitting the most of its cards stands in, the first in Color's order on a
-- tie: the deck is legal under some choice exactly when it is under that one.
--
-- CR 903.12e: under a Brawl commander with no colour, basic land cards of one
-- basic land type are exempt. A deck list states no type, so the type the most
-- basic land cards have stands in, the first in Subtype's order on a tie:
-- every basic land card has at most one basic land type, so the deck is legal
-- under some choice exactly when it is under that one.
inIdentity :: Bool -> Deck.Deck -> [Violation.Violation]
inIdentity brawl deck
  | Map.null (Deck.commander deck) = []
  | otherwise =
      let identity = foldMap Commander.colorIdentity (Map.keys (Deck.commander deck))
          basics = Map.fromListWith (+) [(subtype, count) | (printing, count) <- Map.toList (Deck.cards deck), basicLand (Printing.card printing), subtype <- Set.toList (landTypes printing)]
          chosen = if brawl && Set.null identity then fmap fst (Maybe.listToMaybe (List.sortOn (Ord.Down . snd) (Map.toList basics))) else Nothing
          exempt printing = basicLand (Printing.card printing) && any (`Set.member` landTypes printing) chosen
          widened = [matching | printing <- Map.keys (Deck.commander deck), CommanderDeckRule.AnyColorIdentity matching <- Set.toList (Face.commanderDeckRules (Card.frontFace (Printing.card printing)))]
          context = Filter.contextFor Teams.none Nothing Nothing
          matches printing = Filter.matches context (Projection.viewOfCard (Card.combined (Printing.card printing)))
          admitted printing = any (matches printing) widened
          within extra printing = Set.isSubsetOf (deckColors printing) (foldMap Set.singleton extra <> identity)
          candidates = [printing | printing <- Map.keys (Deck.cards deck), not (exempt printing), not (admitted printing), not (within Nothing printing)]
          admits matching color printing = matches printing matching && within (Just color) printing
          choose matching = admits matching (List.maximumBy (Ord.comparing (\color -> (length (filter (admits matching color) candidates), Ord.Down color))) [minBound .. maxBound])
          extraColors = [choose matching | printing <- Map.keys (Deck.commander deck), CommanderDeckRule.ChosenExtraColor matching <- Set.toList (Face.commanderDeckRules (Card.frontFace (Printing.card printing)))]
          extended printing = any ($ printing) extraColors
          outside printing = not (exempt printing) && not (admitted printing) && not (within Nothing printing) && not (extended printing)
          byName = Map.fromList [(Face.name (Card.combined (Printing.card printing)), printing) | printing <- Map.keys (Deck.cards deck)]
       in [Violation.OutsideColorIdentity written | (written, printing) <- Map.toList byName, outside printing]

-- CR 903.5c / 903.5d: a card's colour identity, and the colours of mana its
-- basic land types make (CR 305.6), which a basic land's reminder text keeps
-- out of its identity (CR 903.4c).
deckColors :: Printing.Printing -> Set.Set Color.Color
deckColors printing = Commander.colorIdentity printing <> Set.fromList [color | Just (ManaType.Colored color) <- fmap Subtype.Engine.subtypeMana (Set.toList (landTypes printing))]

-- CR 305.6: the basic land types on any of a card's faces.
landTypes :: Printing.Printing -> Set.Set Subtype.Subtype
landTypes printing = Set.filter (Maybe.isJust . Subtype.Engine.subtypeMana) (foldMap (TypeLine.subtypes . Face.typeLine) (Card.Type.faces (Printing.card printing)))

-- A card's standing in one pool: Nothing for a card the table does not have,
-- and Just Nothing for one the pool does not have, which is not legal there.
poolStanding :: LegalityTable.LegalityTable -> Text.Text -> Card.Type.Card -> Maybe (Maybe Legality.Legality)
poolStanding table pool card = fmap (Map.lookup pool) (Map.lookup (Ingest.cardKey card) (LegalityTable.cards table))

-- Each card of deck and sideboard the pool bans, does not have, or the table
-- cannot say of. A restricted card is copiesAtMost's.
inPool :: (Card.Type.Card -> Maybe (Maybe Legality.Legality)) -> Deck.Deck -> [Violation.Violation]
inPool standing = foldMap judge . cardsIn
  where
    judge (written, card) = case standing card of
      Nothing -> [Violation.NoLegalityData written]
      Just Nothing -> [Violation.NotLegalCard written]
      Just (Just Legality.Banned) -> [Violation.BannedCard written]
      Just (Just Legality.Restricted) -> []
      Just (Just Legality.Legal) -> []

-- CR 607.2n: the cards exiled before the game with cards of each name are
-- ones an ability of that name exiles, and as many as some number of the
-- deck's cards of that name, each revealed, exile between them (Arcane
-- Savant's and Volatile Chimera's rulings of 2016-08-23). Whether a card was
-- drafted is the deck's own statement in limited play and false in
-- constructed (docs/decisions.md, "Which cards a player drafted that aren't in
-- their deck").
exiledBeforeGame :: Bool -> Deck.Deck -> [Violation.Violation]
exiledBeforeGame drafted deck = foldMap judge (Map.toList (Deck.exiledBeforeGame deck))
  where
    context = Filter.contextFor Teams.none Nothing Nothing
    judge (written, exiled) =
      let revealable = [(exile, count) | (printing, count) <- Map.toList (Deck.cards deck), let face = Card.combined (Printing.card printing), Face.name face == written, Just exile <- [Face.pregameExile face]]
          -- How many of them the deck could reveal; two printings of one name
          -- print one ability, so which one is read does not matter.
          copies = sum (fmap snd revealable)
          exiling = fmap fst (Maybe.listToMaybe revealable)
          total = sum exiled
          allowed exile = drafted && PregameExile.Engine.admits exile copies total
          matches exile printing = Filter.matches context (Projection.viewOfCard (Card.combined (Printing.card printing))) (PregameExile.filter exile)
       in [Violation.PregameExileCount written total | total > 0, not (any allowed exiling)]
            <> [Violation.PregameExileMismatch (Face.name (Card.combined (Printing.card printing))) written | Just exile <- [exiling], printing <- Map.keys exiled, not (matches exile printing)]

-- CR 407.3: no ante card in deck or sideboard of a game not played for ante,
-- with a pool or without one.
anteCards :: GameSettings.GameSettings -> Deck.Deck -> [Violation.Violation]
anteCards settings deck = [Violation.AnteCard written | (written, card) <- cardsIn deck, Ante.barred settings (Card.combined card)]

-- CR 315.1: no conspiracy card in constructed play, sideboard included.
conspiracies :: Deck.Deck -> [Violation.Violation]
conspiracies deck = [Violation.ConspiracyCard written | (written, card) <- cardsIn deck, Set.member CardType.Conspiracy (TypeLine.types (Face.typeLine (Card.combined card)))]

-- CR 207.5: two colours circled on each copy of a card that instructs it,
-- in deck, sideboard and commanders, and none on any other card. Each name and
-- count once.
circledColors :: Deck.Deck -> [Violation.Violation]
circledColors deck =
  Set.toList $
    Set.fromList
      [ Violation.MiscircledCard (Face.name face) circled instructed
      | printing <- Map.keys (bothSections deck) <> Map.keys (Deck.commander deck),
        let face = Card.combined (Printing.card printing)
            circled = Int.toNaturalSaturating (Set.size (Printing.circled printing))
            instructed = if Face.circlesTwo face then 2 else 0 :: Natural.Natural,
        circled /= instructed
      ]

-- CR 123.2a: a constructed player who brings sticker sheets brings at least
-- ten, each unique; one who brings none plays without stickers. A Commander
-- deck is built ahead of time, so constructed play (CR 100.2a). Sheets are
-- told apart by name, as Parun.DeckList's Stickers section names them.
stickerSheets :: Deck.Deck -> [Violation.Violation]
stickerSheets deck =
  let brought = Deck.stickerSheets deck
      size = Natural.length brought
      tallies = Map.fromListWith (+) [(StickerSheet.name sheet, 1 :: Natural.Natural) | sheet <- Foldable.toList brought]
   in [Violation.TooFewStickerSheets size 10 | not (null brought), size < 10]
        <> [Violation.DuplicateStickerSheet name count | (name, count) <- Map.toList tallies, count > 1]

-- Each card of deck, sideboard and commanders once, by name.
cardsIn :: Deck.Deck -> [(CardName.CardName, Card.Type.Card)]
cardsIn deck = Map.toList (Map.fromList [(Face.name (Card.combined card), card) | printing <- Map.keys (everySection deck), let card = Printing.card printing])

-- CR 205.4c: the supertype makes a basic land, not the name or a basic land
-- type, so snow basics and Wastes are exempt and Dryad Arbor is not.
basicLand :: Card.Type.Card -> Bool
basicLand card =
  let typeLine = Face.typeLine (Card.combined card)
   in Set.member Supertype.Basic (TypeLine.supertypes typeLine) && Set.member CardType.Land (TypeLine.types typeLine)

-- | Everything wrong with a deck list, as lines a person can read: its bad
-- lines, then its deck's violations of the format asked for or its header
-- names, against the card pool given, in a game not for ante. Beside them,
-- the format checked.
report :: Maybe Text.Text -> Maybe CardPool -> DeckList.Listed -> IO (Maybe Text.Text, [Text.Text])
report asked cardPool listed = do
  let (format, disagreement) = DeckList.formatFor asked listed
      lines' = fmap DeckList.explain (DeckList.problems listed <> disagreement)
  checked <- traverse named format
  let violations = case checked of
        Nothing -> []
        Just (Left problem) -> [problem]
        Just (Right known) -> fmap explain (known GameSettings.plain cardPool (DeckList.deck listed))
  pure (format, lines' <> violations)

-- | A violation as a line a person can read.
explain :: Violation.Violation -> Text.Text
explain violation = Text.pack $ case violation of
  Violation.TooFewCards pile size least -> show size <> " cards in " <> pileName pile <> ", fewer than " <> show least
  Violation.TooManyCards size most -> show size <> " cards in the deck, more than " <> show most
  Violation.TooManyCopies pile written count limit -> show count <> " copies of " <> spelled written <> foldMap (\kind -> " in " <> pileName (Just kind)) pile <> ", more than " <> show limit
  Violation.SideboardTooLarge size most -> show size <> " cards in the sideboard, more than " <> show most
  Violation.BannedCard written -> spelled written <> " is banned"
  Violation.NotLegalCard written -> spelled written <> " is not legal"
  Violation.NoLegalityData written -> spelled written <> " has no legality data"
  Violation.AnteCard written -> spelled written <> " is an ante card in a game not for ante"
  Violation.ConspiracyCard written -> spelled written <> " is a conspiracy card in constructed play"
  Violation.MiscircledCard written circled instructed -> spelled written <> " circles " <> show circled <> " colours, not " <> show instructed
  Violation.NoCommander -> "no commander"
  Violation.IllegalCommander written -> spelled written <> " cannot be this deck's commander"
  Violation.OutsideColorIdentity written -> spelled written <> " is outside the commander's colour identity"
  Violation.TooFewStickerSheets size least -> show size <> " sticker sheets, fewer than " <> show least
  Violation.DuplicateStickerSheet name count -> show count <> " copies of the sticker sheet " <> Text.unpack name <> ", more than 1"
  Violation.TooManyPhenomena count most -> show count <> " phenomena in the planar deck, more than " <> show most
  Violation.MisplacedCard pile written -> spelled written <> " does not belong in " <> pileName (Just pile)
  Violation.NotInCardPool written -> spelled written <> " is not in the card pool"
  Violation.NotVanguardCard written -> spelled written <> " is not a vanguard card"
  Violation.MissingVanguard missing players -> show missing <> " of " <> show players <> " players without a vanguard card"
  Violation.PregameExileCount written count -> show count <> " cards exiled with cards named " <> spelled written <> ", which the deck's do not allow"
  Violation.PregameExileMismatch exiled written -> spelled exiled <> " is not a card that cards named " <> spelled written <> " exile"
  where
    spelled = Text.unpack . CardName.unwrap
    pileName pile = case pile of
      Nothing -> "the deck"
      Just SideDeck.Attraction -> "the Attraction deck"
      Just SideDeck.Contraption -> "the Contraption deck"
      Just SideDeck.Planar -> "the planar deck"
      Just SideDeck.Scheme -> "the scheme deck"

-- | The check of the shipped format a name means, against the shipped table:
-- "constructed" and "limited" for the rules alone, "vintage" and the like with
-- a pool.
named :: Text.Text -> IO (Either Text.Text (GameSettings.GameSettings -> Maybe CardPool -> Deck.Deck -> [Violation.Violation]))
named name = do
  found <- shipped name
  case found of
    Left problem -> pure (Left problem)
    Right format -> do
      table <- tablePath >>= loadTable
      pure (fmap (\loaded settings -> check loaded settings format) table)

-- | The shipped format a name means, or why there is none.
shipped :: Text.Text -> IO (Either Text.Text Format.Format)
shipped name = do
  root <- formatsRoot
  found <- loadFormat root name
  pure $ case found of
    Nothing -> Left (Text.pack "not a format: " <> name)
    Just (Left problem) -> Left (name <> Text.pack ": " <> problem)
    Just (Right format) -> Right format

-- | Where the shipped legality table is.
tablePath :: IO FilePath
tablePath = Paths.getDataFileName "legalities.json"

-- | The legality table a file holds.
loadTable :: FilePath -> IO (Either Text.Text LegalityTable.LegalityTable)
loadTable path = fmap (decodeWith LegalityTable.Codec.codec) (ByteString.readFile path)

-- | The directory the shipped formats are in, one file each.
formatsRoot :: IO FilePath
formatsRoot = Paths.getDataFileName "formats"

-- | The format a name means, the file of that name under a formats root:
-- Nothing when there is none.
loadFormat :: FilePath -> Text.Text -> IO (Maybe (Either Text.Text Format.Format))
loadFormat root name
  | Text.null name || Text.any (\c -> not (Char.isAsciiLower c || Char.isDigit c)) name = pure Nothing
  | otherwise = do
      let path = root <> "/" <> Text.unpack name <> ".json"
      exists <- Directory.doesFileExist path
      if exists
        then fmap (Just . decodeWith Format.Codec.codec) (ByteString.readFile path)
        else pure Nothing

decodeWith :: Codec.Codec a -> ByteString.ByteString -> Either Text.Text a
decodeWith codec bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode codec

-- | A table as data/legalities.json holds it: Parun.Codec.LegalityTable's
-- wire, one card a line so a ban announcement's diff names the cards.
renderTable :: LegalityTable.LegalityTable -> Text.Text
renderTable table =
  let quoted = Common.render . Codec.encode Common.text
      card (key, pools) = Text.pack "    " <> quoted key <> Text.pack ": " <> Common.render (Codec.encode (Common.textMap id Right Legality.Codec.codec) pools)
   in Text.unlines
        [ Text.pack "{",
          Text.pack "  \"date\": " <> quoted (LegalityTable.date table) <> Text.pack ",",
          Text.pack "  \"cards\": {",
          Text.intercalate (Text.pack ",\n") (fmap card (Map.toAscList (LegalityTable.cards table))),
          Text.pack "  }",
          Text.pack "}"
        ]
