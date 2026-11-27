-- | A card as the board draws it: a tile coloured by the object's colours,
-- with its name, cost, type line, power and toughness and status, and the
-- sheet a tapped tile opens, with its Oracle text and what the seat may do
-- with it.
module Parun.Web.Card where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Card as Engine.Card
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Oracle as Oracle
import qualified Parun.Oracle.Cost as Oracle.Cost
import qualified Parun.Oracle.Face as Oracle.Face
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Oracle.Normalise as Normalise
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CharacteristicColors as CharacteristicColors
import qualified Parun.Types.ClassLevel as ClassLevel
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Defense as Defense
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Loyalty as Loyalty
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RoomHalf as RoomHalf
import qualified Parun.Types.RoomIndex as RoomIndex
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Mana as Mana

-- | How a tile looks beyond the object itself, and what tapping it sends.
data Cue msg = MkCue
  { -- | Extra classes: "playable", "choosable", "chosen".
    marks :: [String],
    tap :: msg,
    -- | What a pointer resting on the tile sends, what leaving it does, and
    -- what a long press or a right click does (page.js's "peek", "unpeek"
    -- and "read"), where the tile previews its sheet and opens it without
    -- what a tap does.
    peek :: Maybe (msg, msg, msg)
  }
  deriving (Eq, Show)

-- | An object's tile: its name and cost, type line, keywords, and a foot of
-- its status and stats.
tile :: PlayerView.PlayerView -> Cue msg -> ObjectView.ObjectView -> Html.Html msg
tile view cue o = case ObjectView.characteristics o of
  Nothing | Maybe.isNothing (ObjectView.face o) -> Element.button (["card", "back"] <> marks cue <> tapped o) (tap cue) [Element.el "span" ["name"] [] [Element.string "Hidden card"]]
  _ ->
    Element.el
      "button"
      (["card", colour o] <> marks cue <> tapped o <> combat view o <> ["sick" | sickNow view o] <> ["targeted" | not (null (targetedBy view (ObjectView.object o)))])
      ([Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (summary view cue o), Element.on "click" (tap cue)] <> peeking cue)
      [ Element.el "span" ["top"] [] $ [Element.el "span" ["name"] [] [Element.text (Wording.nameOf view o)]] <> foldMap (pure . Mana.cost) (ObjectView.characteristics o >>= ProjectedCharacteristics.manaCost),
        -- Every tile has the same lines, each empty or cut short rather than
        -- moved or wrapped, so a row of them is one height.
        Element.el "span" ["type"] [] (legendMark o <> [Element.text (typeLine o)]),
        Element.el "span" ["keywords"] [] [Element.text (Text.intercalate (Text.pack ", ") (tileKeywords o))],
        Element.el "span" ["foot"] [] (states view o <> badges o <> stats o)
      ]

-- | Whether the object attacks or blocks (CR 508.1, 509.1).
combat :: PlayerView.PlayerView -> ObjectView.ObjectView -> [String]
combat view o =
  let oid = ObjectView.object o
   in ["attacking" | Map.member oid (PlayerView.attackers view)] <> ["blocking" | any (Set.member oid) (PlayerView.blockers view)]

-- | Whether the object is summoning sick where that can matter (CR 302.6):
-- on its controller's turn, when it could attack, and otherwise only while
-- it has an ability with {T} or {Q} in its cost.
sickNow :: PlayerView.PlayerView -> ObjectView.ObjectView -> Bool
sickNow view o =
  ObjectView.summoningSick o
    && (ObjectView.controller o == Just (PlayerView.active view) || any tapAbility (ObjectView.characteristics o))
  where
    tapAbility c =
      not (null (Subtype.intrinsicManaOf c))
        || any (any (`elem` [CostComponent.TapThis, CostComponent.UntapThis]) . Cost.components . ActivatedAbility.cost) (ProjectedCharacteristics.activatedAbilities c)

-- | The states a tile spells out: attacking, blocking, summoning sick, its
-- designations (marked) and its choices (chosen).
states :: PlayerView.PlayerView -> ObjectView.ObjectView -> [Html.Html msg]
states view o =
  let shown cls label tip = Element.el "span" ["badge", "state", cls] [Element.attr "title" (Text.pack tip)] [Element.string label]
      kinds = combat view o
      by = targetedBy view (ObjectView.object o)
   in [shown "target" "Target" ("Targeted by " <> Text.unpack (Wording.listing by)) | not (null by)]
        <> [shown "attack" "Attacking" "Attacking" | elem "attacking" kinds]
        <> [shown "block" "Blocking" "Blocking" | elem "blocking" kinds]
        <> [shown "sick" "Sick" "Summoning sick: can't attack or tap yet (CR 302.6)" | sickNow view o]
        <> [shown "marked" label tip | (label, tip) <- marked o]
        <> [shown "chosen-value" label tip | (label, tip) <- chosen view o]

-- | The designations an object has, short and spelled out: the
-- Ring-bearer (CR 701.54b), a Class's level (CR 717.2b), a Room's unlocked
-- halves (CR 709.5c), a dungeon's room (CR 309.4), a card on an adventure
-- (CR 716.3d), the rest (Parun.Types.Designation), what a copy effect
-- covers (CR 707.2) and a commander's tax (CR 903.8).
marked :: ObjectView.ObjectView -> [(String, String)]
marked o =
  [("Ring-bearer", "The Ring-bearer: legendary, and harder to block (CR 701.54)") | Maybe.isJust (ObjectView.ringBearerFor o)]
    <> [("Protected by " <> Text.unpack (Wording.player p), "Its protector, who may block for it and whom it can't attack (CR 310.9)") | Just p <- [ObjectView.protector o]]
    <> [("Level " <> show (ClassLevel.unwrap l), "Class level " <> show (ClassLevel.unwrap l) <> " (CR 717)") | Just l <- [ObjectView.classLevel o]]
    <> [ (label, label <> " (CR 709.5)")
       | not (Set.null (ObjectView.unlockedHalves o)),
         let label = case Set.toList (ObjectView.unlockedHalves o) of
               [RoomHalf.LeftHalf] -> "Left door unlocked"
               [RoomHalf.RightHalf] -> "Right door unlocked"
               _ -> "Both doors unlocked"
       ]
    <> [("Room " <> show (RoomIndex.unwrap r + 1), "The venture marker is on room " <> show (RoomIndex.unwrap r + 1) <> " (CR 309.4)") | Just r <- [ObjectView.ventureRoom o]]
    <> [("On an adventure", "On an adventure: it may be played from exile, though not as its Adventure (CR 716.3d)") | ObjectView.onAdventure o]
    <> [(designationWord d, designationWord d) | d <- Set.toList (ObjectView.designations o)]
    <> [("Copy", "A copy: beneath the copy effect, it is " <> Text.unpack (CardName.unwrap n) <> " (CR 707.2)") | Just n <- [ObjectView.beneathCopy o]]
    <> [("Tax +" <> show n, "Costs {" <> show n <> "} more to cast from the command zone (CR 903.8)") | let n = ObjectView.commanderTax o, n > 0]

-- | The choices an object holds (CR 607.2d), short and spelled out: a
-- creature type, colour, card type, name, player, number or objects, each
-- acquisition's (CR 607.5) alike.
chosen :: PlayerView.PlayerView -> ObjectView.ObjectView -> [(String, String)]
chosen view o =
  ListUtils.nubOrd $
    each ObjectView.chosenSubtype (\ts -> let w = joined (fmap typeWord (NonEmpty.toList ts)) in [(w, "Chosen type: " <> w)])
      <> each ObjectView.chosenColors (\cs -> let w = joined (fmap show (Set.toList cs)) in [(w, "Chosen color: " <> w) | not (Set.null cs)])
      <> each ObjectView.chosenColorless (const [("Colorless", "Chosen: colorless")])
      <> each ObjectView.chosenCardType (\t -> [(show t, "Chosen card type: " <> show t)])
      <> each ObjectView.chosenNames named
      <> each ObjectView.chosenPlayers (fmap (uncurry picked) . Map.toList)
      <> each ObjectView.chosenNumber (\n -> [(show n, "Chosen number: " <> show n)])
      <> each ObjectView.chosenNumberBy (fmap (\(p, n) -> (seat p <> ": " <> show n, seat p <> " chose number " <> show n)) . Map.toList)
      <> each ObjectView.chosenObjects (\ids -> [("Chose " <> objects ids, "Chosen: " <> objects ids) | not (Set.null ids)])
  where
    -- Every acquisition's choice of one kind.
    each :: (ObjectView.ObjectView -> Map.Map k a) -> (a -> [(String, String)]) -> [(String, String)]
    each field f = concatMap f (Map.elems (field o))
    joined = List.intercalate ", "
    typeWord t = maybe (show t) Text.unpack (Subtype.creatureTypeWord t)
    seat = Text.unpack . Wording.player
    objects = Text.unpack . Wording.listing . fmap (Wording.named view) . Set.toList
    -- Each player's names, "Player 2 named" where more than one player named.
    named byPlayer =
      [ (label, if Map.size byPlayer > 1 then seat p <> " named " <> list else label)
      | (p, ns) <- Map.toList byPlayer,
        not (Set.null ns),
        let list = joined (fmap (Text.unpack . CardName.unwrap) (Set.toList ns)),
        let label = "Named " <> list
      ]
    picked which p = case which of
      PlayerChoice.ChosenPlayer -> (seat p, "Chosen player: " <> seat p)
      PlayerChoice.Promised _ _ -> ("Gift: " <> seat p, "Gift promised to " <> seat p <> " (CR 702.174a)")
      PlayerChoice.SecretChooser -> ("Chosen secretly", seat p <> " chose secretly")

designationWord :: Designation.Designation -> String
designationWord d = case d of
  Designation.Renowned -> "Renowned"
  Designation.Monstrous -> "Monstrous"
  Designation.Suspected -> "Suspected"
  Designation.Solved -> "Solved"
  Designation.Saddled -> "Saddled"
  Designation.Prepared -> "Prepared"
  Designation.Harnessed -> "Harnessed"
  Designation.Embalmed -> "Embalmed"
  Designation.AlphaSector -> "Alpha sector"
  Designation.BetaSector -> "Beta sector"
  Designation.GammaSector -> "Gamma sector"
  Designation.Sprocket1 -> "Sprocket 1"
  Designation.Sprocket2 -> "Sprocket 2"
  Designation.Sprocket3 -> "Sprocket 3"

-- | What on the stack targets the object, by name (CR 601.2c, 602.2b, 603.3d).
targetedBy :: PlayerView.PlayerView -> ObjectId.ObjectId -> [Text.Text]
targetedBy view oid = [Wording.nameOf view item | item <- PlayerView.stack view, any (aims oid) (ObjectView.targets item)]
  where
    aims target r = case r of
      Recipient.ToCreature x -> x == target
      Recipient.ToPlaneswalker x -> x == target
      Recipient.ToBattle x -> x == target
      Recipient.ToObject x -> x == target
      Recipient.ToPlayer _ -> False
      Recipient.ToPile _ -> False

-- | What on the stack targets the player, by name.
targetingPlayer :: PlayerView.PlayerView -> PlayerId.PlayerId -> [Text.Text]
targetingPlayer view pid = [Wording.nameOf view item | item <- PlayerView.stack view, Recipient.ToPlayer pid `elem` ObjectView.targets item]

-- | The keyword abilities the object has now, as Oracle text prints each;
-- granted ones included.
keywords :: ObjectView.ObjectView -> [Text.Text]
keywords o = case ObjectView.characteristics o of
  Nothing -> []
  Just c ->
    let self = Text.intercalate (Text.pack " // ") (fmap CardName.unwrap (Set.toList (ProjectedCharacteristics.names c)))
     in ListUtils.nubOrd (concatMap (take 1 . Oracle.Keyword.printed self) (Map.keys (ProjectedCharacteristics.keywords c)))

-- | The keywords short enough for a tile; the sheet lists them all.
tileKeywords :: ObjectView.ObjectView -> [Text.Text]
tileKeywords = filter (\t -> Text.length t <= 18 && not (Text.any (== '{') t)) . keywords

-- | The handlers a previewing tile has, and the mark (data-read) telling
-- page.js its long press reads it.
peeking :: Cue msg -> [Attribute.Attribute msg]
peeking cue = foldMap (\(on, off, open) -> [Element.on "peek" on, Element.on "unpeek" off, Element.on "read" open, Element.attr "data-read" Text.empty]) (peek cue)

-- | The tile read aloud: its name, type and keywords, power and toughness,
-- status, and what tapping it does.
summary :: PlayerView.PlayerView -> Cue msg -> ObjectView.ObjectView -> Text.Text
summary view cue o =
  let pt = case ObjectView.characteristics o of
        Just c | Just p <- ProjectedCharacteristics.power c, Just t <- ProjectedCharacteristics.toughness c -> [Text.pack (show p <> "/" <> show t)]
        _ -> []
      doing =
        [Text.pack "tapped" | ObjectView.tapped o == TapState.Tapped]
          <> [Text.pack "attacking" | elem "attacking" (combat view o)]
          <> [Text.pack "blocking" | elem "blocking" (combat view o)]
          <> [Text.pack "summoning sick" | sickNow view o]
          <> [Text.pack "targeted by " <> Wording.listing by | let by = targetedBy view (ObjectView.object o), not (null by)]
          <> [Text.pack (show (ObjectView.damage o) <> " damage") | ObjectView.damage o > 0]
      action
        | elem "playable" (marks cue) = [Text.pack "you can play or use it"]
        | elem "auto-next" (marks cue) = [Text.pack "can be chosen, and paying automatically taps it next"]
        | elem "focused" (marks cue) = [Text.pack "blocking with it: choose the attacker it blocks"]
        | elem "chosen" (marks cue) = [Text.pack "chosen"]
        | elem "choosable" (marks cue) = [Text.pack "can be chosen"]
        | otherwise = []
      typed = [Text.pack "legendary" | isLegendary o] <> [typeLine o]
      kind = case tileKeywords o of
        [] -> typed
        words' -> typed <> [Text.intercalate (Text.pack ", ") words']
   in Text.intercalate (Text.pack ", ") ([Wording.nameOf view o] <> kind <> pt <> doing <> action)

tapped :: ObjectView.ObjectView -> [String]
tapped o = ["tapped" | ObjectView.tapped o == TapState.Tapped]

-- | The colour class: one per colour, "multi" for two or more, and for
-- none "land" or "colorless".
colour :: ObjectView.ObjectView -> String
colour o =
  let colours = foldMap (Set.toList . ProjectedCharacteristics.colors) (ObjectView.characteristics o)
   in case colours of
        [c] -> "c-" <> letter c
        [] | Wording.isA CardType.Land o -> "c-land"
        [] -> "c-none"
        _ -> "c-multi"

-- | A colour's mana symbol letter (CR 107.4).
letterUpper :: Color.Color -> String
letterUpper c = fmap Char.toUpper (letter c)

letter :: Color.Color -> String
letter c = case c of
  Color.White -> "w"
  Color.Blue -> "u"
  Color.Black -> "b"
  Color.Red -> "r"
  Color.Green -> "g"

-- | CR 205.4d: whether the object is legendary, so subject to the legend rule.
isLegendary :: ObjectView.ObjectView -> Bool
isLegendary = any (Set.member Supertype.Legendary . ProjectedCharacteristics.supertypes) . ObjectView.characteristics

-- | A crown ahead of a legendary tile's type line, which leaves out the
-- supertype (CR 205.4d).
legendMark :: ObjectView.ObjectView -> [Html.Html msg]
legendMark o = [Element.el "span" ["legend"] [Element.attr "title" (Text.pack "Legendary (CR 205.4d)")] [Icon.icon [] Icon.MkCrown] | isLegendary o]

-- | The type line, short: subtypes where there are any, else card types.
typeLine :: ObjectView.ObjectView -> Text.Text
typeLine o = case ObjectView.ability o of
  Just _ -> Text.pack "Ability"
  Nothing -> case ObjectView.characteristics o of
    Nothing -> Text.pack "Face down"
    Just c ->
      let subtypes = subtypeWords c
          types = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.cardTypes c))
          named = Set.map CardName.unwrap (ProjectedCharacteristics.names c)
       in -- A basic land's subtype is its name, so its card types say more;
          -- an object with no types is an emblem (CR 114.3).
          if null subtypes && null types then Text.pack "Emblem" else if null subtypes || Set.member (Text.unwords subtypes) named then Text.unwords types else Text.unwords subtypes

-- | The subtypes `c` gives the object, in printed order (Subtype.inPrintedOrder).
subtypeWords :: ProjectedCharacteristics.ProjectedCharacteristics -> [Text.Text]
subtypeWords c = fmap (Text.pack . show) (Subtype.inPrintedOrder c)

-- | Damage marked and counters, each a small badge.
badges :: ObjectView.ObjectView -> [Html.Html msg]
badges o =
  [Element.el "span" ["badge", "damage"] [Element.attr "title" (Text.pack "Damage")] [Element.string (show (ObjectView.damage o) <> " dmg")] | ObjectView.damage o > 0]
    <> [Element.el "span" ["badge", "counter"] [] [Element.text (Text.pack (show n <> " ") <> Wording.counterName kind)] | (kind, n) <- Map.toList (ObjectView.counters o), n > 0, not (cornered kind)]
  where
    -- The corner shows these (stats).
    cornered kind = case (kind, ObjectView.characteristics o) of
      (CounterKind.Loyalty, Just c) -> Maybe.isJust (ProjectedCharacteristics.loyalty c)
      (CounterKind.Defense, Just c) -> Maybe.isJust (ProjectedCharacteristics.defense c)
      _ -> False

-- | Power and toughness, loyalty or defense, as printed in the corner.
stats :: ObjectView.ObjectView -> [Html.Html msg]
stats o = case ObjectView.characteristics o of
  Nothing -> []
  Just c ->
    let -- On the battlefield, its loyalty or defense is its counters
        -- (CR 306.5c, 310.4c), not the printed number.
        counted kind = Map.lookup kind (ObjectView.counters o)
        loyalty l = case l of
          Loyalty.Literal n -> show n
          Loyalty.Variable -> "X"
        -- Raised or lowered from its base power and toughness (CR 208.4b) by
        -- counters and effects.
        printedPT = (,) <$> ProjectedCharacteristics.basePower c <*> ProjectedCharacteristics.baseToughness c
        base = foldMap (\(bp, bt) -> ", base " <> show bp <> "/" <> show bt) printedPT
        change p t = case printedPT of
          Just (bp, bt)
            | (p, t) == (bp, bt) -> []
            | p >= bp && t >= bt -> ["up"]
            | p <= bp && t <= bt -> ["down"]
            | otherwise -> ["mixed"]
          Nothing -> []
     in foldMap (\(p, t) -> [Element.el "span" (["pt"] <> change p t) [Element.attr "title" (Text.pack ("Power and toughness" <> base))] [Element.string (show p <> "/" <> show t)]]) ((,) <$> ProjectedCharacteristics.power c <*> ProjectedCharacteristics.toughness c)
          <> foldMap (\l -> [Element.el "span" ["pt", "loyalty"] [Element.attr "title" (Text.pack "Loyalty")] [Element.string (maybe (loyalty l) show (counted CounterKind.Loyalty))]]) (ProjectedCharacteristics.loyalty c)
          <> foldMap (\d -> [Element.el "span" ["pt", "defense"] [Element.attr "title" (Text.pack "Defense")] [Element.string (maybe (show (Defense.unwrap d)) show (counted CounterKind.Defense))]]) (ProjectedCharacteristics.defense c)

-- | The Oracle text of the faces the seat may read the object by
-- (Wording.cardNames), each line apart, a face's name heading its own where
-- there are several; then, under a heading each, the printed text of the
-- cards merged under it (ObjectView.merged); or why the card data could not
-- say. The faces the object has say what its abilities are now (wording),
-- together where there are several; every other face says what is printed on
-- it.
oracle :: (CardName.CardName -> Either e (Maybe Card.Card)) -> ObjectView.ObjectView -> Either e [Text.Text]
oracle lookUp o = do
  let names = Wording.cardNames o
  looked <- traverse lookUp (Set.toList names)
  under <- traverse (traverse (\n -> fmap ((,) n) (lookUp n))) (Foldable.toList (ObjectView.merged o))
  -- Every face printed on the card, those it shows first: a double-faced
  -- card's other face, a flip card's other half and an adventure are on the
  -- card for anyone who sees it to read.
  let every = concatMap (Foldable.toList . Card.faces) (ListUtils.nubOrd (Maybe.catMaybes looked))
      (shown, others) = List.partition (\f -> Set.member (Face.name f) names) every
      faces = shown <> others
      -- The characteristics the names came from (Wording.cardNames), and
      -- ownEffects is beside whichever they are.
      now = ObjectView.face o Applicative.<|> ObjectView.characteristics o
      -- The faces it shows whose characteristics it has, by their names: a
      -- split card's halves off the stack (CR 709.4), the half cast (CR
      -- 709.3b), a Room's unlocked doors (CR 709.5); and those together.
      had = filter (\f -> any (Set.member (Face.name f) . ProjectedCharacteristics.names) now) shown
      own = ObjectView.ownEffects o
      room = ProjectedCharacteristics.halves =<< now
      -- On the battlefield, where a Room has its designations (CR 709.5c): a
      -- door unlocked, or no name, which a Room has nowhere else (CR 709.3b,
      -- 709.4).
      placed = not (Set.null (ObjectView.unlockedHalves o)) || null had
      locked f = placed && any (any (`Set.notMember` ObjectView.unlockedHalves o) . Engine.Card.halfPositionOf (Face.name f)) room
      whole = case (now, had) of
        (Just c, one : more@(_ : _)) -> Just (wordingOf own c (one NonEmpty.:| more))
        -- A Room with no door unlocked has neither door's rules text (CR 709.5).
        (Just c, []) | Just card <- room, placed -> Just (wording own c (Engine.Card.roomFace Set.empty card))
        _ -> Nothing
      body f = case (now, had) of
        (Just c, [one]) | Face.name one == Face.name f -> said (wording own c f) (printed f)
        _ -> printed f
      heading f
        | locked f = " (locked):"
        | Set.member (Face.name f) names || null shown = ":"
        | otherwise = " (other face):"
      lines' f = [CardName.unwrap (Face.name f) <> Text.pack (heading f) | length faces > 1 || not (null under)] <> body f
      -- Faces it has whose abilities differ say them once, under their names.
      body' = case whole of
        Just (MkWorded worded) ->
          [Text.intercalate (Text.pack " // ") (fmap (CardName.unwrap . Face.name) had) <> Text.pack ":" | not (null had)]
            <> said (MkWorded worded) []
            <> concatMap lines' (filter (`notElem` had) faces)
        Just MkUnworded -> said MkUnworded (concatMap lines' faces)
        _ -> concatMap lines' faces
      -- A merged card's face it shows (CR 731.2), as printed.
      merged component = case component of
        Just (n, card) -> (CardName.unwrap n <> Text.pack " (merged under it):") : concatMap printed (filter ((== n) . Face.name) (foldMap (Foldable.toList . Card.faces) card))
        Nothing -> [Text.pack "A face-down card (merged under it)"]
  pure ((if null faces then foldMap abilityLines now else body') <> concatMap merged under)

-- | A face's printed Oracle text, a line each.
printed :: Face.Face Card.Card -> [Text.Text]
printed = foldMap Text.lines . Face.oracleText

-- | What faces say of the object's abilities (CR 113.1a), given how they
-- stand (wording) and their printed text: that text while the abilities are
-- what it says, the renderer's wording of them once they differ, and otherwise
-- that text under a note that it may be out of date.
said :: Wording -> [Text.Text] -> [Text.Text]
said how asPrinted = case how of
  MkAsPrinted -> asPrinted
  MkWorded [] -> [Text.pack "It has no abilities."]
  MkWorded worded -> worded
  -- Not implemented: wording an ability the renderer can't (#8955).
  MkUnworded -> Text.pack "Its abilities have changed; this text may be out of date." : asPrinted

-- | wording of the faces an object has together, combined as CR 709.4 and
-- 709.5 combine them (Parun.Engine.Card.foldSplit). The combined view keeps
-- the first face's spell alone (CR 709.3b), so a later face's spell abilities
-- can't be worded from it.
wordingOf :: Maybe ProjectedCharacteristics.ProjectedCharacteristics -> ProjectedCharacteristics.ProjectedCharacteristics -> NonEmpty.NonEmpty (Face.Face Card.Card) -> Wording
wordingOf own c faces = case wording own c (Engine.Card.foldSplit faces) of
  MkWorded _ | any ((/= Face.defaultSpell) . Face.spell) (NonEmpty.tail faces) -> MkUnworded
  how -> how

-- | How a face's text stands to the object's abilities now.
data Wording
  = -- | They are the face's printed abilities.
    MkAsPrinted
  | -- | They differ, and the renderer words every one, a line each.
    MkWorded [Text.Text]
  | -- | They differ, and the renderer can't word one.
    MkUnworded
  deriving (Eq, Show)

-- | Whether the abilities `c` gives the object are what its face says,
-- compared as card data (current) together with CR 305.6's intrinsic mana
-- abilities, and if not whether the renderer words each. What the face says
-- is the object under its own static abilities alone (ObjectView.ownEffects,
-- `own`), where one applies to it or it is merged: a Sliver's grant to itself
-- is its printed text, as Karlov Watchdog's "during your turn" is, and a
-- mutated permanent's abilities from under it its cards' (CR 702.140e).
wording :: Maybe ProjectedCharacteristics.ProjectedCharacteristics -> ProjectedCharacteristics.ProjectedCharacteristics -> Face.Face Card.Card -> Wording
wording own c face
  | now == baseline, intrinsicNow == intrinsicBaseline, extras c == maybe (mempty, mempty, Projection.View.seedCharacteristicPT face) extras own = MkAsPrinted
  | unworded = MkUnworded
  | otherwise = maybe MkUnworded (MkWorded . (<> fmap intrinsic intrinsicNow)) (traverse Maybe.listToMaybe (Oracle.faceAbilities now))
  where
    now = current c face
    printedLine = Face.typeLine face
    intrinsicPrinted = Subtype.intrinsicMana (TypeLine.types printedLine) (Set.fromList (TypeLine.subtypes printedLine))
    intrinsicNow = Subtype.intrinsicManaOf c
    baseline = maybe face (`current` face) own
    intrinsicBaseline = maybe intrinsicPrinted Subtype.intrinsicManaOf own
    -- What `current` leaves out.
    extras p = (ProjectedCharacteristics.lentKeywords p, ProjectedCharacteristics.grantedSpendManaAsThough p, ProjectedCharacteristics.characteristicPT p)
    intrinsic t = Text.pack "({T}: Add {" <> Oracle.Cost.manaType t <> Text.pack "}.)"
    unworded =
      -- What no face field holds.
      not (null (ProjectedCharacteristics.lentKeywords c))
        || not (null (ProjectedCharacteristics.grantedSpendManaAsThough c))
        -- A characteristic-defining P/T other than the printed one, which
        -- the projection holds with its star already substituted.
        || maybe False (\pt -> Just pt /= Projection.View.seedCharacteristicPT face) (ProjectedCharacteristics.characteristicPT c)

-- | The face with the abilities `c` gives the object written over its own
-- (CR 613.1f): the lists the layers write as they stand, the lists no layer
-- writes gone under a layer-6 wipe (ProjectedCharacteristics.lostAllAbilities),
-- and the grants beside them.
current :: ProjectedCharacteristics.ProjectedCharacteristics -> Face.Face Card.Card -> Face.Face Card.Card
current c face =
  let lost = ProjectedCharacteristics.lostAllAbilities c
      kept :: (Monoid a) => a -> a
      kept xs = if lost then mempty else xs
      colours = do
        defined <- Face.characteristicColors face
        Monad.guard (not lost && (ProjectedCharacteristics.definesChosenColor c || not (CharacteristicColors.chosen defined)))
        pure defined
   in Projection.View.withRuleAbilities
        (kept (ProjectedCharacteristics.ruleAbilities c) <> ProjectedCharacteristics.grantedRuleAbilities c)
        face
          { Face.keywords = ProjectedCharacteristics.keywords c,
            -- Any other characteristic-defining P/T is unworded (wording).
            Face.characteristicPT = if ProjectedCharacteristics.characteristicPT c == Projection.View.seedCharacteristicPT face then Face.characteristicPT face else Nothing,
            Face.characteristicColors = colours,
            Face.staticAbilities = kept (ProjectedCharacteristics.staticAbilities c) <> fmap snd (ProjectedCharacteristics.grantedStaticAbilities c),
            Face.playerAbilities = kept (ProjectedCharacteristics.playerAbilities c) <> fmap snd (ProjectedCharacteristics.grantedPlayerAbilities c),
            Face.specialActions = kept (ProjectedCharacteristics.specialActions c),
            Face.spell = ProjectedCharacteristics.spell c,
            Face.activatedAbilities = ProjectedCharacteristics.activatedAbilities c,
            Face.replacementEffects = ProjectedCharacteristics.replacementEffects c,
            Face.triggeredAbilities = ProjectedCharacteristics.triggeredAbilities c,
            Face.delayedAbilities = ProjectedCharacteristics.delayedAbilities c,
            Face.enchant = ProjectedCharacteristics.enchant c,
            Face.castingPermissions = ProjectedCharacteristics.castingPermissions c,
            Face.counterability = ProjectedCharacteristics.counterability c,
            Face.copyability = ProjectedCharacteristics.copyability c,
            Face.additionalCosts = kept (ProjectedCharacteristics.additionalCosts c),
            Face.additionalCostChoices = kept (ProjectedCharacteristics.additionalCostChoices c),
            Face.alternativeCosts = kept (ProjectedCharacteristics.alternativeCosts c) <> ProjectedCharacteristics.grantedAlternativeCosts c,
            Face.costReductions = kept (ProjectedCharacteristics.costReductions c) <> ProjectedCharacteristics.grantedCostReductions c,
            Face.castingRestrictions = kept (ProjectedCharacteristics.castingRestrictions c)
          }

-- | What an object no card data prints says, an emblem's abilities (CR
-- 114.3) say: each the renderer words, and a note when it can't word one.
abilityLines :: ProjectedCharacteristics.ProjectedCharacteristics -> [Text.Text]
abilityLines c =
  let self = CardName.MkCardName (Text.intercalate (Text.pack " // ") (fmap CardName.unwrap (Set.toList (ProjectedCharacteristics.names c))))
      bare = Oracle.Face.bare self Nothing Nothing Set.empty TypeLine.empty Nothing Nothing Map.empty
      worded = Oracle.faceAbilities (current c bare)
   in concatMap (take 1) worded <> [Text.pack "It has abilities this can't word yet." | any null worded]

-- | What the sheet shows of the object it reads.
data Reading = MkReading
  { object :: ObjectId.ObjectId,
    -- | Its Oracle text, a line each (oracle), or why there is none.
    text :: [Text.Text],
    -- | Opened by a tap, rather than previewed under a resting pointer.
    pinned :: Bool
  }
  deriving (Eq, Show)

-- | The sheet a tapped tile opens: the object large, its full type line,
-- status and Oracle text, and these buttons, the seat's options for it; an
-- ability's text as `texts` words it (quoted).
sheet :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectView.ObjectView -> Reading -> [Html.Html msg] -> msg -> Html.Html msg
sheet texts view o reading actions close =
  Element.el
    "div"
    ["sheet-backdrop", if pinned reading then "" else "peek"]
    (if pinned reading then [Element.attr "role" (Text.pack "dialog"), Element.attr "aria-modal" (Text.pack "true")] else [Element.attr "aria-hidden" (Text.pack "true")])
    $ [Element.button ["scrim"] close [Element.el "span" ["sr-only"] [] [Element.string "Close"]] | pinned reading]
      <> [ Element.el
             "section"
             ["sheet", colour o]
             []
             $ [ Element.el "header" [] [] $
                   [Element.el "h2" [] [] [Element.text (Wording.nameOf view o)]]
                     <> foldMap (pure . Mana.cost) (ObjectView.characteristics o >>= ProjectedCharacteristics.manaCost),
                 Element.el "p" ["type-line"] [] [Element.text (fullType o)]
               ]
               <> [Element.el "p" ["status"] [] (Mana.rich line) | line <- status texts view o]
               <> [Element.el "div" ["oracle"] [] [Element.el "p" [] [] (Mana.rich line) | line <- text reading, not (Text.null line)] | not (null (text reading))]
               <> [Element.el "div" ["stats-row"] [] (badges o <> stats o) | not (null (badges o <> stats o))]
               <> [Element.el "div" ["sheet-actions"] [] actions | pinned reading, not (null actions)]
               <> [Element.button ["close"] close [Element.string "Close"] | pinned reading]
         ]

-- | The full type line: supertypes, card types, a dash, then subtypes.
fullType :: ObjectView.ObjectView -> Text.Text
fullType o = case (ObjectView.ability o, ObjectView.characteristics o) of
  (Just _, _) -> Text.pack "Ability on the stack"
  (Nothing, Nothing) -> Text.pack "Face-down card"
  (Nothing, Just c) ->
    let supertypes = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.supertypes c))
        types = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.cardTypes c))
        subtypes = subtypeWords c
     in -- An object with no types is an emblem (CR 114.3).
        if null (supertypes <> types <> subtypes) then Text.pack "Emblem" else Text.unwords (supertypes <> types <> if null subtypes then [] else Text.pack "\8212" : subtypes)

-- | What the tile cannot fit: tapped, its designations and choices spelled
-- out, what it is attached to, what an ability says and what it targets, and
-- the card behind a face-down one.
status :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectView.ObjectView -> [Text.Text]
status texts view o =
  [Text.pack "Tapped" | ObjectView.tapped o == TapState.Tapped]
    <> [Text.pack "Attacking" | Map.member (ObjectView.object o) (PlayerView.attackers view)]
    <> [Text.pack "Blocking" | any (Set.member (ObjectView.object o)) (PlayerView.blockers view)]
    <> [Text.pack "Summoning sick: it can't attack or use {T} abilities until its controller's next turn" | ObjectView.summoningSick o]
    <> [Text.pack tip | (_, tip) <- marked o <> chosen view o]
    <> [Text.pack "Keyword abilities: " <> Text.intercalate (Text.pack ", ") (keywords o) | not (null (keywords o))]
    <> [Text.pack "Attached to " <> Wording.recipientName view r | r <- Maybe.maybeToList (ObjectView.attachedTo o)]
    <> quoted texts view o
    <> targeting view o
    <> faceDown o

-- | A stack entry's detail, alike for a spell and an ability: what it says
-- (a spell's text as `texts` holds it, else what its abilities say; an
-- ability's text), then what it targets, and the card behind a face-down
-- spell.
stackDetail :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectView.ObjectView -> [Text.Text]
stackDetail texts view o =
  ( case (ObjectView.ability o, Map.lookup (ObjectView.object o) texts) of
      (Just _, _) -> quoted texts view o
      (Nothing, Just t) | not (null t) -> t
      _ -> foldMap abilityLines (ObjectView.characteristics o)
  )
    <> targeting view o
    <> faceDown o

-- | The Oracle text (oracle) of each object the seat sees, by its object,
-- for a spell's stack entry (stackDetail) and for wording an ability as its
-- source's sheet does (asShown); empty where the card data can't say. Each
-- is worked out only when read.
written :: (CardName.CardName -> Either e (Maybe Card.Card)) -> PlayerView.PlayerView -> Map.Map ObjectId.ObjectId [Text.Text]
written lookUp view = fmap (Either.fromRight [] . oracle lookUp) (Wording.objects view)

-- | An ability of the object `source`, worded as the renderer words it, as
-- that object's text in `texts` (written) words it instead where a line of it
-- reads as the same ability (Normalise.normalise): a button or a stack entry
-- then says "This creature" where its card does.
asShown :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectId.ObjectId -> Text.Text -> Text.Text
asShown texts view source rendered = Maybe.fromMaybe rendered $ do
  o <- Map.lookup source (Wording.objects view)
  lines' <- Map.lookup source texts
  let alike name line = Normalise.normalise name line == Normalise.normalise name rendered
  List.find (\line -> any (`alike` line) (Wording.cardNames o)) lines'

-- | An ability's text, quoted, as its source's text words it (asShown).
quoted :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectView.ObjectView -> [Text.Text]
quoted texts view o =
  [ Text.pack "\"" <> maybe t (\source -> asShown texts view source t) (AbilityView.source a) <> Text.pack "\""
  | a <- Maybe.maybeToList (ObjectView.ability o),
    t <- Maybe.maybeToList (Wording.abilityText view a)
  ]

targeting :: PlayerView.PlayerView -> ObjectView.ObjectView -> [Text.Text]
targeting view o = [Text.pack "Targeting " <> Wording.listing (fmap (Wording.recipientName view) (ObjectView.targets o)) | not (null (ObjectView.targets o))]

faceDown :: ObjectView.ObjectView -> [Text.Text]
faceDown o = [Text.pack "Face down; you may look at it: " <> Text.intercalate (Text.pack " // ") (fmap CardName.unwrap (Set.toList (ProjectedCharacteristics.names f))) | f <- Maybe.maybeToList (ObjectView.face o)]
