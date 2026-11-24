-- | A seat's view of the game as a table: the turn and step, the other
-- seats across the top, the stack between, and the seat's own battlefield
-- and hand nearest it. Each permanent is a tile (Parun.Web.Card), its
-- attachments tucked behind it; graveyards and exile open on a tap.
module Parun.Web.Board where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerDesignation as PlayerDesignation
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Status as Status
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Mana as Web.Mana

-- | How each tile and each seat looks and what tapping it sends.
data Marks msg = MkMarks
  { object :: ObjectView.ObjectView -> Card.Cue msg,
    -- | A seat that may be tapped (as a target, say), and how.
    seat :: PlayerId.PlayerId -> Maybe (Card.Cue msg)
  }

-- | The turn bar: the turn and its step; the phases as a track of icons,
-- each named for assistive technology and in a tooltip, beside what is
-- given for after them and the controls given; the current one marked (and
-- marked as an extra one, CR 500.8) and those with no step still scheduled
-- passed, none marked before the first turn (CR 103.8).
turnBar :: [Html.Html msg] -> [Html.Html msg] -> PlayerView.PlayerView -> Html.Html msg
turnBar after controls board =
  let current = phaseIndex (PlayerView.phase board)
      ahead = Set.fromList (fmap phaseIndex (Foldable.toList (PlayerView.remaining board)))
      extra = PlayerView.extra board
      shown i
        | not (PlayerView.begun board) = []
        | i == current = ["now"] <> ["extra" | extra]
        | Set.member i ahead = []
        | otherwise = ["past"]
      label =
        ( if PlayerView.begun board
            then
              [ Element.el "strong" [] [] [Element.string ("Turn " <> show (PlayerView.turn board))],
                Element.el "span" ["step"] [] [Element.text (Text.pack " \x00b7 " <> moment board)]
              ]
            else [Element.el "strong" [] [] [Element.text (moment board)]]
        )
          <> [Element.el "span" ["extra"] [Element.attr "title" extraTip] [Element.string "Extra"] | extra]
          <> foldMap (pure . daytime) (PlayerView.daytime board)
   in Element.el
        "div"
        ["turn-bar", if yours board then "yours" else "theirs"]
        []
        $ [ Element.el "p" ["turn-label"] [] label,
            Element.el
              "ol"
              ["phases"]
              [Element.attr "aria-label" (Text.pack "Phases")]
              [ Element.el "li" (shown i <> cls) ([Element.attr "title" (if i == current && extra then extraTip else Text.pack full)] <> [Element.attr "aria-current" (Text.pack "step") | i == current, PlayerView.begun board]) [Icon.icon [] glyph, Element.el "span" ["sr-only"] [] [Element.string full]]
              | (i, (glyph, cls, full)) <- zip [0 :: Integer ..] phaseIcons
              ]
          ]
          <> after
          <> controls

extraTip :: Text.Text
extraTip = Text.pack "An extra phase or step this turn (CR 500.8, 500.9, 514.3a)"

-- | Whether the turn is the viewer's: no one's before the first turn begins
-- (CR 103.8).
yours :: PlayerView.PlayerView -> Bool
yours board = PlayerView.begun board && PlayerView.active board == PlayerView.viewer board

-- | Whose turn it is and its step: "Your upkeep", "Player 2's end step";
-- before the first turn, the mulligans and opening-hand actions (CR 103.5,
-- 103.6).
moment :: PlayerView.PlayerView -> Text.Text
moment board
  | not (PlayerView.begun board) = Text.pack "Before the first turn"
  | otherwise = (if yours board then Text.pack "Your " else possessive (PlayerView.active board)) <> Wording.phase (PlayerView.phase board)

-- | Each phase's icon on the track, its classes and its name (CR 500.1).
phaseIcons :: [(Icon.Icon, [String], String)]
phaseIcons =
  [ (Icon.MkBeginning, [], "Beginning phase"),
    (Icon.MkMain, [], "Precombat main phase"),
    (Icon.MkCombat, [], "Combat phase"),
    (Icon.MkMain, ["second"], "Postcombat main phase"),
    (Icon.MkEnding, [], "Ending phase")
  ]

-- | Day or night (CR 732.1), beside the turn.
daytime :: Daytime.Daytime -> Html.Html msg
daytime d =
  let (cls, label, tip) = case d of
        Daytime.Day -> ("day", "\x2600\xFE0E Day", "It is day (CR 732)")
        Daytime.Night -> ("night", "\x263E Night", "It is night (CR 732)")
   in Element.el "span" ["daytime", cls] [Element.attr "title" (Text.pack tip)] [Element.string label]

possessive :: PlayerId.PlayerId -> Text.Text
possessive pid = name pid <> Text.pack "'s "

-- | A seat's name as the table shows it.
name :: PlayerId.PlayerId -> Text.Text
name = Wording.player

phaseIndex :: Phase.Phase -> Integer
phaseIndex p = case p of
  Phase.Beginning step -> case step of
    BeginningStep.Untap -> 0
    BeginningStep.Upkeep -> 0
    BeginningStep.DrawStep -> 0
  Phase.PrecombatMain -> 1
  Phase.Combat step -> case step of
    CombatStep.BeginningOfCombat -> 2
    CombatStep.DeclareAttackers -> 2
    CombatStep.DeclareBlockers -> 2
    CombatStep.CombatDamage -> 2
    CombatStep.EndOfCombat -> 2
  Phase.PostcombatMain -> 3
  Phase.Ending step -> case step of
    EndingStep.EndStep -> 4
    EndingStep.Cleanup -> 4

-- | The table: other seats, the stack, and the seat's own. The others go in
-- turn order from the seat's, those still in the game only (CR 800.4a), side
-- by side where the screen allows; each spell on the stack says its text
-- as `texts` holds it (Card.written).
view :: Marks msg -> Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> Html.Html msg
view marks texts board =
  let (mine, others) = List.partition ((== PlayerView.viewer board) . SeatView.player) (fromViewer board)
      present = filter ((== Status.Playing) . SeatView.status) others
      across =
        [ Element.keyed
            "div"
            (["opponents"] <> ["several" | length present > 1])
            []
            [(Text.pack (show (PlayerId.unwrap (SeatView.player s))), seatArea marks board False s) | s <- present]
        | not (null present)
        ]
   in Element.el "div" ["table"] [] $
        across
          <> stackArea marks texts board
          <> fmap (seatArea marks board True) mine

-- | The seats in turn order (CR 103.1), starting from the viewer's.
fromViewer :: PlayerView.PlayerView -> [SeatView.SeatView]
fromViewer board =
  let (before, from) = break ((== PlayerView.viewer board) . SeatView.player) (PlayerView.seats board)
   in from <> before

-- | The stack, a lane between the seats taking the room they leave; said
-- empty while it is.
stackArea :: Marks msg -> Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> [Html.Html msg]
stackArea marks texts board =
  [ Element.el "div" ["stack-lane"] [] $ case PlayerView.stack board of
      [] -> [Element.el "p" ["stack-empty", "eyebrow"] [] [Element.string "Stack empty"]]
      items ->
        [ Element.el
            "section"
            ["stack"]
            [Element.attr "aria-label" (Text.pack "Stack")]
            [ Element.el "h3" ["zone-title"] [] [Element.string "Stack", Element.el "span" ["hint"] [] [Element.string " top resolves first"]],
              Element.keyed "div" ["stack-items"] [] [(key o, stackItem marks texts board o) | o <- items]
            ]
        ]
  ]

stackItem :: Marks msg -> Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> ObjectView.ObjectView -> Html.Html msg
stackItem marks texts board o =
  let cue = object marks o
      whose = foldMap (\p -> if p == PlayerView.viewer board then Text.pack "You" else name p) (ObjectView.controller o)
   in Element.el
        "button"
        (["stack-item", Card.colour o] <> Card.marks cue)
        ([Element.attr "type" (Text.pack "button"), Element.on "click" (Card.tap cue)] <> Card.peeking cue)
        ( [ Element.el "span" ["who"] [] [Element.text whose],
            Element.el "span" ["name"] [] [Element.text (Wording.nameOf board o)]
          ]
            <> [Element.el "span" ["detail"] [] (Web.Mana.rich line) | line <- Card.stackDetail texts board o]
        )

seatArea :: Marks msg -> PlayerView.PlayerView -> Bool -> SeatView.SeatView -> Html.Html msg
seatArea marks board own s =
  let pid = SeatView.player s
      permanents = filter ((== Just pid) . ObjectView.controller) (PlayerView.battlefield board)
      -- CR 702.26b: phased out, beside the permanents of the seat it phased
      -- out under; and the seat's own in the command zone (CR 408.1).
      controllerOr o = Maybe.fromMaybe (ObjectView.owner o) (ObjectView.controller o)
      phased = filter ((== pid) . controllerOr) (PlayerView.phasedOut board)
      commanded = filter ((== pid) . controllerOr) (PlayerView.command board)
      onBattlefield = Map.fromList [(ObjectView.object o, o) | o <- PlayerView.battlefield board]
      -- An attachment rides with the permanent it is attached to, when that
      -- is on the battlefield; otherwise it stands alone.
      hostOf o = do
        r <- ObjectView.attachedTo o
        oid <- attachedObject r
        if Map.member oid onBattlefield then Just oid else Nothing
      riders = Map.fromListWith (flip (<>)) [(h, [o]) | o <- PlayerView.battlefield board, Just h <- [hostOf o]]
      standing = filter (Maybe.isNothing . hostOf) permanents
      (creatures, rest) = List.partition (Wording.isA CardType.Creature) standing
      (lands, others) = List.partition (Wording.isA CardType.Land) rest
      ridersOf o = Map.findWithDefault [] (ObjectView.object o) riders
      tileOf o = Card.tile board (object marks o) o
      -- Permanents the seat cannot tell apart on the table (the same tile,
      -- nothing attached) share one tile with their count; tapping it acts
      -- on the first.
      alike a b = null (ridersOf a) && null (ridersOf b) && Html.toText (tileOf a) == Html.toText (tileOf b)
      group objects = case objects of
        [] -> []
        o : alikeRest ->
          [ ( key o,
              Element.el
                "div"
                ["group"]
                []
                ( tileOf o
                    : [Element.el "span" ["many"] [Element.attr "aria-label" (Text.pack (show (length objects) <> " of them"))] [Element.string ("\x00d7" <> show (length objects))] | not (null alikeRest)]
                      <> fmap tileOf (ridersOf o)
                )
            )
          ]
      row cls objects = [Element.keyed "div" ["row", cls] [] (concatMap group (List.groupBy alike (Wording.ordered board objects))) | not (null objects)]
      front = row "front" creatures
      -- Behind the creatures, the other permanents and the lands, sharing a
      -- line where they fit (style.css).
      back = [Element.el "div" ["back"] [] (row "others" others <> row "lands" lands) | not (null others && null lands)]
      labelled cls title objects = [Element.el "div" ["row", cls] [Element.attr "aria-label" (Text.pack title)] (Element.el "span" ["row-label"] [] [Element.string title] : fmap tileOf objects) | not (null objects)]
      extra = labelled "phased" "Phased out" phased <> labelled "command" "Command zone" commanded
      field = Element.el "div" ["battlefield"] [] (if own then front <> back <> extra else extra <> back <> front)
      empty = [Element.el "p" ["empty"] [] [Element.string "No permanents"] | null permanents, null phased, null commanded]
   in Element.el
        "section"
        ["seat", if own then "you" else "opponent"]
        [Element.attr "aria-label" (if own then Text.pack "Your side" else name pid)]
        (if own then empty <> [field, seatHead marks board own s] else [seatHead marks board own s, field] <> empty)

seatHead :: Marks msg -> PlayerView.PlayerView -> Bool -> SeatView.SeatView -> Html.Html msg
seatHead marks board own s =
  let pid = SeatView.player s
      label = if own then Text.pack "You" else name pid
      lifeBox = Element.keyed "span" ["life"] [Element.attr "title" (Text.pack "Life"), Element.attr "data-seat-life" (Text.pack (show (PlayerId.unwrap pid)))] [(Text.pack (show (SeatView.life s)), Element.el "span" ["value"] [] [Element.string (show (SeatView.life s))])]
      -- A seat that may be chosen is a button, ringed as a tile is.
      identity = case seat marks pid of
        Just cue -> Element.button (["who"] <> Card.marks cue) (Card.tap cue) [Element.el "span" ["seat-name"] [] [Element.text label], lifeBox]
        Nothing -> Element.el "div" ["who"] [] [Element.el "span" ["seat-name"] [] [Element.text label], lifeBox]
      chip cls k v tip = Element.el "span" ["count", cls] [Element.attr "title" (Text.pack tip)] ([Element.el "span" ["k"] [] [Element.string k]] <> [Element.string v | not (null v)])
      counterWord kind = Text.unpack (Wording.spaced (Text.pack (show kind)))
      -- CR 903.10a: the combat damage each commander has dealt the seat, by
      -- the commander's name up to its comma, beside its life.
      whose owner = if owner == PlayerView.viewer board then "your" else Text.unpack (name owner) <> "'s"
      commanderDamage =
        [ chip "commander-damage" (takeWhile (/= ',') (Text.unpack (CardName.unwrap commander)) <> ":") (show n <> " commander damage") ("Combat damage dealt by " <> Text.unpack (CardName.unwrap commander) <> ", " <> whose owner <> " commander (CR 903.10a)")
        | (owner, dealt) <- Map.toList (SeatView.commanderDamage s),
          (commander, n) <- Map.toList dealt
        ]
      -- The seat's designations (CR 726.1, 727.1, 701.54c, 702.179b,
      -- 702.131c, 702.195b, 309.7) and player counters (CR 122.1).
      designations =
        [Element.el "span" ["count", "monarch"] [Element.attr "title" (Text.pack "The monarch draws a card at the beginning of their end step (CR 726)")] [Element.el "span" ["k"] [] [Icon.icon [] Icon.MkCrown, Element.string " Monarch"]] | PlayerView.monarch board == Just pid]
          <> [chip "initiative" "Initiative" "" "Has the initiative: ventures into Undercity on taking it and at their upkeep (CR 727)" | PlayerView.initiative board == Just pid]
          <> [chip "ring" "The Ring" (show (SeatView.ringTemptations s)) "Times the Ring has tempted this player (CR 701.54)" | SeatView.ringTemptations s > 0]
          <> [chip "speed" "Speed" (show n) "Speed (CR 702.179)" | Just n <- [SeatView.speed s]]
          <> [chip "blessing" (designationName d) "" (designationTip d) | d <- Set.toList (SeatView.designations s)]
          <> [chip "dungeons" "Dungeons" (show (SeatView.completedDungeons s)) "Dungeons completed (CR 309.7)" | SeatView.completedDungeons s > 0]
          <> [chip (if kind == PlayerCounterKind.Poison then "poison" else "player-counter") (capitalized (counterWord kind)) (show n) (counterWord kind <> " counters") | (kind, n) <- Map.toList (SeatView.counters s), n > 0]
      -- CR 102.1 / 117.1: whose turn it is and who holds priority.
      holding =
        [chip "turn" (if own then "Your turn" else "Their turn") "" "Whose turn it is: the active player (CR 102.1)" | PlayerView.active board == pid, PlayerView.begun board]
          <> [chip "priority" (if own then "You have priority" else "Has priority") "" "Holds priority (CR 117.1)" | PlayerView.priority board == Just pid]
      pool = Mana.unwrap (SeatView.manaPool s)
      aimed = Card.targetingPlayer board pid
   in Element.el "div" (["seat-head"] <> ["targeted" | not (null aimed)]) [Element.attr "title" (Text.pack "Targeted by " <> Wording.listing aimed) | not (null aimed)] $
        [ identity,
          Element.el
            "div"
            ["counts"]
            []
            ( holding
                <> commanderDamage
                <> [ chip "zone" "Hand" (show (SeatView.handSize s)) "Cards in hand",
                     chip "zone" "Library" (show (SeatView.librarySize s)) "Cards in library"
                   ]
                <> pile marks board "Graveyard" (SeatView.graveyard s)
                <> pile marks board "Exile" (SeatView.exile s)
                <> designations
            )
        ]
          <> [Element.el "div" ["pool"] [Element.attr "title" (Text.pack "Mana pool")] (Element.el "span" ["k"] [] [Element.string "Pool"] : concatMap (Web.Mana.rich . unit) pool) | not (null pool)]
          <> [Element.el "div" ["seen"] [] (Element.el "span" ["k"] [] [Element.string "Seen in hand"] : fmap (\o -> Card.tile board (object marks o) o) (Wording.handOrder board (SeatView.hand s))) | not own, not (null (SeatView.hand s))]
          <> [Element.el "div" ["seen"] [] (Element.el "span" ["k"] [] [Element.string "Library, top first"] : fmap (\o -> Card.tile board (object marks o) o) (SeatView.library s)) | not (null (SeatView.library s))]
  where
    unit u = Text.pack "{" <> Wording.manaType (ManaUnit.manaType u) <> Text.pack "}"

designationName :: PlayerDesignation.PlayerDesignation -> String
designationName d = case d of
  PlayerDesignation.CitysBlessing -> "City's blessing"
  PlayerDesignation.EnduringStory -> "Enduring story"

designationTip :: PlayerDesignation.PlayerDesignation -> String
designationTip d = case d of
  PlayerDesignation.CitysBlessing -> "Has the city's blessing (CR 702.131)"
  PlayerDesignation.EnduringStory -> "Has an enduring story (CR 702.195)"

capitalized :: String -> String
capitalized word = case word of
  c : rest -> Char.toUpper c : rest
  [] -> []

-- | A public pile, a chip by its name and count opening to its cards;
-- nothing while it is empty.
pile :: Marks msg -> PlayerView.PlayerView -> String -> [ObjectView.ObjectView] -> [Html.Html msg]
pile marks board title objects = case objects of
  [] -> []
  _ ->
    [ Element.el
        "details"
        ["count", "zone", "pile"]
        []
        [ Element.el "summary" [] [Element.attr "title" (Text.pack title)] [Element.el "span" ["k"] [] [Element.string title], Element.string (show (length objects))],
          Element.keyed "div" ["pile-cards"] [] [(key o, Card.tile board (object marks o) o) | o <- reverse objects]
        ]
    ]

-- | The seat's hand, apart from the table so it is always in reach.
hand :: Marks msg -> PlayerView.PlayerView -> [Html.Html msg]
hand marks board = [handArea marks board s | s <- PlayerView.seats board, SeatView.player s == PlayerView.viewer board]

handArea :: Marks msg -> PlayerView.PlayerView -> SeatView.SeatView -> Html.Html msg
handArea marks board s =
  Element.el
    "section"
    ["hand"]
    [Element.attr "aria-label" (Text.pack "Your hand")]
    [ Element.el "h3" ["zone-title"] [] [Element.string ("Hand \x00b7 " <> show (length (SeatView.hand s)))],
      -- The tiles narrow, then overlap, by as much as fits them all in one
      -- row (style.css reads the count).
      Element.keyed "div" ["hand-cards"] [Element.attr "style" (Text.pack ("--n: " <> show (length (SeatView.hand s))))] [(key o, Card.tile board (object marks o) o) | o <- Wording.handOrder board (SeatView.hand s)]
    ]

key :: ObjectView.ObjectView -> Text.Text
key o = Text.pack (show (ObjectId.unwrap (ObjectView.object o)))

attachedObject :: Recipient.Recipient -> Maybe ObjectId.ObjectId
attachedObject r = case r of
  Recipient.ToCreature oid -> Just oid
  Recipient.ToPlaneswalker oid -> Just oid
  Recipient.ToBattle oid -> Just oid
  Recipient.ToObject oid -> Just oid
  Recipient.ToPlayer _ -> Nothing
  Recipient.ToPile _ -> Nothing
