-- | The web client's game log: what a seat saw happen, as Wording.events
-- tells it, headed by each turn as it began, the bookkeeping the board
-- already shows set apart as minor.
module Parun.Web.Log where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

-- | A line of the log.
data Entry
  = -- | A turn began: its number, and whose it is.
    MkTurn Natural PlayerId.PlayerId
  | -- | Something happened; minor when the board already shows it.
    MkEvent Weight Text.Text
  deriving (Eq, Show)

data Weight
  = MkMajor
  | MkMinor
  deriving (Eq, Show)

-- | The entries of the view the seat has not read yet, and its marks (how
-- far it has read each game's log: the turn, and how many of that turn's
-- entries) moved past them: the rest of last turn's, where the seat last read during it,
-- then a heading for this turn and its entries.
unseen :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural) -> PlayerView.PlayerView -> (Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural), [Entry])
unseen seen view =
  let mark = (PlayerView.viewer view, PlayerView.game view)
      turn = PlayerView.turn view
      entries = PlayerView.witnessed view
      heading = MkTurn turn (PlayerView.active view)
      missed = case Map.lookup mark seen of
        Just (t, n)
          | t == turn -> told view (List.genericDrop n entries)
          | t + 1 == turn -> told view (List.genericDrop n (PlayerView.witnessedLastTurn view)) <> [heading] <> told view entries
        _ -> heading : told view entries
   in (Map.insert mark (turn, Natural.length entries) seen, missed)

-- | The entries as Wording.events tells them, each run of minor or major
-- ones told together so draws still group.
told :: PlayerView.PlayerView -> [LoggedEvent.LoggedEvent] -> [Entry]
told view logged =
  let tell run = case run of
        first : _ -> fmap (MkEvent (minor first)) (Wording.events (Just (PlayerView.viewer view)) view run)
        [] -> []
   in concatMap tell (List.groupBy (\a b -> minor a == minor b) logged)

-- | Minor: a resolving spell's move off the stack, which the board shows as
-- it lands.
minor :: LoggedEvent.LoggedEvent -> Weight
minor logged = case LoggedEvent.event logged of
  GameEvent.Moved m | ZoneChange.from (Moved.change m) == Zone.Stack -> MkMinor
  _ -> MkMajor

-- | The entries' sentences, for the end of the game and the newest-first
-- summary; headings as "Turn 5: Player 2".
sentence :: PlayerId.PlayerId -> Entry -> Text.Text
sentence viewer entry = case entry of
  MkTurn n active -> Text.pack ("Turn " <> show n <> ": ") <> (if active == viewer then Text.pack "your turn" else Wording.player active <> Text.pack "'s turn")
  MkEvent _ t -> t

-- | The entries less each heading with no event under it: a turn nothing
-- has happened in yet, or one whose events a cut left out.
headed :: (x -> Entry) -> [x] -> [x]
headed entry xs = [x | (x, next) <- zip xs (fmap Just (drop 1 xs) <> [Nothing]), not (isTurn (entry x)) || maybe False (not . isTurn . entry) next]

-- | Whether an entry is a heading.
isTurn :: Entry -> Bool
isTurn entry = case entry of
  MkTurn _ _ -> True
  MkEvent _ _ -> False

-- | An entry as a list item; where `linked` names the objects the seat can
-- read and what tapping one sends, each name in the entry becomes a link
-- to that object's sheet.
item :: PlayerId.PlayerId -> Maybe (Map.Map Text.Text ObjectId.ObjectId, ObjectId.ObjectId -> msg) -> Bool -> Entry -> Html.Html msg
item viewer linked fresh entry = case entry of
  MkTurn _ _ -> Element.el "li" ["turn"] [] [Element.text (sentence viewer entry)]
  MkEvent w t -> Element.el "li" (["fresh" | fresh] <> ["minor" | w == MkMinor]) [] (maybe (Mana.rich t) (\(known, reading) -> linking known reading t) linked)

-- | Entries in the order given, each beside its place in the log, as a list
-- per turn under that turn's heading ("Turn 5 · You"), a turn with no
-- entries left out (headed); `linked` says by an
-- entry's position here what its names link to (item), `lit` by its place
-- whether it is fresh.
grouped :: PlayerId.PlayerId -> (Int -> Maybe (Map.Map Text.Text ObjectId.ObjectId, ObjectId.ObjectId -> msg)) -> (Int -> Bool) -> Seq.Seq (Int, Entry) -> Html.Html msg
grouped viewer linked lit entries =
  let numbered = headed (\(_, _, e) -> e) (Foldable.toList (Seq.mapWithIndex (\j (i, e) -> (j, i, e)) entries))
      -- Runs each starting at a heading, the first perhaps without one.
      runs = List.groupBy (\_ (_, _, e) -> not (isTurn e)) numbered
      heading e = case e of
        MkTurn n active -> [Element.el "h3" ["log-turn-heading"] [] [Element.text (Text.pack ("Turn " <> show n <> " \x00b7 ") <> (if active == viewer then Text.pack "You" else Wording.player active))]]
        MkEvent _ _ -> []
      run items = case items of
        (_, i, first) : _ ->
          [ ( Text.pack (show i),
              Element.el "section" ["log-turn"] [] $
                heading first
                  <> [Element.keyed "ul" ["log-list"] [] [(Text.pack (show i'), item viewer (linked j) (lit i') e) | (j, i', e) <- items, not (isTurn e)]]
            )
          ]
        [] -> []
   in Element.keyed "div" ["log-turns"] [] (concatMap run runs)

-- | The text, each name it holds a link (the longest where two start
-- together), the rest drawn as Mana.rich draws it.
linking :: Map.Map Text.Text ObjectId.ObjectId -> (ObjectId.ObjectId -> msg) -> Text.Text -> [Html.Html msg]
linking known reading t =
  let found = [(Text.length before, name, oid) | (name, oid) <- Map.toList known, not (Text.null name), let (before, after) = Text.breakOn name t, not (Text.null after)]
   in case List.sortOn (\(at, name, _) -> (at, negate (Text.length name))) found of
        [] -> Mana.rich t
        (at, name, oid) : _ ->
          Mana.rich (Text.take at t)
            <> [Element.el "button" ["log-card"] [Element.attr "type" (Text.pack "button"), Element.on "click" (reading oid)] [Element.text name]]
            <> linking known reading (Text.drop (at + Text.length name) t)

-- | The names of the objects the view shows, each to one object, a card on
-- the battlefield or the stack before one elsewhere, and any of them before
-- one only a witnessed move, look or reveal showed (Wording.recalled).
linkable :: PlayerView.PlayerView -> Map.Map Text.Text ObjectId.ObjectId
linkable view =
  let shown = PlayerView.battlefield view <> PlayerView.stack view <> concatMap (\seat -> SeatView.hand seat <> SeatView.graveyard seat <> SeatView.exile seat) (PlayerView.seats view)
      remembered = [(name, oid) | oid <- Map.keys (Wording.snapshots view), Map.notMember oid (Wording.objects view), Just name <- [Wording.knownName view oid]]
   in Map.fromListWith (\_ first -> first) ([(Wording.cardName o, ObjectView.object o) | o <- shown, Maybe.isNothing (ObjectView.ability o), Maybe.isJust (ObjectView.characteristics o)] <> remembered)

-- | Entries, each beside something (its place), newest turn first, each
-- turn's heading above its entries and those newest first.
newestFirst :: Seq.Seq (a, Entry) -> Seq.Seq (a, Entry)
newestFirst entries =
  let turns = Foldable.foldl' (\acc x -> if isTurn (snd x) then Seq.singleton x Seq.<| acc else case Seq.viewl acc of current Seq.:< rest -> (current Seq.|> x) Seq.<| rest; Seq.EmptyL -> Seq.singleton (Seq.singleton x)) Seq.empty entries
      ordered turn = case Seq.viewl turn of
        first Seq.:< rest | isTurn (snd first) -> first Seq.<| Seq.reverse rest
        _ -> Seq.reverse turn
   in Foldable.fold (fmap ordered turns)

-- | The events among the entries, oldest first.
events :: Seq.Seq Entry -> [Text.Text]
events = Foldable.foldr (\entry rest -> case entry of MkEvent _ t -> t : rest; MkTurn _ _ -> rest) []
