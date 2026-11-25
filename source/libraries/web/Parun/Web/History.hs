{-# LANGUAGE ApplicativeDo #-}

-- | The games this browser finished (#9431): each kept as the summary its
-- lists show and the scenario file it replays from (Command.rendered), the
-- newest 'cap' of them, as rows for the home screen and the replays.
module Parun.Web.History where

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Color as Codec.Color
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

-- | A finished game as its lists show it.
data Game = MkGame
  { -- | When it ended, in milliseconds since the Unix epoch: its key too.
    ended :: Integer,
    -- | How many minutes the page's clock ran ahead of UTC then.
    offset :: Integer,
    -- | The build that played it (Parun.Web.Link), whose rules replay it.
    build :: Text.Text,
    -- | How it ended for the player: won, lost or drawn, or who won a
    -- shared screen's game.
    verdict :: Ending.Verdict,
    -- | The turn it ended in.
    turns :: Natural,
    -- | The result and how each player left (Ending.said).
    said :: [Text.Text],
    -- | Every seat, in seat order.
    seats :: [Seat]
  }
  deriving (Eq, Show)

-- | A seat of a finished game.
data Seat = MkSeat
  { who :: Who,
    -- | Its deck as the match-up names it (DeckBrowser.label).
    deck :: Text.Text,
    -- | Its deck's name (Lobby.deckName), where this browser held it, to
    -- play it again.
    deckName :: Maybe Text.Text,
    colours :: Set.Set Color.Color
  }
  deriving (Eq, Show)

-- | Who played a seat.
data Who
  = -- | The player this page plays.
    MkYou
  | MkBot
  | -- | A player invited from another browser, one the player also played
    -- on this screen, or the host who invited you.
    MkPerson
  | -- | Another player on this screen, handed it before each question
    -- (Link.MkHandedOver).
    MkHere
  deriving (Bounded, Enum, Eq, Show)

-- | The game's key: when it ended.
key :: Game -> Text.Text
key = Text.pack . show . ended

codec :: Codec.Codec Game
codec = Fields.object $ do
  ended' <- Fields.required "ended" Common.integer ended
  offset' <- Fields.required "offset" Common.integer offset
  build' <- Fields.required "build" Common.text build
  verdict' <- Fields.required "verdict" Ending.verdictCodec verdict
  turns' <- Fields.required "turns" Common.natural turns
  said' <- Fields.required "said" (Common.list Common.text) said
  seats' <- Fields.required "seats" (Common.list seatCodec) seats
  pure (MkGame ended' offset' build' verdict' turns' said' seats')

seatCodec :: Codec.Codec Seat
seatCodec = Fields.object $ do
  who' <- Fields.required "who" Arm.enum who
  deck' <- Fields.required "deck" Common.text deck
  name' <- Fields.required "name" (Common.maybe Common.text) deckName
  colours' <- Fields.required "colours" (Common.set Codec.Color.codec) colours
  pure (MkSeat who' deck' name' colours')

-- | A game's summary as the page keeps it: one line.
encode :: Game -> Text.Text
encode = Common.render . Codec.encode codec

-- | A kept summary, or why it does not read.
decode :: Text.Text -> Either Text.Text Game
decode kept = Common.parse kept >>= Codec.decode codec

-- | A kept game's scenario file, decoded, or why it does not.
scenarioOf :: Text.Text -> Either Text.Text Scenario.Scenario
scenarioOf file = Common.parse file >>= Codec.decode Codec.Scenario.codec

-- | How many games are kept: a new one past it drops the oldest.
cap :: Int
cap = 50

-- | The games kept once this one is added, newest first, and those it drops
-- past the 'cap'.
adding :: Game -> [Game] -> ([Game], [Game])
adding game games = splitAt cap (newestFirst (game : filter ((/= key game) . key) games))

-- | When the player last played each deck this browser holds, by its name
-- (deckName): when the newest game they played it in ended.
lastPlayed :: [Game] -> Map.Map Text.Text Integer
lastPlayed games = Map.fromListWith max [(name, ended game) | game <- games, MkSeat MkYou _ (Just name) _ <- seats game]

newestFirst :: [Game] -> [Game]
newestFirst = List.sortOn (Ord.Down . ended)

-- | The games kept, read back as keys and summaries: those that read,
-- newest first, up to the 'cap', and the keys of the rest, to forget: a
-- summary that does not read, and any game past the cap, which another tab
-- may have kept meanwhile.
loaded :: [(Text.Text, Text.Text)] -> ([Game], [Text.Text])
loaded kept =
  let (unread, games) = Either.partitionEithers [either (const (Left k)) Right (decode summary) | (k, summary) <- kept]
      (keeping, past) = splitAt cap (newestFirst games)
   in (keeping, unread <> fmap key past)

-- | What to say of a game a build other than the one running played: its
-- answers may not replay the same game under rules or cards since changed.
drift :: Text.Text -> Game -> Maybe Text.Text
drift running game
  | build game == running = Nothing
  | otherwise = Just (Text.pack "Recorded on an older version; it may not replay exactly.")

-- | The game a play keeps, with its scenario file, ended `at` milliseconds
-- since the epoch, the clock `ahead` minutes ahead of UTC, on `build`,
-- between these seats, in the turn its trail last sampled (Ending.sampled):
-- one with a result, which a concession gives; none for one given up from
-- the home screen or stopped short.
recordOf :: Integer -> Integer -> Text.Text -> [Seat] -> Ending.Played -> Maybe (Game, Text.Text)
recordOf at ahead build' seats' played = case played of
  Ending.MkAbandoned -> Nothing
  Ending.MkFinished ending
    | Ending.verdict ending == Ending.MkStopped -> Nothing
    | otherwise ->
        -- A trail is empty only where no board was ever seen: no turn began.
        Just (MkGame at ahead build' (Ending.verdict ending) (maybe 0 fst (Maybe.listToMaybe (reverse (Ending.trail ending)))) (Ending.said ending) seats', Ending.scenario ending)

-- | The label the game's scenario gives the player's seat (Command.labels).
viewerLabel :: Game -> Maybe Text.Text
viewerLabel game = do
  at <- List.findIndex ((== MkYou) . who) (seats game)
  fmap Label.unwrap (Map.lookup (Command.idOf (Int.toNaturalSaturating at + 1)) (Command.labels (Natural.length (seats game))))

-- | What the replay calls each seat, by the label the game's scenario gives
-- it (Command.labels): what the game called it, "You" for the player's but
-- in a shared game, and "Player 2" by seat for any other, whoever played it.
seatNames :: Game -> Map.Map Text.Text Text.Text
seatNames game =
  let count = Natural.length (seats game)
      named (n, seat) = (Label.unwrap <$> Map.lookup (Command.idOf n) (Command.labels count), if who seat == MkYou && not (shared game) then Text.pack "You" else Wording.player (Command.idOf n))
   in Map.fromList [(label, name) | (Just label, name) <- fmap named (zip [1 :: Natural ..] (seats game))]

-- | Whether another player played on this screen, so the game takes no
-- seat's side.
shared :: Game -> Bool
shared = any ((== MkHere) . who) . seats

-- | Who played a seat, as a word.
whoName :: Who -> String
whoName w = case w of
  MkYou -> "You"
  MkBot -> "Bot"
  MkPerson -> "Player"
  MkHere -> "Player"

-- | How the game ended for the player, as a word, or who won a shared
-- screen's game.
outcome :: Game -> String
outcome game = case verdict game of
  Ending.MkWon -> "Won"
  Ending.MkLost -> "Lost"
  Ending.MkDrawn -> "Draw"
  Ending.MkWonBy pid -> Text.unpack (Wording.player pid) <> " won"
  Ending.MkStopped -> "Stopped"

-- | The decks played, "Ajani (M19) vs Liliana (M19)".
matchup :: Game -> Text.Text
matchup = Text.intercalate (Text.pack " vs ") . fmap deck . seats

-- | Who played, "You vs Bot"; in a shared game, each person by seat,
-- "Player 1 vs Player 2".
players :: Game -> Text.Text
players game = Text.intercalate (Text.pack " vs ") (zipWith named [1 ..] (seats game))
  where
    named n seat
      | shared game && who seat /= MkBot = Wording.player (Command.idOf n)
      | otherwise = Text.pack (whoName (who seat))

-- | When the game ended on the page's clock then: "21 Nov 2026, 14:05".
date :: Game -> String
date game =
  let local = div (ended game) 60000 + offset game
      (days, minutes) = divMod local 1440
      (year, month, day) = civil days
      months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
      twoDigits n = (if n < 10 then "0" else "") <> show n
   in show day <> " " <> Maybe.fromMaybe "" (lookup month (zip [1 ..] months)) <> " " <> show year <> ", " <> twoDigits (div minutes 60) <> ":" <> twoDigits (mod minutes 60)

-- | The name a downloaded game is saved under, by when it ended on the
-- page's clock then, to the second: "parun-game-2026-11-21-140500.json".
fileName :: Game -> Text.Text
fileName game =
  let (days, seconds) = divMod (div (ended game) 1000 + offset game * 60) 86400
      (year, month, day) = civil days
      twoDigits n = (if n < 10 then "0" else "") <> show n
   in Text.pack ("parun-game-" <> show year <> "-" <> twoDigits month <> "-" <> twoDigits day <> "-" <> twoDigits (div seconds 3600) <> twoDigits (mod (div seconds 60) 60) <> twoDigits (mod seconds 60) <> ".json")

-- | The proleptic Gregorian year, month and day so many days after 1 January
-- 1970 (Howard Hinnant's civil_from_days).
civil :: Integer -> (Integer, Integer, Integer)
civil days =
  let z = days + 719468
      era = div z 146097
      doe = z - era * 146097
      yoe = div (doe - div doe 1460 + div doe 36524 - div doe 146096) 365
      doy = doe - (365 * yoe + div yoe 4 - div yoe 100)
      mp = div (5 * doy + 2) 153
      day = doy - div (153 * mp + 2) 5 + 1
      month = if mp < 10 then mp + 3 else mp - 9
   in (yoe + era * 400 + (if month <= 2 then 1 else 0), month, day)

-- | A game as a row of a list: how it ended, the decks and their colours,
-- who played, its last turn and its date, then `actions`.
row :: Game -> [Html.Html msg] -> Html.Html msg
row game actions =
  Element.el
    "li"
    ["game-row"]
    []
    [ Element.el "span" ["result", tone] [] [Element.string (outcome game)],
      Element.el
        "div"
        ["game-row-text"]
        []
        [ Element.el "span" ["game-row-matchup"] [] (List.intersperse (Element.el "span" ["versus"] [] [Element.string " vs "]) (fmap seatOf (seats game))),
          Element.el "span" ["muted", "game-row-meta"] [] [Element.text (players game), Element.string (" \x00b7 Turn " <> show (turns game) <> " \x00b7 " <> date game)]
        ],
      Element.el "div" ["game-row-actions"] [] actions
    ]
  where
    tone = case verdict game of
      Ending.MkWon -> "won"
      Ending.MkLost -> "lost"
      _ -> "drawn"
    seatOf seat = Element.el "span" ["game-row-deck"] [] (Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Card.letterUpper c)) | c <- Foldable.toList (colours seat)] : [Element.text (deck seat)])
