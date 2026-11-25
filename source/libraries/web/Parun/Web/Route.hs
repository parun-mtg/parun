-- | The screen the address bar names (#9515), so a link, a reload or the
-- browser's back and forward leads to it: the page's query, which engine.js
-- hands the engine whole (Parun.Browser.Shell's opening), and which the
-- engine writes back as the page moves between screens.
module Parun.Web.Route where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Error
import Numeric.Natural (Natural)
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Uri.Query as Query
import qualified Parun.Web.Replay as Web.Replay
import qualified Text.Read as Read

data Route
  = MkHome
  | -- | The lobby.
    MkPlay
  | -- | Another browser's game joined, the host's offer in the page's fragment.
    MkJoin Stage
  | -- | The Decks screen, this deck selected if any.
    MkDecks (Maybe Text.Text)
  | -- | The builder, on this deck if any.
    MkBuild (Maybe Text.Text)
  | -- | The list of replays.
    MkReplays
  | -- | A replay at this position, watched or played on from there.
    MkReplay Way Web.Replay.Source Natural
  deriving (Eq, Show)

-- | Whether the game a host's link joins has started.
data Stage
  = -- | Still in the host's lobby, so the guest brings a deck.
    MkGathering
  | -- | Under way, so it picks up where it stopped, its decks already set.
    MkUnderWay
  deriving (Eq, Show)

-- | What a replay's route does at its position.
data Way
  = MkWatching
  | -- | Played on as this seat, else every seat (Web.Replay.MkBranch).
    MkPlayingOn (Maybe PlayerId.PlayerId)
  deriving (Eq, Show)

-- | The route's fields: a scenario by its name (scenario=, and at=), a game
-- finished here by its key (game=), played on with branch, valued by the
-- seat's number (Web.Board.name) where one seat is played; the replays as
-- replays, though the empty scenario= of an older link reads so too.
fields :: Route -> [(Text.Text, Text.Text)]
fields route =
  let named key = maybe [(Text.pack key, Text.empty)] (\name -> [(Text.pack key, name)])
   in case route of
        MkHome -> []
        MkPlay -> [(Text.pack "play", Text.empty)]
        MkJoin MkGathering -> [(Text.pack "join", Text.empty)]
        MkJoin MkUnderWay -> [(Text.pack "rejoin", Text.empty)]
        MkDecks deck -> named "decks" deck
        MkBuild deck -> named "build" deck
        MkReplays -> [(Text.pack "replays", Text.empty)]
        MkReplay way source at ->
          let which = case source of
                Web.Replay.MkScenario name -> (Text.pack "scenario", name)
                Web.Replay.MkPlayed key -> (Text.pack "game", key)
              branch = case way of
                MkWatching -> []
                MkPlayingOn seat -> [(Text.pack "branch", foldMap (Text.pack . show . (+ 1) . PlayerId.unwrap) seat)]
           in [which, (Text.pack "at", Text.pack (show at))] <> branch

-- | The query naming the route, empty for the home screen.
render :: Route -> Text.Text
render =
  Encoding.decodeLatin1
    . LazyByteString.toStrict
    . Builder.toLazyByteString
    . Query.render
    . fmap (Bifunctor.bimap Encoding.encodeUtf8 Encoding.encodeUtf8)
    . fields

-- | The route a query names, the home screen where it names none; a
-- position that does not read is the first.
parse :: Text.Text -> Route
parse query =
  let decoded = Encoding.decodeUtf8With Error.lenientDecode
      given = fmap (Bifunctor.bimap decoded decoded) . Query.parse $ Encoding.encodeUtf8 query
      field key = lookup (Text.pack key) given
      named key = fmap (\value -> if Text.null value then Nothing else Just value) (field key)
      at = Maybe.fromMaybe (0 :: Natural) (field "at" >>= Read.readMaybe . Text.unpack)
      seatOf value = PlayerId.MkPlayerId . subtract 1 <$> Monad.mfilter (> 0) (Read.readMaybe (Text.unpack value))
      way = maybe MkWatching (MkPlayingOn . seatOf) (field "branch")
      replay source = MkReplay way source at
   in Maybe.fromMaybe MkHome $
        Foldable.asum
          [ named "scenario" >>= fmap (replay . Web.Replay.MkScenario),
            MkReplays <$ named "scenario",
            MkReplays <$ field "replays",
            named "game" >>= fmap (replay . Web.Replay.MkPlayed),
            MkPlay <$ field "play",
            MkJoin MkGathering <$ field "join",
            MkJoin MkUnderWay <$ field "rejoin",
            MkDecks <$> named "decks",
            MkBuild <$> named "build"
          ]
