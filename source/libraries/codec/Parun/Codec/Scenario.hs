{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Scenario where

import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.Board as Board
import qualified Parun.Codec.Check as Check
import qualified Parun.Codec.Entrant as Entrant
import qualified Parun.Codec.Format as Format
import qualified Parun.Codec.GameSettings as GameSettings
import qualified Parun.Codec.Label as Label
import qualified Parun.Codec.Timed as Timed
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.GameSettings as GameSettings.Type
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.Start as Start

codec :: Codec.Codec Scenario.Scenario
codec = Fields.object $ do
  description <- Fields.required "description" Common.text Scenario.description
  start <- startFields
  timeline <- Fields.defaulted "timeline" Seq.empty (Common.seq Timed.codec) Scenario.timeline
  final <- Fields.defaulted "final" Seq.empty (Common.seq Check.codec) Scenario.final
  pure
    Scenario.MkScenario
      { Scenario.description = description,
        Scenario.start = start,
        Scenario.timeline = timeline,
        Scenario.final = final
      }

-- | A board under "board", or the decks of a new game under "game"; exactly
-- one. A game's format, if any, is under "format", the other options it was
-- played with (CR 800.2), where not none, under "settings", and its starting
-- player (CR 103.1), where not the first seat, under "first"; a board refuses
-- all three, and settings refuse a format of their own.
startFields :: Fields.Fields Scenario.Scenario Start.Start
startFields =
  let board = Fields.defaulted "board" Nothing (Common.maybe Board.codec) (boardOf . Scenario.start)
      game = Fields.defaulted "game" Nothing (Common.maybe (Common.nonEmpty Entrant.codec)) (gameOf . Scenario.start)
      format = Fields.defaulted "format" Nothing (Common.maybe Format.codec) (formatOf . Scenario.start)
      settings = Fields.defaulted "settings" Nothing (Common.maybe GameSettings.codec) (settingsOf . Scenario.start)
      first = Fields.defaulted "first" Nothing (Common.maybe Label.codec) (firstOf . Scenario.start)
      every = (,,,,) <$> board <*> game <*> format <*> settings <*> first
   in every
        { Fields.decode = \pairs -> do
            found <- Fields.decode every pairs
            case found of
              (Just b, Nothing, Nothing, Nothing, Nothing) -> Right (Start.FromBoard b)
              (Just _, Nothing, Just _, _, _) -> Left (Text.pack "a format is a game's, not a board's")
              (Just _, Nothing, _, Just _, _) -> Left (Text.pack "settings are a game's, not a board's")
              (Just _, Nothing, _, _, Just _) -> Left (Text.pack "a starting player is a game's, not a board's")
              (Nothing, Just g, f, o, s)
                | any (Maybe.isJust . GameSettings.Type.format) o -> Left (Text.pack "a game's format is under \"format\", not its settings")
                | otherwise -> Right (Start.FromDecks (Maybe.fromMaybe GameSettings.Type.plain o) {GameSettings.Type.format = f} g s)
              _ -> Left (Text.pack "expected exactly one of board and game")
        }
  where
    boardOf start = case start of
      Start.FromBoard b -> Just b
      Start.FromDecks {} -> Nothing
    gameOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks _ g _ -> Just g
    formatOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks o _ _ -> GameSettings.Type.format o
    -- A game's options but its format, which "format" holds; none where they
    -- are plain's.
    settingsOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks o _ _ ->
        let rest = o {GameSettings.Type.format = Nothing}
         in if rest == GameSettings.Type.plain then Nothing else Just rest
    firstOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks _ _ s -> s
