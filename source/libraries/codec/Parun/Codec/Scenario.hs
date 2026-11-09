{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Scenario where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.Board as Board
import qualified Parun.Codec.Check as Check
import qualified Parun.Codec.Entrant as Entrant
import qualified Parun.Codec.Format as Format
import qualified Parun.Codec.Timed as Timed
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
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
-- one. A game's format, if any, is under "format", which a board refuses.
startFields :: Fields.Fields Scenario.Scenario Start.Start
startFields =
  let board = Fields.defaulted "board" Nothing (Common.maybe Board.codec) (boardOf . Scenario.start)
      game = Fields.defaulted "game" Nothing (Common.maybe (Common.nonEmpty Entrant.codec)) (gameOf . Scenario.start)
      format = Fields.defaulted "format" Nothing (Common.maybe Format.codec) (formatOf . Scenario.start)
      every = (,,) <$> board <*> game <*> format
   in every
        { Fields.decode = \pairs -> do
            found <- Fields.decode every pairs
            case found of
              (Just b, Nothing, Nothing) -> Right (Start.FromBoard b)
              (Just _, Nothing, Just _) -> Left (Text.pack "a format is a game's, not a board's")
              (Nothing, Just g, f) -> Right (Start.FromDecks f g)
              _ -> Left (Text.pack "expected exactly one of board and game")
        }
  where
    boardOf start = case start of
      Start.FromBoard b -> Just b
      Start.FromDecks _ _ -> Nothing
    gameOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks _ g -> Just g
    formatOf start = case start of
      Start.FromBoard _ -> Nothing
      Start.FromDecks f _ -> f
