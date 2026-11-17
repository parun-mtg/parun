-- | A seat's view of the game as a page: the turn, each player, the
-- battlefield by controller, the stack, the seat's hand, and what it saw
-- happen since it last decided. Each card is tapped to read it, sending what
-- `reading` makes of its id.
module Parun.Web.Board where

import qualified Data.Text as Text
import qualified Parun.Play.Render as Render
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html

view :: (ObjectId.ObjectId -> msg) -> PlayerView.PlayerView -> Html.Html msg
view reading board =
  Html.element (Text.pack "section") [] $
    [Html.element (Text.pack "p") [] [Html.text $ Text.pack ("Turn " <> show (PlayerView.turn board) <> ", ") <> Render.phase (PlayerView.phase board) <> Text.pack ". " <> Render.player (PlayerView.active board) <> Text.pack " is active."]]
      <> foldMap seat (PlayerView.seats board)
      <> zone "Stack" (PlayerView.stack board)
      <> foldMap (zone "Your hand" . Render.handOrder board . SeatView.hand) [s | s <- PlayerView.seats board, SeatView.player s == PlayerView.viewer board]
      <> zone' "Since your last decision" (Render.events board $ Render.witnessed board)
  where
    seat s =
      Html.element (Text.pack "h3") [] [Html.text $ Render.player (SeatView.player s) <> Text.pack (": " <> show (SeatView.life s) <> " life, " <> show (SeatView.handSize s) <> " in hand, " <> show (SeatView.librarySize s) <> " in library")]
        : zone "Battlefield" (Render.ordered board [o | o <- PlayerView.battlefield board, ObjectView.controller o == Just (SeatView.player s)])
          <> zone "Graveyard" (SeatView.graveyard s)
    zone title objects = listing title [Html.element (Text.pack "li") [Attribute.MkProperty (Text.pack "class") (Text.pack "card"), Attribute.MkOn (Text.pack "click") . reading $ ObjectView.object o] [Html.text $ Render.describe board o] | o <- objects]
    zone' title items = listing title [Html.element (Text.pack "li") [] [Html.text item] | item <- items]
    listing title items
      | null items = []
      | otherwise = [Html.element (Text.pack "p") [] [Html.text $ Text.pack title <> Text.pack ":"], Html.element (Text.pack "ul") [] items]
