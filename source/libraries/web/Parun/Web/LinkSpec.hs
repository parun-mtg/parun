module Parun.Web.LinkSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Link as Link

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Link" $ do
  Spec.describe s "encode" $ do
    Spec.it s "reads back every message as it was sent" $ do
      let request = Request.MkRequest Nothing (Question.MkQuestion (PlayerId.MkPlayerId 1) (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]) Nothing Nothing) (Just (Text.pack "expected Play or Draw")) 0
          messages =
            [ Link.MkHello (Text.pack "saved/elves") (Text.pack "Format: constructed\n24 Forest\n36 Llanowar Elves\n"),
              Link.MkMatchup [Link.MkPlace (Text.pack "Host") (Text.pack "Elves (Decks)") (Just True), Link.MkPlace (Text.pack "You") (Text.pack "Choosing a deck") (Just False), Link.MkPlace (Text.pack "Bot 2") (Text.pack "Random deck") Nothing],
              Link.MkReady True,
              Link.MkReady False,
              Link.MkStart (PlayerId.MkPlayerId 2),
              Link.MkAsk request,
              Link.MkDeciding (PlayerId.MkPlayerId 2),
              Link.MkAnswer (R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]),
              Link.MkEnd Ending.MkLost [Text.pack "Player 1 wins."] (Text.pack "{}\n"),
              Link.MkEnd Ending.MkWon [] Text.empty,
              Link.MkEnd Ending.MkDrawn [] Text.empty,
              Link.MkEnd Ending.MkStopped [Text.pack "The game stopped: a player's connection closed."] Text.empty
            ]
          build = Text.pack "parun-0123456789abcdef.wasm"
      Spec.assertEq s (fmap (Link.decode build . Link.encode build) messages) (fmap Right messages)

    Spec.it s "puts each message on one line" $ do
      Spec.assertBool s (not (Text.any (== '\n') (Link.encode (Text.pack "b") (Link.MkHello (Text.pack "x") (Text.pack "1 Forest\n1 Island\n"))))) "one line"

  Spec.describe s "decode" $ do
    Spec.it s "refuses a message from another build" $ do
      Spec.assertBool s (either (Text.isInfixOf (Text.pack "another build")) (const False) (Link.decode (Text.pack "new") (Link.encode (Text.pack "old") (Link.MkReady True)))) "refused"

    Spec.it s "refuses a type it does not know" $ do
      Spec.assertEq s (Link.decode (Text.pack "b") (Text.pack "{\"type\":\"shout\",\"build\":\"b\"}")) (Left (Text.pack "not a message: shout"))

  Spec.describe s "saved" $ do
    Spec.it s "reads back a kept game, a reply added as a line" $ do
      let game = Link.MkSaved key (NonEmpty.fromList [(Text.pack "sets/a/one", Text.pack "Format: constructed\n24 Forest\n"), (Text.pack "Bot", Text.pack "20 Island\n"), (Text.pack "Here", Text.pack "20 Swamp\n"), (Text.pack "Yours", Text.pack "20 Plains\n")]) [0] (Map.fromList [(1, Link.MkHandedOver), (2, Link.MkYours)]) Difficulty.MkEasy Persona.MkAggro [R.Text (Text.pack "Keep")]
          more = R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]
          build = Text.pack "b"
      Spec.assertEq s (Link.unsaved build (Link.saved build game <> Text.pack "\n" <> Link.savedReply more)) (Right game {Link.replies = Link.replies game <> [more]})

    Spec.it s "refuses a game another build played" $ do
      let game = Link.MkSaved key (NonEmpty.fromList [(Text.pack "one", Text.pack "20 Forest\n")]) [] Map.empty Difficulty.MkHard Persona.MkBalanced []
      Spec.assertBool s (either (Text.isInfixOf (Text.pack "another build")) (const False) (Link.unsaved (Text.pack "new") (Link.saved (Text.pack "old") game))) "refused"
  where
    key = Key.Type.UnsafeKey (Seq.fromList [1, 2, 3, 4, 5, 6, 7, 0xdeadbeef])
