-- | Reading a request's offer: the options it lists and the names a client
-- shows them by, from the seat's view. Shared by every client of the line
-- protocol; how a client asks and reads an answer is its own.
module Parun.Play.Offer where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.Cost as Codec.Cost
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Oracle.Cost as Oracle.Cost
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Reply as R
import qualified Parun.Types.SeatView as SeatView
import qualified Text.Read as Read

textOf :: R.Reply -> Maybe Text.Text
textOf reply = case reply of
  R.Text t -> Just t
  _ -> Nothing

objectOf :: R.Reply -> Maybe ObjectId.ObjectId
objectOf reply = do
  t <- textOf reply
  digits <- Text.stripPrefix (Text.pack "$") t
  fmap ObjectId.MkObjectId (Read.readMaybe (Text.unpack digits))

-- | An object the offer spells @"$7"@, named as the view shows it.
objectName :: PlayerView.PlayerView -> R.Reply -> Text.Text
objectName view reply = maybe (Text.pack "something") (Render.named view) (objectOf reply)

-- | An object an offer lists, told apart from others of its name.
listedName :: PlayerView.PlayerView -> R.Reply -> Text.Text
listedName view reply = maybe (Text.pack "something") (Render.choosable view) (objectOf reply)

-- | A seat a listed offer names by its bare id, or declining as None.
playerName :: R.Reply -> Text.Text
playerName reply = case reply of
  R.Text x -> maybe x (Render.player . PlayerId.MkPlayerId) (Read.readMaybe (Text.unpack x))
  _ -> Text.pack "None"

recipientName :: PlayerView.PlayerView -> R.Reply -> Text.Text
recipientName view reply = case reply of
  R.Array [R.Text tag, R.Text x]
    | tag == Text.pack "Player" -> maybe x (Render.player . PlayerId.MkPlayerId) (Read.readMaybe (Text.unpack x))
    | otherwise -> listedName view (R.Text x)
  R.Array [R.Text tag, draw] | tag == Text.pack "Pile" -> pileName draw
  _ -> Text.pack "something"

-- | CR 406.4's pile, named by its kind alone since its cards are hidden; a
-- draw past the first out of it is named by its ordinal.
pileName :: R.Reply -> Text.Text
pileName draw =
  let foretold = (Protocol.field "pile" draw >>= Protocol.field "type" >>= textOf) == Just (Text.pack "OfForetold")
      nth = maybe Text.empty (\n -> if n > 1 then Text.pack (", card " <> show n) else Text.empty) (Protocol.field "ordinal" draw >>= Protocol.naturalOf)
   in Text.pack (if foretold then "A foretold card" else "A face-down exiled pile") <> nth

-- | A word an offer spells an option with: a yes or no as one
-- (Parun.Types.OptionalDecision), a tag joining words (BuysBack) as words, and
-- anything else, a card's name say, as itself.
tagName :: Text.Text -> Text.Text
tagName t
  | t == Text.pack "Exercises" = Text.pack "Yes"
  | t == Text.pack "Declines" = Text.pack "No"
  | Text.all (\c -> Char.isAsciiUpper c || Char.isAsciiLower c) t,
    Text.any Char.isAsciiUpper (Text.drop 1 t) =
      case Text.uncons (Render.spaced t) of
        Just (c, rest) -> Text.cons (Char.toUpper c) rest
        Nothing -> t
  | otherwise = t

-- | An option of a listed offer: an object as listedName names it, a
-- recipient or attack target as recipientName does, declining as None, an
-- empty set (CR 700.3d's empty pile) as Nothing, and a word or number as
-- itself.
optionName :: PlayerView.PlayerView -> R.Reply -> Text.Text
optionName view reply = case reply of
  R.Null -> Text.pack "None"
  R.Array [] -> Text.pack "Nothing"
  R.Number n -> Text.pack (show n)
  R.Text t
    | Maybe.isJust (objectOf reply) -> listedName view reply
    | otherwise -> tagName t
  R.Array [R.Text tag, _] | tag `elem` fmap Text.pack ["Player", "Planeswalker", "Battle"] -> recipientName view reply
  R.Array xs -> Text.intercalate (Text.pack ", ") (fmap (optionName view) xs)
  -- A damage event (Parun.Play.PromptOffer.damageEvent Protocol.namer).
  R.Object _
    | Just source <- Protocol.field "source" reply,
      Just target <- Protocol.field "target" reply,
      Just amount <- Protocol.field "amount" reply >>= Protocol.naturalOf ->
        Text.concat [objectName view source, Text.pack (" deals " <> show amount <> " damage to "), recipientName view target]
  _ -> Common.render (Codec.Reply.toValue reply)

-- | A ChooseCost candidate's cost as Oracle text prints it (Oracle.Cost.cost),
-- or the offer itself where it does not print.
candidateName :: Text.Text -> R.Reply -> Text.Text
candidateName self candidate =
  Maybe.fromMaybe (Common.render (Codec.Reply.toValue candidate)) (decoded (Codec.Cost.codec Codec.Keyword.codec) candidate >>= Maybe.listToMaybe . Oracle.Cost.cost self (Oracle.Keyword.keywordWord self))

decoded :: Codec.Codec a -> R.Reply -> Maybe a
decoded codec = either (const Nothing) Just . Codec.decode codec . Codec.Reply.toValue

-- | An action as the offer names it.
actionName :: PlayerView.PlayerView -> R.Reply -> Text.Text
actionName view action =
  let object = fmap (objectName view) (Protocol.field "object" action)
      name = Protocol.field "name" action >>= textOf
      subject = Maybe.fromMaybe Text.empty (Monad.mplus name object)
   in case maybe "" Text.unpack (Protocol.field "action" action >>= textOf) of
        "Pass" -> Text.pack "Pass"
        "PlayLand" -> Text.pack "Play " <> subject
        "Cast" -> Text.pack "Cast " <> subject
        "Activate" -> maybe (Text.pack "Activate an ability of ") (\i -> Text.pack ("Activate ability " <> show (i + 1) <> " of ")) (Protocol.field "ability" action >>= Protocol.naturalOf) <> subject
        "ActivateManaAbility" -> Text.pack "Activate a mana ability of " <> subject
        -- CR 701.40c: which procedure, where two may turn one permanent up.
        "TurnFaceUp" -> Text.pack "Turn face up " <> subject <> foldMap (\p -> Text.pack " (" <> Render.spaced p <> Text.pack ")") (Protocol.field "procedure" action >>= textOf)
        -- CR 116.2d: which of the permanent's abilities is ignored.
        "Ignore" -> Text.pack "Ignore " <> subject <> foldMap (Text.pack "'s " <>) (Protocol.field "abilityName" action >>= textOf)
        -- CR 702.170a: which plot cost is paid.
        "Plot" -> Text.pack "Plot " <> subject <> foldMap (\c -> Text.pack " for " <> candidateName subject c) (Protocol.field "cost" action)
        kind -> Render.spaced (Text.pack kind) <> maybe Text.empty (Text.pack " " <>) object

-- | A list's options as shown, each beside its index in the list: an option
-- is dropped when an earlier one plays, casts or plots a card in the seat's
-- hand that the view shows exactly as it shows this one's, bar its id, and
-- the two offers match bar the object. Other actions never merge, and neither
-- do cards elsewhere, which a permission may tell apart.
distinct :: PlayerView.PlayerView -> [R.Reply] -> [(Integer, R.Reply)]
distinct view = reverse . snd . List.foldl' keep ([], []) . zip [0 ..]
  where
    keep (keys, kept) (i, option) = case key option of
      Just k | k `elem` keys -> (keys, kept)
      k -> (maybe keys (: keys) k, (i, option) : kept)
    held = Map.fromList [(ObjectView.object o, o) | s <- PlayerView.seats view, SeatView.player s == PlayerView.viewer view, o <- SeatView.hand s]
    key option = do
      kind <- Protocol.field "action" option >>= textOf
      Monad.guard (kind `elem` fmap Text.pack ["PlayLand", "Cast", "Plot"])
      o <- Protocol.field "object" option >>= objectOf >>= (`Map.lookup` held)
      R.Object pairs <- Just option
      pure (filter ((/= Text.pack "object") . fst) pairs, o {ObjectView.object = ObjectId.MkObjectId 0})

-- | Recipients grouped by seat, the deciding seat's first, each seat's
-- player ahead of its objects, and otherwise in the offer's order.
bySeat :: PlayerView.PlayerView -> [R.Reply] -> [R.Reply]
bySeat view = List.sortOn key
  where
    key reply =
      let pid = maybe (recipientPlayer reply) (Render.seatOf view) (recipientObject reply)
       in (Maybe.isNothing pid, fmap (\p -> (p /= PlayerView.viewer view, p)) pid, Maybe.isJust (recipientObject reply))

-- | The player a recipient names, if it is one.
recipientPlayer :: R.Reply -> Maybe PlayerId.PlayerId
recipientPlayer reply = case reply of
  R.Array [R.Text tag, R.Text x] | tag == Text.pack "Player" -> fmap PlayerId.MkPlayerId (Read.readMaybe (Text.unpack x))
  _ -> Nothing

-- | The object a recipient names, if it is not a player.
recipientObject :: R.Reply -> Maybe ObjectId.ObjectId
recipientObject reply = case reply of
  R.Array [R.Text tag, x] | tag /= Text.pack "Player" -> objectOf x
  _ -> Nothing
