-- | Reading a request's offer: the options it lists and the names a client
-- shows them by, from the seat's view. Shared by every client of the line
-- protocol; how a client asks and reads an answer is its own.
module Parun.Play.Offer where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Cost as Codec.Cost
import qualified Parun.Codec.EntryOption as Codec.EntryOption
import qualified Parun.Codec.GameEvent as Codec.GameEvent
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Zone as Codec.Zone
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Oracle.Cost as Oracle.Cost
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaRetention as ManaRetention
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.ProductionTag as ProductionTag
import qualified Parun.Types.Question as Question
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

-- | A mana source a payment offers, and the mana each of its abilities makes.
manaSource :: R.Reply -> Maybe (R.Reply, [[ManaType.ManaType]])
manaSource reply = do
  source <- Protocol.field "source" reply
  makes <- Protocol.field "makes" reply >>= decoded (Common.list (Common.list Codec.ManaType.codec))
  pure (source, makes)

-- | Where Pass sits in a ChooseAction offer whose every other action is a
-- mana ability (CR 605.3a), which a client need not ask about.
onlyPass :: R.Reply -> Maybe Integer
onlyPass offer = do
  kinds <- Protocol.arrayOf offer >>= mapM (Protocol.field "action" Monad.>=> textOf)
  Monad.guard (all (`elem` [Text.pack "Pass", Text.pack "ActivateManaAbility"]) kinds)
  fmap toInteger (List.elemIndex (Text.pack "Pass") kinds)

-- | CR 601.2g: the next source auto taps, and the colors it is tapped for
-- (none for generic mana). The payer's pool goes first against the cost; then
-- each colored symbol still owed is paid from the source that makes the
-- fewest of the colors still owed, then generic mana from the source that
-- makes the fewest types. Nothing when the cost holds a symbol other than
-- generic or one type of mana, when the pool already covers it, or when no
-- source makes what is still owed.
autoSource :: PlayerView.PlayerView -> R.Reply -> Maybe (R.Reply, Set.Set ManaType.ManaType)
autoSource view offer = do
  (colored, generic) <- owing view offer
  sources <- Protocol.field "sources" offer >>= Protocol.arrayOf >>= mapM manaSource
  let wanted = Set.fromList colored
      makes (_, yields) = Set.fromList (concat yields)
      fewest by = Maybe.listToMaybe . List.sortOn by
  source <-
    if not (Set.null wanted)
      then fewest (\s -> (Set.size (Set.intersection wanted (makes s)), Set.size (makes s))) (filter (not . Set.disjoint wanted . makes) sources)
      else
        if generic > 0
          then fewest (Set.size . makes) (filter (not . Set.null . makes) sources)
          else Nothing
  pure (fst source, Set.intersection wanted (makes source))

-- | CR 601.2g-h: what of a ChooseManaSource offer's cost the payer's pool leaves
-- owed: each colored symbol the pool cannot pay, the generic mana the rest of
-- the pool does not cover. Nothing when the cost holds a symbol other than
-- generic or one type of mana.
owing :: PlayerView.PlayerView -> R.Reply -> Maybe ([ManaType.ManaType], Natural)
owing view offer = do
  cost <- Protocol.field "cost" offer >>= decoded Codec.ManaCost.codec
  owed <- mapM owedOf (ManaCost.unwrap cost)
  let pool = foldMap (fmap ManaUnit.manaType . Mana.unwrap . SeatView.manaPool) (payerSeat view offer)
      (colored, unspent) = List.foldl' spendOne ([], pool) (Either.rights owed)
      generic = sum (Either.lefts owed)
  pure (colored, generic - min generic (Natural.length unspent))
  where
    owedOf symbol = case symbol of
      ManaSymbol.Generic n -> Just (Left n)
      ManaSymbol.OfType t -> Just (Right t)
      _ -> Nothing
    spendOne (still, pool) t = case List.elemIndex t pool of
      Just i -> (still, take i pool <> drop (i + 1) pool)
      Nothing -> (still <> [t], pool)

-- | The seat of a payment offer's payer.
payerSeat :: PlayerView.PlayerView -> R.Reply -> Maybe SeatView.SeatView
payerSeat view offer = do
  payer <- Protocol.field "payer" offer >>= textOf >>= Read.readMaybe . Text.unpack
  List.find ((== PlayerId.MkPlayerId payer) . SeatView.player) (PlayerView.seats view)

-- | A recipient as a target prompt lists it: as recipientName names it, and a
-- pile numbered among the offer's piles where it holds several.
targetName :: PlayerView.PlayerView -> [R.Reply] -> R.Reply -> Text.Text
targetName view legal reply =
  recipientName view reply <> case (pileOf reply, piles) of
    (Just pile, _ : _ : _) -> foldMap (\i -> Text.pack (" (pile " <> show (i + 1) <> ")")) (List.elemIndex pile piles)
    _ -> Text.empty
  where
    piles = ListUtils.nubOrd (Maybe.mapMaybe pileOf legal)
    pileOf r = case r of
      R.Array [R.Text tag, draw] | tag == Text.pack "Pile" -> Protocol.field "pile" draw
      _ -> Nothing

-- | A ChooseTargets offer's source and its slots: name, how many, and the
-- legal recipients.
slotsOf :: Question.Question -> Maybe (R.Reply, [(R.Reply, Natural, [R.Reply])])
slotsOf question = do
  let offer = Question.offer question
  source <- Protocol.field "source" offer
  slots <- Protocol.field "slots" offer >>= Protocol.arrayOf
  parsed <- Monad.forM slots $ \slot -> case slot of
    R.Array [name, R.Array [wanted, R.Array legal]] -> fmap (\n -> (name, n, legal)) (Protocol.naturalOf wanted)
    _ -> Nothing
  pure (source, parsed)

-- | A trigger ordering's entry: its source, and its Oracle text where the
-- offer carries it.
triggerName :: PlayerView.PlayerView -> R.Reply -> Text.Text
triggerName = sourcedName (Text.pack "'s triggered ability")

-- | An entry naming its source (null for none, a rule's), and after it its
-- Oracle text where the offer carries it, or `noun` where it does not; a
-- hand action's place among its card's where the offer says it.
sourcedName :: Text.Text -> PlayerView.PlayerView -> R.Reply -> Text.Text
sourcedName noun view entry =
  let source = maybe (Text.pack "A rule") (objectName view) (Monad.mfilter (/= R.Null) (Protocol.field "source" entry))
      place = foldMap (\i -> Text.pack (" (ability " <> show (i + 1) <> ")")) (Protocol.field "ability" entry >>= Protocol.naturalOf)
   in maybe (source <> noun) (\text -> source <> Text.pack ": " <> text) (Protocol.field "text" entry >>= textOf) <> place

-- | A unit of mana in the pool: its type, and what sets it apart from others
-- of its type (CR 106.6, 106.4, 107.4h).
unitName :: ManaUnit.ManaUnit -> Text.Text
unitName unit =
  let notes =
        [Text.pack "from a snow source" | Set.member ProductionTag.Snow (ManaUnit.tags unit)]
          <> [Text.pack ("from a " <> show st) | ProductionTag.Subtype st <- Set.toList (ManaUnit.tags unit)]
          <> [Text.pack "from " <> article (Text.toLower (Text.pack (show ct))) | ProductionTag.CardType ct <- Set.toList (ManaUnit.tags unit)]
          <> [Text.pack "kept until end of turn" | ManaUnit.retention unit == ManaRetention.UntilEndOfTurn]
          <> [Text.pack "kept until end of combat" | ManaUnit.retention unit == ManaRetention.UntilEndOfCombat]
          <> [Text.pack "restricted" | Maybe.isJust (ManaUnit.restriction unit)]
          <> [Text.pack "with an effect when spent" | Maybe.isJust (ManaUnit.rider unit) || Maybe.isJust (ManaUnit.spendTrigger unit)]
      article noun = (if Text.take 1 noun `elem` fmap Text.singleton "aeiou" then Text.pack "an " else Text.pack "a ") <> noun
   in Text.pack "{" <> Render.manaType (ManaUnit.manaType unit) <> Text.pack "}" <> (if null notes then Text.empty else Text.pack " (" <> Text.intercalate (Text.pack ", ") notes <> Text.pack ")")

-- | A mana source as an offer names it, beside the mana types of each yield.

-- | What the first item of an order is, by the prompt's kind and what its
-- offer tells.
orderNote :: Text.Text -> R.Reply -> Text.Text
orderNote kind offer = case Text.unpack kind of
  -- CR 401.4: from the top inward.
  "ArrangeLibraryCards" -> Text.pack "the first goes on top"
  "ArrangeLibraryArrivals" -> case Protocol.field "end" offer of
    Just (R.Text end) | end == Text.pack "Top" -> Text.pack "the first goes on top"
    _ -> Text.pack "the first goes on the bottom"
  -- CR 613.7m.
  "OrderTimestamps" -> Text.pack "the first gets the earliest timestamp"
  "OrderManaActivations" -> Text.pack "the first is activated first"
  "OrderCombatTolls" -> Text.pack "the first is paid first"
  -- CR 713.8a.
  "OrderComponentCards" -> Text.pack "the first goes into " <> maybe (Text.pack "the zone") PromptOffer.zoneName (Protocol.field "zone" offer >>= decoded Codec.Zone.codec) <> Text.pack " first"
  _ -> Text.pack "the first goes first"

-- | An entry of an offer naming what each option picks
-- (Parun.Scenario.Promptindexing): a cast as actionName names it, an entering
-- shape (Parun.Types.EntryOption) by its power and toughness, keywords and
-- subtypes, an event as the log tells it, an object stating something as
-- sourcedName names it, an unwitnessed event (null) as unseen, and anything
-- else as an option is named.
entryName :: PlayerView.PlayerView -> R.Reply -> Text.Text
entryName view entry
  | Maybe.isJust (Protocol.field "action" entry) = actionName view entry
  | Just shape <- decoded Codec.EntryOption.codec entry =
      Text.intercalate (Text.pack ", ") (Text.pack (show (EntryOption.power shape) <> "/" <> show (EntryOption.toughness shape)) : concatMap (take 1 . Oracle.Keyword.keywordWord Text.empty) (Set.toList (EntryOption.keywords shape)) <> fmap (Text.pack . show) (Set.toList (EntryOption.subtypes shape)))
  | Just event <- decoded Codec.GameEvent.codec entry =
      Maybe.fromMaybe (Common.render (Codec.Reply.toValue entry)) (Render.event view (LoggedEvent.MkLoggedEvent (EventGroup.MkEventGroup 0) event))
  | Maybe.isJust (Protocol.field "source" entry) = sourcedName Text.empty view entry
  | entry == R.Null = Text.pack "Something you did not see"
  | otherwise = optionName view entry
