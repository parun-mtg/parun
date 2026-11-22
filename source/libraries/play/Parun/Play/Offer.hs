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
import qualified Parun.Codec.CostComponent as Codec.CostComponent
import qualified Parun.Codec.CounterKind as Codec.CounterKind
import qualified Parun.Codec.EntryOption as Codec.EntryOption
import qualified Parun.Codec.GameEvent as Codec.GameEvent
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaSymbol as Codec.ManaSymbol
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.StoredResult as Codec.StoredResult
import qualified Parun.Codec.TextWord as Codec.TextWord
import qualified Parun.Codec.WordFamily as Codec.WordFamily
import qualified Parun.Codec.Zone as Codec.Zone
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Engine.TextWord as TextWord.Engine
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Oracle.Cost as Oracle.Cost
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Types.DieResult as DieResult
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.GameEvent as GameEvent
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
import qualified Parun.Types.RollReading as RollReading
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.StoredResult as StoredResult
import qualified Parun.Types.TextWord as TextWord
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
objectName view reply = maybe (Text.pack "something") (Wording.named view) (objectOf reply)

-- | An object an offer lists, told apart from others of its name.
listedName :: PlayerView.PlayerView -> R.Reply -> Text.Text
listedName view reply = maybe (Text.pack "something") (Wording.choosable view) (objectOf reply)

-- | A seat a listed offer names by its bare id, or declining as None.
playerName :: R.Reply -> Text.Text
playerName reply = case reply of
  R.Text x -> maybe x Wording.player (playerOf reply)
  _ -> Text.pack "None"

-- | The seat a bare id names, as a listed offer and a player recipient
-- (recipientPlayer) carry it.
playerOf :: R.Reply -> Maybe PlayerId.PlayerId
playerOf reply = case reply of
  R.Text x -> PlayerId.MkPlayerId <$> Read.readMaybe (Text.unpack x)
  _ -> Nothing

recipientName :: PlayerView.PlayerView -> R.Reply -> Text.Text
recipientName view reply = case reply of
  R.Array [R.Text tag, R.Text x]
    | tag == Text.pack "Player" -> maybe x Wording.player (playerOf (R.Text x))
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
      case Text.uncons (Wording.spaced t) of
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
    -- A counter's kind, a mana type, or another tagged value by its tag.
    | Just kind <- decoded (Codec.CounterKind.codec Codec.Keyword.codec) reply -> Wording.counterName kind <> Text.pack " counter"
    | Just mana <- decoded Codec.ManaType.codec reply -> Text.pack "{" <> Wording.manaType mana <> Text.pack "}"
    -- A permission to cast (Parun.Codec.CastFromZone) by where it opens: CR
    -- 400.11's outside the game, a zone, or both.
    | Just places <- permissionPlaces reply -> Text.pack "from " <> Text.intercalate (Text.pack " or ") places
    | Just tag <- Protocol.field "type" reply >>= textOf -> tagName tag <> foldMap (\value -> Text.pack " " <> optionName view value) (Protocol.field "value" reply)
    -- A cost, as Oracle text prints it.
    | Maybe.isJust (Protocol.field "components" reply) -> candidateName Text.empty reply
  _ -> plainName reply

-- | Where a permission to cast opens, outside the game first, as The
-- Holodeck's Oracle text orders them; Nothing for an offer that is no such
-- permission.
permissionPlaces :: R.Reply -> Maybe [Text.Text]
permissionPlaces reply =
  let outside = [Text.pack "outside the game" | Protocol.field "outsideTheGame" reply == Just (R.Boolean True)]
      zone = [Text.pack "the " <> Text.toLower z | Just z <- [Protocol.field "from" reply >>= Protocol.field "zone" >>= textOf]]
   in if null (outside <> zone) then Nothing else Just (outside <> zone)

-- | A ChooseCost candidate's cost as Oracle text prints it (Oracle.Cost.cost),
-- or the offer itself where it does not print.
candidateName :: Text.Text -> R.Reply -> Text.Text
candidateName self candidate =
  Maybe.fromMaybe piecewise (decoded (Codec.Cost.codec Codec.Keyword.codec) candidate >>= Maybe.listToMaybe . Oracle.Cost.cost self (Oracle.Keyword.keywordWord self))
  where
    -- Its mana and each component apart, where the whole does not print.
    piecewise =
      let mana = [Wording.manaCost m | Just m <- [Protocol.field "mana" candidate >>= decoded Codec.ManaCost.codec], not (null (ManaCost.unwrap m))]
          components = foldMap (fmap (componentName self)) (Protocol.field "components" candidate >>= Protocol.arrayOf)
       in if null (mana <> components) then plainName candidate else Text.intercalate (Text.pack ", ") (mana <> components)

-- | A cost component as Oracle text prints it (Oracle.Cost.component), or in
-- plain words where it does not print.
componentName :: Text.Text -> R.Reply -> Text.Text
componentName self component =
  Maybe.fromMaybe (plainName component) (decoded (Codec.CostComponent.codec Codec.Keyword.codec) component >>= Maybe.listToMaybe . Oracle.Cost.component self (Oracle.Keyword.keywordWord self))

-- | A value an offer spells that nothing renders, in words rather than JSON:
-- each tag spaced, a fixed amount as its number, and an empty filter as
-- nothing.
plainName :: R.Reply -> Text.Text
plainName reply = Text.unwords (filter (not . Text.null) (go reply))
  where
    go r = case r of
      R.Null -> []
      R.Boolean _ -> []
      R.Number n -> [Text.pack (show n)]
      R.Text t -> [Text.toLower (Wording.spaced t)]
      R.Array xs -> concatMap go xs
      R.Object pairs
        | Just (R.Text tag) <- lookup (Text.pack "type") pairs ->
            [Text.toLower (Wording.spaced tag) | tag `notElem` fmap Text.pack ["Fixed", "And", "Literal", "Exactly"]] <> foldMap go (lookup (Text.pack "value") pairs)
        | otherwise -> concatMap (go . snd) pairs

decoded :: Codec.Codec a -> R.Reply -> Maybe a
decoded codec = either (const Nothing) Just . Codec.decode codec . Codec.Reply.toValue

-- | An action as the offer names it.
actionName :: PlayerView.PlayerView -> R.Reply -> Text.Text
actionName view action =
  let object = fmap (objectName view) (Protocol.field "object" action)
      subject = Maybe.fromMaybe Text.empty (actionSubject view action)
   in case maybe "" Text.unpack (Protocol.field "action" action >>= textOf) of
        "Pass" -> Text.pack "Pass"
        "PlayLand" -> Text.pack "Play " <> subject
        "Cast" -> Text.pack "Cast " <> subject
        -- The ability's Oracle text where the offer carries it, which says
        -- what it costs and does.
        "Activate"
          | Just text <- Protocol.field "text" action >>= textOf -> subject <> Text.pack ": " <> text
          | otherwise -> maybe (Text.pack "Activate an ability of ") (\i -> Text.pack ("Activate ability " <> show (i + 1) <> " of ")) (Protocol.field "ability" action >>= Protocol.naturalOf) <> subject
        "ActivateManaAbility" -> Text.pack "Activate a mana ability of " <> subject
        -- CR 701.40c: which procedure, where two may turn one permanent up.
        "TurnFaceUp" -> Text.pack "Turn face up " <> subject <> foldMap (\p -> Text.pack " (" <> Wording.spaced p <> Text.pack ")") (Protocol.field "procedure" action >>= textOf)
        -- CR 116.2d: which of the permanent's abilities is ignored.
        "Ignore" -> Text.pack "Ignore " <> subject <> foldMap (Text.pack "'s " <>) (Protocol.field "abilityName" action >>= textOf)
        -- CR 702.170a: which plot cost is paid.
        "Plot" -> Text.pack "Plot " <> subject <> foldMap (\c -> Text.pack " for " <> candidateName subject c) (Protocol.field "cost" action)
        kind -> Wording.spaced (Text.pack kind) <> maybe Text.empty (Text.pack " " <>) object

-- | An action as a verb phrase within a sentence: the label's own case bar
-- its first letter, and an activation by its source alone.
actionPhrase :: PlayerView.PlayerView -> R.Reply -> Text.Text
actionPhrase view action
  | (Protocol.field "action" action >>= textOf) == Just (Text.pack "Activate") = Text.pack "activate " <> Maybe.fromMaybe Text.empty (actionSubject view action)
  | otherwise = maybe Text.empty (\(c, rest) -> Text.cons (Char.toLower c) rest) (Text.uncons (actionName view action))

-- | What an action acts on: the name the offer gives, else its object's.
actionSubject :: PlayerView.PlayerView -> R.Reply -> Maybe Text.Text
actionSubject view action = Monad.mplus (Protocol.field "name" action >>= textOf) (fmap (objectName view) (Protocol.field "object" action))

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
      let pid = maybe (recipientPlayer reply) (Wording.seatOf view) (recipientObject reply)
       in (Maybe.isNothing pid, fmap (\p -> (p /= PlayerView.viewer view, p)) pid, Maybe.isJust (recipientObject reply))

-- | The player a recipient names, if it is one.
recipientPlayer :: R.Reply -> Maybe PlayerId.PlayerId
recipientPlayer reply = case reply of
  R.Array [R.Text tag, x] | tag == Text.pack "Player" -> playerOf x
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
  recipientName view reply <> life <> case (pileOf reply, piles) of
    (Just pile, _ : _ : _) -> foldMap (\i -> Text.pack (" (pile " <> show (i + 1) <> ")")) (List.elemIndex pile piles)
    _ -> Text.empty
  where
    -- A player by their life total too, which aiming damage weighs.
    life = case reply of
      _
        | Just pid <- recipientPlayer reply,
          seat : _ <- filter ((== pid) . SeatView.player) (PlayerView.seats view) ->
            Text.pack (" (" <> show (SeatView.life seat) <> " life)")
      _ -> Text.empty
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
   in Text.pack "{" <> Wording.manaType (ManaUnit.manaType unit) <> Text.pack "}" <> (if null notes then Text.empty else Text.pack " (" <> Text.intercalate (Text.pack ", ") notes <> Text.pack ")")

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
  "OrderEntries" -> Text.pack "the first applies its as-enters effects first"
  "OrderCombatTolls" -> Text.pack "the first is paid first"
  -- CR 713.8a.
  "OrderComponentCards" -> Text.pack "the first goes into " <> maybe (Text.pack "the zone") PromptOffer.zoneName (Protocol.field "zone" offer >>= decoded Codec.Zone.codec) <> Text.pack " first"
  _ -> Text.pack "the first goes first"

-- | An entry of an offer naming what each option picks
-- (Parun.Scenario.Promptindexing): a cast as actionName names it, an entering
-- shape (Parun.Types.EntryOption) by its power and toughness, keywords and
-- subtypes, an event as the log tells it, an object stating something as
-- sourcedName names it, a roll modifier as modifierName does, an unwitnessed
-- event (null) as unseen, and anything else as an option is named.
entryName :: PlayerView.PlayerView -> R.Reply -> Text.Text
entryName view entry
  | Maybe.isJust (Protocol.field "action" entry) = actionName view entry
  | Just place <- Protocol.field "place" entry = modifierName view entry place
  | Just shape <- decoded Codec.EntryOption.codec entry =
      Text.intercalate (Text.pack ", ") (Text.pack (show (EntryOption.power shape) <> "/" <> show (EntryOption.toughness shape)) : concatMap (take 1 . Oracle.Keyword.keywordWord Text.empty) (Set.toList (EntryOption.keywords shape)) <> fmap (Text.pack . show) (Set.toList (EntryOption.subtypes shape)))
  | Just event <- decoded Codec.GameEvent.codec entry =
      Maybe.fromMaybe (Common.render (Codec.Reply.toValue entry)) (Wording.event Nothing view (LoggedEvent.MkLoggedEvent (EventGroup.MkEventGroup 0) event))
  | Maybe.isJust (Protocol.field "source" entry) = sourcedName Text.empty view entry
  | entry == R.Null = Text.pack "Something you did not see"
  | otherwise = optionName view entry

-- | CR 706.2b: a roll modifier (Prompt.ChooseRollModifier's entry) by the
-- object stating it, told apart from others of its name, and the die it
-- reaches by its place in the roll, counted from 1, and its natural result;
-- the whole roll where it names no die.
modifierName :: PlayerView.PlayerView -> R.Reply -> R.Reply -> Text.Text
modifierName view entry place =
  let source = maybe (Text.pack "A rule") (listedName view) (Monad.mfilter (/= R.Null) (Protocol.field "source" entry))
      reach = case (Protocol.naturalOf place, Protocol.field "result" entry >>= Protocol.naturalOf) of
        (Just at, Just result) -> Text.pack ("die " <> show (at + 1) <> ", showing " <> show result)
        _ -> Text.pack "the whole roll"
   in source <> Text.pack ": " <> reach

-- | CR 706.2a: a reroll's question (Prompt.Rethrow's offer): the die by its
-- place in the roll, counted from 1, and what it shows, the roll's every
-- result where it has more than one, and the cost where there is one. A
-- coin's (CR 705.1, Goblin Bookie's reflip) is worded as a reflip, beside
-- every coin of its unit.
rerollQuestion :: Text.Text -> R.Reply -> Maybe Text.Text
rerollQuestion self offer = do
  let face = fmap Text.toLower (Protocol.field "face" offer >>= textOf)
      (verb, thing, whole) = if Maybe.isJust face then ("Reflip", "coin", "The coins") else ("Reroll", "die", "The roll")
  shown <- Monad.mplus (fmap (Text.pack . show) (Protocol.field "result" offer >>= Protocol.naturalOf)) face
  results <- Protocol.field "results" offer >>= Protocol.arrayOf
  place <- Protocol.field "place" offer >>= Protocol.naturalOf
  let named result = maybe (Text.toLower (plainName result)) (Text.pack . show) (Protocol.naturalOf result)
      which
        | length results > 1 = Text.pack (verb <> " " <> thing <> " " <> show (place + 1) <> " of " <> show (length results) <> ", showing ") <> shown <> Text.pack ("? " <> whole <> ": ") <> Text.intercalate (Text.pack ", ") (fmap named results) <> Text.pack "."
        | otherwise = Text.pack (verb <> " the " <> thing <> " showing ") <> shown <> Text.pack "?"
  pure (which <> foldMap (\cost -> Text.pack " Cost: " <> candidateName self cost <> Text.pack ".") (Protocol.field "cost" offer))

-- | CR 706.4: a result Prompt.ChooseDieResult offers, beside what its
-- `carries` entry tells: who threw it, where another player did, each
-- natural result and event it carries, an unwitnessed one as unseen, and the
-- places of the roll's rerolls it carries.
resultName :: PlayerView.PlayerView -> R.Reply -> R.Reply -> Text.Text
resultName view result carry =
  let seat = fmap PlayerId.MkPlayerId (Protocol.field "thrower" carry >>= textOf >>= Read.readMaybe . Text.unpack)
      thrower = [Text.pack "thrown by " <> Wording.player pid | Just pid <- [seat], pid /= PlayerView.viewer view]
      who pid = if pid == PlayerView.viewer view then Text.pack "you" else Wording.player pid
      event reply
        | reply == R.Null = Text.pack "something you did not see"
        | Just (GameEvent.DieResultSettled r) <- decoded Codec.GameEvent.codec reply,
          DieResult.reading r == RollReading.NaturalResult =
            who (DieResult.roller r) <> Text.pack (" rolled " <> show (DieResult.result r))
        | otherwise = entryName view reply
      rerolls = [Text.pack ("from the roll's reroll " <> show place) | Just places <- [Protocol.field "rerolls" carry >>= Protocol.arrayOf], Just place <- fmap Protocol.naturalOf places]
      notes = thrower <> foldMap (fmap event) (Protocol.field "events" carry >>= Protocol.arrayOf) <> rerolls
   in optionName view result <> (if null notes then Text.empty else Text.pack " (" <> Text.intercalate (Text.pack "; ") notes <> Text.pack ")")

-- | An amount-per-item offer's shape with these amounts in amountLeaves'
-- order, a map leaving out each key that takes none.
fillAmounts :: R.Reply -> [Natural] -> R.Reply
fillAmounts caps = fst . go caps
  where
    go reply typed = case reply of
      R.Array items
        | all (Maybe.isJust . Protocol.naturalOf) items ->
            let (rest, filled) = List.mapAccumL (\ns item -> swap (go item ns)) typed items
             in (R.Array filled, rest)
        | otherwise ->
            let (rest, filled) = List.mapAccumL (\ns item -> swap (entry item ns)) typed items
             in (R.Array (Maybe.catMaybes filled), rest)
      _ -> case typed of
        n : rest -> (R.Number (toInteger n), rest)
        [] -> (R.Number 0, [])
    entry item typed = case item of
      R.Array [key, value] ->
        let (filled, rest) = go value typed
         in (if filled `elem` [R.Number 0, R.Array []] then Nothing else Just (R.Array [key, filled]), rest)
      _ -> (Nothing, typed)
    swap (a, b) = (b, a)

-- | A key of an amount-per-item offer as the seat reads it: a counter's kind,
-- a stored die result, a recipient, or as an option is named.
amountKeyName :: PlayerView.PlayerView -> R.Reply -> Text.Text
amountKeyName view key
  | Just kind <- decoded (Codec.CounterKind.codec Codec.Keyword.codec) key = Wording.counterName kind <> Text.pack " counters"
  | Just result <- decoded Codec.StoredResult.codec key = Text.pack ("d" <> show (StoredResult.sides result) <> " showing " <> show (StoredResult.value result))
  | R.Array [R.Text _, _] <- key = recipientName view key
  | otherwise = optionName view key

-- | What a listed offer tells the seat beside its options, a line each, in
-- the order the offer gives: a cost or mana as Oracle text prints it, a list of who
-- did what as each beside its player, and anything else as an option is named.
-- An empty list says nothing.
told :: PlayerView.PlayerView -> Text.Text -> R.Reply -> [Text.Text]
told view self offer = case offer of
  R.Object pairs ->
    [ label key <> Text.pack ": " <> value key reply
    | (key, reply) <- pairs,
      key `notElem` fmap Text.pack ["source", "options", "entries", "players", "among", "departed", "least", "most", "names", "costs", "counts", "repeats", "amounts", "floors", "recipients", "counters", "fromOne", "oneKind"],
      reply /= R.Array []
    ]
  _ -> []
  where
    label key = case Text.uncons (Wording.spaced key) of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> key
    value key reply
      | key == Text.pack "cost" = candidateName self reply
      | key == Text.pack "symbol", Just symbol <- decoded Codec.ManaSymbol.codec reply = Wording.manaCost (ManaCost.MkManaCost [symbol])
      | key == Text.pack "mana", Just mana <- decoded Codec.ManaType.codec reply = Wording.manaType mana
      | Just pairs <- Protocol.arrayOf reply,
        Just named <- mapM byPlayer pairs =
          Text.intercalate (Text.pack "; ") named
      | otherwise = optionName view reply
    byPlayer pair = case pair of
      R.Array [player@(R.Text _), what] | Maybe.isNothing (objectOf player) -> Just (playerName player <> Text.pack ": " <> optionName view what)
      _ -> Nothing

-- | A value as an offer spells it.
encoded :: Codec.Codec a -> a -> R.Reply
encoded codec = Either.fromRight R.Null . Codec.Reply.fromValue . Codec.encode codec

-- | A ChooseWordSwap offer's words (CR 612.2): those the change may replace,
-- and those it may replace one with, outside the card's forbidden words
-- (Parun.Engine.TextWord.swappable), each as the answer spells it and named.
wordSwap :: R.Reply -> Maybe ([(R.Reply, Text.Text)], [(R.Reply, Text.Text)])
wordSwap offer = do
  families <- Protocol.field "families" offer >>= decoded (Common.set Codec.WordFamily.codec)
  let forbidden = Maybe.fromMaybe Set.empty (Protocol.field "forbidden" offer >>= decoded (Common.set Codec.TextWord.codec))
      every = fmap TextWord.ColorWord [minBound ..] <> fmap TextWord.SubtypeWord [minBound ..]
      words' = [w | w <- every, any (`TextWord.Engine.choosable` w) families]
      option w = (encoded Codec.TextWord.codec w, wordName w)
  pure (fmap option words', fmap option (filter (`Set.notMember` forbidden) words'))

-- | Whether a ChooseWordSwap offer lets this pair stand (CR 612.2): both words
-- one family's, and the new one not forbidden (Parun.Engine.TextWord.swappable).
swapAllowed :: R.Reply -> R.Reply -> R.Reply -> Bool
swapAllowed offer from to = Maybe.fromMaybe False $ do
  families <- Protocol.field "families" offer >>= decoded (Common.set Codec.WordFamily.codec)
  let forbidden = Maybe.fromMaybe Set.empty (Protocol.field "forbidden" offer >>= decoded (Common.set Codec.TextWord.codec))
  pair <- (,) <$> decoded Codec.TextWord.codec from <*> decoded Codec.TextWord.codec to
  pure (TextWord.Engine.swappable families forbidden pair)

-- | A word a text change swaps, as Oracle text prints it.
wordName :: TextWord.TextWord -> Text.Text
wordName word = case word of
  TextWord.ColorWord c -> Text.toLower (Text.pack (show c))
  TextWord.SubtypeWord s -> Maybe.fromMaybe (Text.pack (show s)) (Subtype.creatureTypeWord s)
