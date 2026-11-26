-- | The web client's lobby (#9421): the seats at the table, each on a team
-- where the variant has teams, the seat being chosen for, which a tap on a
-- seat or a seat added changes, the game's options, and Start at their foot;
-- the decks that seat may play, filtered by what is typed, by colour, by set
-- and by whether they are saved; and the chosen deck's detail. Or a game to
-- play on from a replay's position (PlayOn): its seats, how each is filled
-- and the clock.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * Deploying creatures (CR 804) (#9794).
module Parun.Web.Lobby where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.AlternatingTeams as AlternatingTeams
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Engine.Emperor as Emperor
import qualified Parun.Engine.GrandMelee as GrandMelee
import qualified Parun.Engine.RuleFamily as RuleFamily.Engine
import qualified Parun.Engine.Seating as Seating
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.AttackOption as AttackOption
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Emperors as Emperors
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.RangeOfInfluence as RangeOfInfluence
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Teams as Teams
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.BotSettings as BotSettings
import qualified Parun.Web.Clock as Clock
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Route as Route
import qualified Text.Read as Read

data Lobby = MkLobby
  { -- | The decks, what is read of them, and what they are filtered by.
    browser :: DeckBrowser.DeckBrowser,
    -- | The player's deck, once chosen.
    mine :: Maybe Text.Text,
    -- | Each other seat's deck, in seat order, once chosen; a random one
    -- otherwise. There are at most 'mostOthers' such seats, and none is a
    -- game of yours alone (goldfish).
    theirs :: [Maybe Text.Text],
    -- | The seat a tapped deck goes to, which a tap on a seat or a seat
    -- added changes, never a pick.
    choosing :: Slot,
    -- | Each seat taken by a player invited from another browser, by its
    -- place in 'theirs', or in a game played on by its place at the table
    -- (Parun.Web.Link).
    invites :: Map.Map Int Invite,
    -- | The link number the next invite takes, never one an invite took
    -- before, so a closed link's late word reaches no other.
    nextLink :: Int,
    -- | Each seat played on this screen, by its place in 'theirs', which
    -- holds its deck, and how it is played there; a bot plays every seat in
    -- neither map.
    locals :: Map.Map Int Link.Sitting,
    -- | Whether you are ready to start a game with the players invited.
    ready :: Bool,
    -- | How strongly each bot plays and what it values in a position
    -- (Parun.Play.Opponent), by its place in 'theirs'; 'BotSettings.defaultBot' in a
    -- place absent ('botAt').
    bots :: Map.Map Int (Difficulty.Difficulty, Persona.Persona),
    -- | Whether a game is in progress, set aside for this lobby, which
    -- starting another abandons.
    paused :: Bool,
    -- | What was tapped that abandons the game in progress, and the value
    -- typed with it, asked about first.
    abandoning :: Maybe (Event, Text.Text),
    -- | The options the game is started with.
    options :: Options,
    -- | The team each seat chose (CR 808.1), by its place at the table,
    -- yours first; 'defaultTeam' in a place absent ('teamAt').
    teams :: Map.Map Int Natural,
    -- | The place at the table each team chose for its emperor (CR 809.2),
    -- yours first, by team; 'emperorOf' where absent.
    emperors :: Map.Map Natural Int,
    -- | Why Start last refused the seats' lists ('planarDealt',
    -- 'schemeDealt'), until the match-up changes.
    refusal :: Maybe Text.Text,
    -- | The game to play on from a replay's position, which seats as the
    -- replay does; Nothing for a new game.
    playingOn :: Maybe PlayOn
  }
  deriving (Eq, Show)

-- | A game to play on from a replay's position: its seats as the
-- replay has them, each seat's deck fixed and its options the replay's
-- (Parun.Scenario.branch), and how it is seated, every seat a person plays
-- given the lobby's clock ('playedOn').
data PlayOn = MkPlayOn
  { begun :: Link.PlayedOn,
    -- | Each seat's deck as the replay names it, in seating order.
    places :: [Text.Text]
  }
  deriving (Eq, Show)

-- | A lobby to play on from a replay's position, seated so, each seat's
-- deck as `named` names it, and each seat filled by an invite given one
-- ('invitedFor').
playingOnFrom :: Link.PlayedOn -> [Text.Text] -> Lobby
playingOnFrom seated named = invitedFor new {playingOn = Just (MkPlayOn seated named)}

-- | A game played on with an invite for each seat but the host's that one
-- fills, a new one taking 'nextLink' onward, and none for any other seat;
-- a new game as it is.
invitedFor :: Lobby -> Lobby
invitedFor lobby = case playingOn lobby of
  Nothing -> lobby
  Just table ->
    let game = begun table
        wanted = [place | place <- [0 .. length (places table) - 1], let pid = PlayerId.MkPlayerId (Int.toNaturalSaturating place), pid /= Link.host game, Link.fillingAt game pid == Link.MkFilledByInvite]
        kept = Map.restrictKeys (invites lobby) (Set.fromList wanted)
        added = filter (`Map.notMember` kept) wanted
     in lobby {invites = kept <> Map.fromList (zip added (fmap newInvite [nextLink lobby ..])), nextLink = nextLink lobby + length added}

-- | The game to play on, with the lobby's clock; Nothing for a new game.
playedOn :: Lobby -> Maybe Link.PlayedOn
playedOn lobby = (\table -> (begun table) {Link.minutes = clock (options lobby)}) <$> playingOn lobby

-- | The ways a seat but the host's of a game played on may be filled, each
-- with its value and its label, a bot as the lobby's default one.
fillingChoices :: [(Link.Filling, (String, String))]
fillingChoices =
  [ (Link.defaultFilling, ("bot", "Bot")),
    (Link.MkFilledHere Link.MkHandedOver, ("here", "Player here")),
    (Link.MkFilledHere Link.MkYours, ("yours", "Yourself")),
    (Link.MkFilledByGoldfish, ("goldfish", "Goldfish (only passes)")),
    (Link.MkFilledByInvite, ("invited", "Invite a player"))
  ]

-- | Whether two fillings are of one kind, whatever a bot's settings.
sameKind :: Link.Filling -> Link.Filling -> Bool
sameKind one two = case (one, two) of
  (Link.MkFilledByBot {}, Link.MkFilledByBot {}) -> True
  _ -> one == two

-- | The game to play on after an event; any other than these changes
-- nothing: a seat but the host's filled as the value chosen names, its
-- bot's difficulty or persona chosen, or the host taking it, the seat the
-- host leaves filled as that one was, but one an invited player fills,
-- whose link is that seat's.
seatedOn :: Event -> Text.Text -> PlayOn -> PlayOn
seatedOn event typed table =
  let game = begun table
      seat place = PlayerId.MkPlayerId (Int.toNaturalSaturating place)
      filled place f = table {begun = game {Link.fillings = Map.insert (seat place) f (Link.fillings game)}}
      other place = place >= 0 && place < length (places table) && seat place /= Link.host game
      named nameOf = Opponent.named nameOf (Text.unpack typed)
      picked = Maybe.listToMaybe [f | (f, (value, _)) <- fillingChoices, value == Text.unpack typed]
   in case event of
        MkFill place
          | other place,
            Just f <- picked,
            not (sameKind f (Link.fillingAt game (seat place))) ->
              filled place f
        MkDifficulty place
          | other place,
            Link.MkFilledByBot _ style <- Link.fillingAt game (seat place),
            Just level <- named Opponent.difficultyName ->
              filled place (Link.MkFilledByBot level style)
        MkPersona place
          | other place,
            Link.MkFilledByBot level _ <- Link.fillingAt game (seat place),
            Just style <- named Opponent.personaName ->
              filled place (Link.MkFilledByBot level style)
        MkTakeSeat place
          | other place,
            Link.fillingAt game (seat place) /= Link.MkFilledByInvite ->
              let taken = seat place
               in table {begun = game {Link.host = taken, Link.fillings = Map.insert (Link.host game) (Link.fillingAt game taken) (Map.delete taken (Link.fillings game))}}
        _ -> table

-- | The options of CR 800.2, and the variants' (CR 407, 901, 904), a game started
-- here is played with ('settings').
data Options = MkOptions
  { -- | CR 800.2: the multiplayer variant, the one a game may use.
    variant :: Variant,
    -- | CR 407.1: whether the game is played for ante.
    ante :: Bool,
    -- | CR 901: whether the game is a Planechase game, each seat whose list
    -- brings no planar deck dealt one ('planarDealt').
    planechase :: Bool,
    -- | CR 901.15: whether a Planechase game uses one communal planar deck.
    singlePlanarDeck :: Bool,
    -- | CR 904.2a: the archenemy's place at the table, yours first, in an
    -- Archenemy game ('schemeDealt'); none in any other.
    archenemy :: Maybe Int,
    -- | CR 806.2b: the attack option a game of three or more seats uses.
    attackOption :: AttackOption.AttackOption,
    -- | CR 806.2a: every player's range of influence in a game of three or
    -- more seats, in seats; unlimited where absent (CR 801.1).
    rangeOfInfluence :: Maybe Natural,
    -- | CR 804.1: whether a game between teams uses the deploy creatures
    -- option ('deployOffered').
    deployCreatures :: Bool,
    -- | The minutes each seat a person plays has on its clock
    -- (Parun.Web.Clock), which no rule sets; untimed where absent.
    clock :: Maybe Natural
  }
  deriving (Eq, Show)

-- | No option in use: CR 802's attack multiple players, which needs no
-- seating agreed ('GameSettings.plain').
noOptions :: Options
noOptions = MkOptions {variant = MkFreeForAll, ante = False, planechase = False, singlePlanarDeck = False, archenemy = Nothing, attackOption = AttackOption.MultiplePlayers, rangeOfInfluence = Nothing, deployCreatures = False, clock = Nothing}

-- | A multiplayer variant a game started here may use (CR 800.2).
data Variant
  = -- | CR 806.
    MkFreeForAll
  | -- | CR 808.
    MkTeamVsTeam
  | -- | CR 810.
    MkTwoHeadedGiant
  | -- | CR 811.
    MkAlternatingTeams
  | -- | CR 809.
    MkEmperor
  | -- | CR 807.
    MkGrandMelee
  deriving (Eq, Show)

-- | The variants a game may choose, each with its value and its label.
variants :: [(Variant, (String, String))]
variants =
  [ (MkFreeForAll, ("free-for-all", "Free-for-All")),
    (MkTeamVsTeam, ("team-vs-team", "Team vs. Team")),
    (MkTwoHeadedGiant, ("two-headed-giant", "Two-Headed Giant")),
    (MkAlternatingTeams, ("alternating-teams", "Alternating Teams")),
    (MkEmperor, ("emperor", "Emperor")),
    (MkGrandMelee, ("grand-melee", "Grand Melee"))
  ]

-- | The variant the game is played as: the one chosen at three seats or
-- more, and Free-for-All at two, a game that is not multiplayer (CR 800.1),
-- or in an Archenemy game, whose teams Parun.Engine.Archenemy.setUp makes
-- (CR 904.2).
variantOf :: Lobby -> Variant
variantOf lobby = if multiplayer (seatCount lobby) && not (archenemyGame lobby) then variant (options lobby) else MkFreeForAll

-- | Whether the game is an Archenemy game (CR 904).
archenemyGame :: Lobby -> Bool
archenemyGame = Maybe.isJust . archenemy . options

-- | Whether the game is played between teams (CR 808.1), so each seat
-- chooses one: every variant but Free-for-All and Grand Melee (CR 807.1).
teamed :: Lobby -> Bool
teamed lobby = variantOf lobby `notElem` [MkFreeForAll, MkGrandMelee]

-- | Whether the game may choose the deploy creatures option: it is played
-- between teams, and not as Emperor, which always uses it (CR 804.1).
deployOffered :: Lobby -> Bool
deployOffered lobby = teamed lobby && variantOf lobby /= MkEmperor

-- | How many teams a seat may choose among: CR 810.1's two, else as many
-- as there are seats.
teamChoices :: Lobby -> Natural
teamChoices lobby = if variantOf lobby == MkTwoHeadedGiant then 2 else seatCount lobby

-- | The team a seat at this place at the table is on until it chooses, of
-- so many: the table halved, yours and the seats after it first, a team
-- sitting together (CR 808.2, 810.3), or in Emperor split into teams of
-- three where it seats two or more (CR 809.1); or, in Alternating Teams,
-- the teams taking turns round the table (CR 811.3), two where the table
-- splits evenly and one seat each otherwise.
defaultTeam :: Variant -> Natural -> Int -> Natural
defaultTeam variant' count place = case variant' of
  MkAlternatingTeams -> Int.toNaturalSaturating place `mod` (if even count then 2 else count)
  MkEmperor -> Int.toNaturalSaturating place * max 2 (count `div` 3) `div` max 1 count
  _ -> Int.toNaturalSaturating place * 2 `div` max 1 count

-- | The team the seat at this place at the table is on, yours first.
teamAt :: Lobby -> Int -> Natural
teamAt lobby place = Map.findWithDefault (defaultTeam (variantOf lobby) (seatCount lobby) place) place (teams lobby)

-- | The places at the table on this team, yours first.
membersOf :: Lobby -> Natural -> [Int]
membersOf lobby team = filter ((== team) . teamAt lobby) [0 .. Natural.toIntSaturating (seatCount lobby) - 1]

-- | The place at the table of this team's emperor (CR 809.2): the one it
-- chose while that seat is still on it, else its middle seat, the later of
-- the two for a team of even size; none for a team nobody is on.
emperorOf :: Lobby -> Natural -> Maybe Int
emperorOf lobby team =
  let members = membersOf lobby team
   in Monad.mfilter (`elem` members) (Map.lookup team (emperors lobby)) Applicative.<|> Maybe.listToMaybe (drop (length members `div` 2) members)

-- | The teams some seat is on, in order.
teamsSeated :: Lobby -> [Natural]
teamsSeated lobby = ListUtils.nubOrd (List.sort (fmap (teamAt lobby) [0 .. Natural.toIntSaturating (seatCount lobby) - 1]))

-- | Each team's emperor (CR 809.2), none outside the Emperor variant.
seatEmperors :: Lobby -> Emperors.Emperors
seatEmperors lobby
  | variantOf lobby == MkEmperor = Emperors.MkEmperors (Map.fromList [(TeamId.MkTeamId team, Command.idOf (Int.toNaturalSaturating place + 1)) | team <- teamsSeated lobby, Just place <- [emperorOf lobby team]])
  | otherwise = Emperors.none

-- | Each seat's team (CR 808.1), none where the game has no teams.
seatTeams :: Lobby -> Teams.Teams
seatTeams lobby
  | teamed lobby = Teams.MkTeams (Map.fromList [(Command.idOf (Int.toNaturalSaturating place + 1), TeamId.MkTeamId (teamAt lobby place)) | place <- [0 .. Natural.toIntSaturating (seatCount lobby) - 1]])
  | otherwise = Teams.none

-- | The attack options a game may choose (CR 806.2b), each with its value
-- and its label; CR 809.3c's is the Emperor variant's.
attackOptions :: [(AttackOption.AttackOption, (String, String))]
attackOptions =
  [ (AttackOption.MultiplePlayers, ("multiple", "Attack any opponents")),
    (AttackOption.Leftward, ("left", "Attack left")),
    (AttackOption.Rightward, ("right", "Attack right"))
  ]

-- | The limited ranges of influence offered (CR 801.2a's commonest), each
-- with its value and label, after an unlimited one.
ranges :: [(Maybe Natural, (String, String))]
ranges = (Nothing, ("unlimited", "Unlimited")) : [(Just n, (show n, show n <> (if n == 1 then " seat" else " seats"))) | n <- [1, 2]]

-- | Whether a game of so many seats is multiplayer, and so chooses CR 806.2's
-- options (CR 800.1).
multiplayer :: Natural -> Bool
multiplayer count = count >= 3

-- | Whether the game chooses CR 806.2's attack option and range of
-- influence: at three seats or more, but in Two-Headed Giant, whose teams
-- attack as CR 805.10 has it, in Emperor and Grand Melee, which use their
-- own (CR 809.3, 807.2), and in an Archenemy game, which uses no option but
-- CR 904.2's.
chosesAttacks :: Lobby -> Bool
chosesAttacks lobby = multiplayer (seatCount lobby) && variantOf lobby `notElem` [MkTwoHeadedGiant, MkEmperor, MkGrandMelee] && not (archenemyGame lobby)

-- | How many seats the game has: yours and each other.
seatCount :: Lobby -> Natural
seatCount lobby = 1 + Natural.length (theirs lobby)

-- | The options the game is started with (CR 800.2), its format aside, which
-- the lists' headers name: the lobby's, CR 806.2's at three seats or more
-- only, a range of influence being every seat's (CR 806.2a); and the
-- variant's over each seat's team. Or why the table can't play the variant:
-- a seating Parun.Engine.Seating.misseated refuses (CR 808.2, 810.3), fewer
-- than two teams (CR 808.1), Two-Headed Giant's two teams of two (CR 810.1),
-- what Parun.Engine.AlternatingTeams.setUp refuses (CR 811.1, 811.3), or
-- what Parun.Engine.Emperor.setUp refuses (CR 809.2, 809.6, 809.6a). Grand
-- Melee's options are Parun.Engine.GrandMelee.setUp's (CR 807.2). An
-- Archenemy game's teams and options are Parun.Engine.Archenemy.setUp's,
-- which the archenemy's scheme deck turns on; it needs a seat against the
-- archenemy (CR 904.2).
settings :: Lobby -> Either String GameSettings.GameSettings
settings lobby =
  let chosen' = options lobby
      count = seatCount lobby
      everyone n = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(Command.idOf k, n) | k <- [1 .. count]])
      plain =
        GameSettings.plain
          { GameSettings.ante = ante chosen',
            GameSettings.singlePlanarDeck = planechase chosen' && singlePlanarDeck chosen',
            GameSettings.deployCreatures = deployOffered lobby && deployCreatures chosen'
          }
      free =
        plain
          { GameSettings.attackOption = if multiplayer count then Just (attackOption chosen') else GameSettings.attackOption GameSettings.plain,
            GameSettings.rangeOfInfluence = if multiplayer count then maybe RangeOfInfluence.unlimited everyone (rangeOfInfluence chosen') else RangeOfInfluence.unlimited
          }
      teams' = seatTeams lobby
      sizes = Map.elems (Map.fromListWith (+) [(team, 1 :: Int) | team <- Map.elems (Teams.unwrap teams')])
      seats = Command.idOf 1 NonEmpty.:| [Command.idOf k | k <- [2 .. count]]
      together' settings'
        | length sizes < 2 = Left "Put the seats on two teams or more."
        | Maybe.isJust (Seating.misseated (Setup.gameWith settings' seats)) = Left "Seat each team together: teammates side by side."
        | otherwise = Right settings'
   in if archenemyGame lobby
        then if count < 2 then Left "Archenemy needs a seat against the archenemy: add one." else Right plain
        else case variantOf lobby of
          MkFreeForAll -> Right free
          MkTeamVsTeam -> together' free {GameSettings.teams = teams'}
          MkTwoHeadedGiant
            | sizes /= [2, 2] -> Left "Two-Headed Giant is two teams of two: four seats, two on each team."
            | otherwise -> together' plain {GameSettings.teams = teams', GameSettings.twoHeadedGiant = True, GameSettings.sharedTeamTurns = True, GameSettings.sharedTeamLife = True}
          MkAlternatingTeams -> maybe (Left "Alternating Teams is teams of equal size, no one beside a teammate.") (Right . MatchScope.settings . GameState.match) (AlternatingTeams.setUp teams' (Setup.gameWith free seats))
          MkEmperor
            | length sizes < 2 -> Left "Put the seats on two teams or more."
            | otherwise -> maybe (Left "Emperor is teams of equal size, each sitting together with its emperor in the middle, no emperor within another's reach.") (Right . MatchScope.settings . GameState.match) (Emperor.setUp teams' (seatEmperors lobby) (Setup.gameWith plain seats))
          MkGrandMelee -> Right (MatchScope.settings (GameState.match (GrandMelee.setUp (Setup.gameWith plain seats))))

-- | A slot a player from another browser takes.
data Invite = MkInvite
  { -- | The page's number for its link (Parun.Browser.linkHost), its own
    -- rather than its seat's, so unseating a seat before it keeps it.
    linkNumber :: Int,
    -- | The link to send them, once made.
    link :: Maybe Text.Text,
    -- | The reply code they sent back, as typed.
    replyCode :: Text.Text,
    -- | Whether their page is connected.
    connected :: Bool,
    -- | Their deck's name and list, once they chose one.
    guestDeck :: Maybe (Text.Text, Text.Text),
    -- | Whether they are ready to start.
    guestReady :: Bool,
    -- | Why the connection failed or ended.
    inviteProblem :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | The stage an invite's link names: in a game played on, its decks
-- fixed, so the guest brings none; else still gathering.
inviteStage :: Lobby -> Route.Stage
inviteStage lobby = if Maybe.isJust (playingOn lobby) then Route.MkSeated else Route.MkGathering

-- | An invite just made over link number `n`: no link yet, nothing typed.
newInvite :: Int -> Invite
newInvite n = MkInvite n Nothing Text.empty False Nothing False Nothing

-- | The seat whose invite has link number `n`.
seatOfLink :: Int -> Lobby -> Maybe Int
seatOfLink n lobby = Maybe.listToMaybe [i | (i, invite) <- Map.toList (invites lobby), linkNumber invite == n]

-- | A side of the match-up.
data Slot
  = MkMine
  | -- | The seat at this place in 'theirs', from zero.
    MkTheirs Int
  deriving (Eq, Show)

-- | Who plays a seat other than yours, chosen as it is added.
data Occupant
  = -- | The search bot (Parun.Play.Opponent).
    MkBot
  | -- | A player invited from another browser (Parun.Web.Link).
    MkInvited
  | -- | A seat played on this screen, as the sitting says.
    MkHere Link.Sitting
  deriving (Eq, Show)

-- | What a tap or a keystroke on the lobby does.
data Event
  = -- | Makes this deck the chosen slot's.
    MkPick Text.Text
  | -- | Chooses a deck for this slot next.
    MkChoose Slot
  | -- | Something done in the deck browser.
    MkBrowse DeckBrowser.Event
  | -- | A random deck of those shown, for the slot being chosen; the shell
    -- picks it.
    MkRandom
  | -- | Adds a seat past the last, up to 'mostOthers', played by this
    -- occupant, with a random deck, and chooses for it unless it is
    -- invited; the shell makes an invite's link, numbered 'nextLink'.
    MkAdd Occupant
  | -- | Unseats the seat at this place in 'theirs', those after it moving
    -- up; the shell closes an invite's link.
    MkDrop Int
  | -- | Starts the game.
    MkStart
  | -- | Opens the deck builder; the shell does it.
    MkOpenBuilder
  | -- | Opens this deck in the builder, a saved one to save over, any other
    -- as a copy; the shell does it.
    MkEdit Text.Text
  | -- | The reply code typed for this invited slot.
    MkReplyCode Int
  | -- | Connects this invited slot with the reply code typed; the shell
    -- does it.
    MkConnect Int
  | -- | Toggles whether you are ready to start with the players invited.
    MkReady
  | -- | Sets the difficulty of the bot at this place in 'theirs', or in a
    -- game played on at this place at the table, to the one whose name is
    -- chosen.
    MkDifficulty Int
  | -- | Sets the persona of the bot at this place in 'theirs', or in a game
    -- played on at this place at the table, to the one whose name is
    -- chosen.
    MkPersona Int
  | -- | In a game played on, fills the seat at this place at the table as
    -- the value chosen names ('fillingChoices').
    MkFill Int
  | -- | In a game played on, the host takes the seat at this place at the
    -- table.
    MkTakeSeat Int
  | -- | Abandons the game in progress for what was asked about; the shell
    -- does it.
    MkAbandon
  | -- | Returns to the game in progress; the shell does it.
    MkResume
  | -- | Keeps the game in progress, and this lobby.
    MkCancel
  | -- | Toggles whether the game is played for ante (CR 407.1).
    MkAnte
  | -- | Toggles whether the game is a Planechase game (CR 901).
    MkPlanechase
  | -- | Toggles whether a Planechase game uses one planar deck (CR 901.15).
    MkSinglePlanarDeck
  | -- | Toggles whether the game is an Archenemy game (CR 904), yours the
    -- archenemy's seat.
    MkArchenemy
  | -- | Sets the archenemy's seat to the place at the table whose number is
    -- chosen, yours first, from zero (CR 904.2a).
    MkArchenemySeat
  | -- | Sets the attack option to the one whose value is chosen
    -- ('attackOptions').
    MkAttackOption
  | -- | Sets the range of influence to the one whose value is chosen
    -- ('ranges').
    MkRangeOfInfluence
  | -- | Toggles whether a game between teams uses the deploy creatures
    -- option (CR 804.1).
    MkDeployCreatures
  | -- | Sets each seat's time to the one whose value is chosen
    -- (Clock.choices).
    MkClock
  | -- | Sets the variant to the one whose value is chosen ('variants'),
    -- each seat back on its 'defaultTeam'.
    MkVariant
  | -- | Puts the seat at this place at the table, yours first, on the team
    -- whose number is chosen, from zero.
    MkTeam Int
  | -- | Makes the seat at the place at the table whose number is chosen,
    -- yours first, from zero, this team's emperor (CR 809.2).
    MkEmperorSeat Natural
  deriving (Eq, Show)

-- | A lobby whose decks are still to read, nothing chosen yet, your seat
-- alone, and any bot added playing as `parun play`'s does unless told
-- otherwise.
new :: Lobby
new =
  MkLobby
    { browser = DeckBrowser.new,
      mine = Nothing,
      theirs = [],
      choosing = MkMine,
      invites = Map.empty,
      nextLink = 0,
      locals = Map.empty,
      ready = False,
      bots = Map.empty,
      paused = False,
      abandoning = Nothing,
      options = noOptions,
      teams = Map.empty,
      emperors = Map.empty,
      refusal = Nothing,
      playingOn = Nothing
    }

-- | A new lobby with this deck yours, on the shelf it is on.
holding :: Text.Text -> Lobby
holding name = browsed (\b -> b {DeckBrowser.shelf = DeckBrowser.shelfOf name}) new {mine = Just name}

-- | How the bot at this place in 'theirs' plays, of these 'bots'.
botAt :: Int -> Map.Map Int (Difficulty.Difficulty, Persona.Persona) -> (Difficulty.Difficulty, Persona.Persona)
botAt = Map.findWithDefault BotSettings.defaultBot

-- | The lobby with its deck browser changed.
browsed :: (DeckBrowser.DeckBrowser -> DeckBrowser.DeckBrowser) -> Lobby -> Lobby
browsed f lobby = lobby {browser = f (browser lobby)}

-- | Every deck.
decks :: Lobby -> Seq.Seq Text.Text
decks = DeckBrowser.decks . browser

-- | Whether a deck is one to play: not a set's whole card list.
playable :: Text.Text -> Bool
playable name = DeckBrowser.isSaved name || not (Text.isSuffixOf (Text.pack "/all") name)

-- | The lobby once this deck is imported: the panel closed, the saved decks
-- shown, and the deck the seat's being chosen for, or, where that bot may
-- not play it, yours, your seat then chosen so the chip shows where it went.
imported :: Text.Text -> Lobby -> Lobby
imported name lobby =
  let picked = update (MkPick name) Text.empty lobby
      seated = if chosen picked (choosing picked) == Just name then picked else update (MkPick name) Text.empty lobby {choosing = MkMine}
   in browsed DeckBrowser.imported seated

-- | The lobby without this deck, every seat holding it emptied.
deleted :: Text.Text -> Lobby -> Lobby
deleted name lobby =
  let emptied :: Maybe Text.Text -> Maybe Text.Text
      emptied = Monad.mfilter (/= name)
   in browsed (DeckBrowser.deleted name) lobby {mine = emptied (mine lobby), theirs = fmap emptied (theirs lobby)}

-- | The lobby holding these decks under these formats and these names, what
-- was chosen and typed kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Map.Map Text.Text Text.Text -> Lobby -> Lobby
withDecks names named titled = browsed (DeckBrowser.withDecks names named titled)

-- | The most seats a game has besides yours: a table of twelve, room for
-- Grand Melee's ten or more (CR 807.1) and four Emperor teams of three
-- (CR 809.1).
mostOthers :: Int
mostOthers = 11

-- | Of so many seats, the player's (`you`) and each in `people` speak the
-- line protocol; the search bot plays every other, at the difficulty and
-- persona (Parun.Play.Opponent) its place in 'theirs' has in `styles`
-- ('botAt'), the seats but the player's taking the places in seating order.
playOptions :: Map.Map Int (Difficulty.Difficulty, Persona.Persona) -> Natural -> PlayerId.PlayerId -> Set.Set PlayerId.PlayerId -> PlayOptions.PlayOptions
playOptions styles count you people =
  let seatPlay place = BotSettings.botPlay (botAt place styles)
      others = filter ((/= you) . Command.idOf) [1 .. count]
   in PlayOptions.none
        { PlayOptions.json = True,
          PlayOptions.bots = Map.fromList [(n, seatPlay place) | (place, n) <- zip [0 ..] others, Set.notMember (Command.idOf n) people]
        }

-- | Whether two decks are under one format, and so may meet: the game's
-- format is the one their headers name (Parun.Play.Command.settle).
together :: Lobby -> Text.Text -> Text.Text -> Bool
together lobby one two = case (Map.lookup one (DeckBrowser.formats (browser lobby)), Map.lookup two (DeckBrowser.formats (browser lobby))) of
  -- A Limited deck meets a deck naming no format that a Limited game takes,
  -- since the game is then Limited (Parun.Play.Command.gameFormat): the
  -- client's reading, so a sealed deck has opponents. A deck not yet
  -- counted is offered, and Start refuses it if it is too small.
  (Just named, Nothing) | named == limited -> bigEnough two
  (Nothing, Just named) | named == limited -> bigEnough one
  (mine', theirs') -> mine' == theirs'
  where
    limited = Text.pack "limited"
    bigEnough name = maybe True (>= RuleFamily.Engine.minimumDeckSize RuleFamily.Limited) (Map.lookup name (DeckBrowser.sizes (browser lobby)))

-- | The decks the bot may play against yours: every deck, until yours is
-- chosen.
opponents :: Lobby -> Seq.Seq Text.Text
opponents lobby = maybe (decks lobby) (\one -> Seq.filter (together lobby one) (decks lobby)) (mine lobby)

-- | The fewest cards a bot's random deck holds against your deck of this
-- many: as many, up to Limited's minimum, so a demo deck meets only its like.
fewestDealt :: Natural -> Natural
fewestDealt yours = min yours (RuleFamily.Engine.minimumDeckSize RuleFamily.Limited)

-- | A bot's deck dealt at random from these, `roll` picking one: one that
-- `keeps` refuses is dropped and the rest rolled again; nothing once none
-- is left.
dealt :: (Monad m) => (Seq.Seq Text.Text -> m (Maybe Text.Text)) -> (Text.Text -> m Bool) -> Seq.Seq Text.Text -> m (Maybe Text.Text)
dealt roll keeps among =
  let next name = keeps name >>= \kept -> if kept then pure (Just name) else dealt roll keeps (Seq.filter (/= name) among)
   in roll among >>= maybe (pure Nothing) next

-- | A card a planar deck may hold (CR 901.3): its name and whether it is a
-- phenomenon card; nothing for any other card, or a synthetic one.
planarCardOf :: Card.Card -> Maybe (CardName.CardName, Bool)
planarCardOf c =
  let face = EngineCard.combined c
      types = TypeLine.types (Face.typeLine face)
      phenomenon = Set.member CardType.Phenomenon types
   in if not (Card.synthetic c) && (phenomenon || Set.member CardType.Plane types) then Just (Face.name face, phenomenon) else Nothing

-- | These, in an order `pick` makes: `pick n` names one of the first `n`
-- left, from zero.
shuffledBy :: (Monad m) => (Int -> m Int) -> [a] -> m [a]
shuffledBy pick among = case among of
  [] -> pure []
  _ -> do
    i <- pick (length among)
    case splitAt i among of
      (before, chosen' : after) -> (chosen' :) <$> shuffledBy pick (before <> after)
      (before, []) -> pure before

-- | CR 901.3: the cards of a planar deck dealt from these, in order: the
-- first ten, a phenomenon past the second passed over; and the cards left,
-- in order. Nothing where too few are.
planarHand :: [(CardName.CardName, Bool)] -> Maybe ([CardName.CardName], [(CardName.CardName, Bool)])
planarHand = go [] (0 :: Int) []
  where
    go hand phenomena passed rest
      | length hand == 10 = Just (reverse hand, reverse passed <> rest)
      | otherwise = case rest of
          [] -> Nothing
          card@(name, phenomenon) : more
            | phenomenon && phenomena >= 2 -> go hand phenomena (card : passed) more
            | otherwise -> go (name : hand) (if phenomenon then phenomena + 1 else phenomena) passed more

-- | The names of the supplementary deck of this kind a deck list brings,
-- one per card.
sideNames :: SideDeck.SideDeck -> Text.Text -> [CardName.CardName]
sideNames kind contents = [card | (DeckList.Supplementary kind', n, card) <- sectioned contents, kind' == kind, _ <- [1 .. n]]

-- | A seat's list with a supplementary deck of this kind added: these
-- cards, one line each.
withSide :: SideDeck.SideDeck -> [CardName.CardName] -> (Text.Text, Text.Text) -> (Text.Text, Text.Text)
withSide kind cards (name, contents) = (name, Text.unlines (Text.stripEnd contents : Text.empty : DeckList.heading (DeckList.Supplementary kind) : [Text.pack "1 " <> CardName.unwrap card | card <- cards]))

-- | A seat's deck as Start's refusals name it.
seatLabel :: (Text.Text, Text.Text) -> Text.Text
seatLabel seat@(name, _) = DeckBrowser.label (DeckBrowser.titleIn seat) name

-- | The seats' lists, each a deck's name and its list, as Start deals them.
-- With Planechase off, refused where one brings a planar deck.
-- With it on, each list bringing none given a planar deck (planarHand) from
-- `pool` in the order `shuffle` puts it: its own shuffle each, or, under CR
-- 901.15a's single planar deck, one shuffle shared, of the names no list
-- brings, so the communal deck repeats none. Refused, saying why, where the
-- pool runs short.
planarDealt :: (Monad m) => ([(CardName.CardName, Bool)] -> m [(CardName.CardName, Bool)]) -> Options -> [(CardName.CardName, Bool)] -> NonEmpty.NonEmpty (Text.Text, Text.Text) -> m (Either Text.Text (NonEmpty.NonEmpty (Text.Text, Text.Text)))
planarDealt shuffle chosen' pool seats
  | not (planechase chosen') = pure $ case [seat | seat <- NonEmpty.toList seats, brings seat] of
      seat : _ -> Left (Text.concat [Text.pack "Planechase is off, but ", seatLabel seat, Text.pack " brings a planar deck: turn Planechase on, or drop its Planar Deck section."])
      [] -> Right seats
  | singlePlanarDeck chosen' = do
      let brought = Set.fromList (foldMap (sideNames SideDeck.Planar . snd) seats)
          step left seat
            | brings seat = (left, Right seat)
            | otherwise = maybe (left, Left short) (\(hand, rest) -> (rest, Right (withSide SideDeck.Planar hand seat))) (planarHand left)
      order <- shuffle [card | card@(name, _) <- pool, Set.notMember name brought]
      pure (sequence (snd (Traversable.mapAccumL step order seats)))
  | otherwise = sequence <$> traverse (\seat -> if brings seat then pure (Right seat) else maybe (Left short) (\(hand, _) -> Right (withSide SideDeck.Planar hand seat)) . planarHand <$> shuffle pool) seats
  where
    brings = not . null . sideNames SideDeck.Planar . snd
    short = Text.pack ("The pool's " <> show (length pool) <> " planes and phenomena can't deal " <> (if singlePlanarDeck chosen' then "one planar deck for " <> show (length seats) <> " players" else "every seat a planar deck") <> ": turn the single planar deck off, or seat fewer players.")

-- | A card a scheme deck may hold (CR 904.3): its name; nothing for any
-- other card, or a synthetic one.
schemeCardOf :: Card.Card -> Maybe CardName.CardName
schemeCardOf c =
  let face = EngineCard.combined c
   in if not (Card.synthetic c) && Set.member CardType.Scheme (TypeLine.types (Face.typeLine face)) then Just (Face.name face) else Nothing

-- | CR 904.3: how many cards a dealt scheme deck holds, at most two of a
-- name.
schemeDeckSize :: Int
schemeDeckSize = 20

-- | The seats' lists, each a deck's name and its list, as Start deals them.
-- With Archenemy off, refused where one brings a scheme deck. With it on,
-- refused where a seat but the archenemy's brings one, since the engine
-- makes an archenemy of the one player bringing a scheme deck
-- (Parun.Engine.Archenemy.sole); and the archenemy's list, bringing none,
-- given the first 'schemeDeckSize' of two of each name in `pool`, in the
-- order `shuffle` puts them (CR 904.3). Refused, saying why, where the pool
-- runs short.
schemeDealt :: (Monad m) => ([CardName.CardName] -> m [CardName.CardName]) -> Options -> [CardName.CardName] -> NonEmpty.NonEmpty (Text.Text, Text.Text) -> m (Either Text.Text (NonEmpty.NonEmpty (Text.Text, Text.Text)))
schemeDealt shuffle chosen' pool seats = case archenemy chosen' of
  Nothing -> pure $ case filter brings (NonEmpty.toList seats) of
    seat : _ -> Left (Text.concat [Text.pack "Archenemy is off, but ", seatLabel seat, Text.pack " brings a scheme deck: turn Archenemy on, or drop its Scheme Deck section."])
    [] -> Right seats
  Just place -> case [seat | (i, seat) <- zip [0 ..] (NonEmpty.toList seats), i /= place, brings seat] of
    seat : _ -> pure (Left (Text.concat [Text.pack "Only the archenemy plays a scheme deck, but ", seatLabel seat, Text.pack " brings one: make it the archenemy, or drop its Scheme Deck section."]))
    [] -> do
      order <- shuffle (names <> names)
      let deck = take schemeDeckSize order
          deal i seat
            | i /= place || brings seat = Right seat
            | length deck < schemeDeckSize = Left short
            | otherwise = Right (withSide SideDeck.Scheme deck seat)
      pure (traverse (uncurry deal) (NonEmpty.zip (0 NonEmpty.:| [1 ..]) seats))
  where
    names = ListUtils.nubOrd pool
    brings = not . null . sideNames SideDeck.Scheme . snd
    short = Text.pack ("The pool's " <> show (length names) <> " schemes can't deal a scheme deck of " <> show schemeDeckSize <> ", two of a name at most: bring one in the archenemy's list.")

-- | A card's colours as a deck shows them: those of the mana symbols in
-- its faces' costs.
cardColours :: Card.Card -> Set.Set Color.Color
cardColours c = foldMap (foldMap costColours . Face.manaCost) (Card.faces c)

-- | The colours a cost's symbols name (CR 105.2, 202.2).
costColours :: ManaCost.ManaCost -> Set.Set Color.Color
costColours printed =
  let ofType t = case t of
        ManaType.Colored c -> Set.singleton c
        ManaType.Colorless -> Set.empty
      symbol sym = case sym of
        ManaSymbol.Generic _ -> Set.empty
        ManaSymbol.OfType t -> ofType t
        ManaSymbol.Hybrid h -> Set.union (ofType (Hybrid.left h)) (ofType (Hybrid.right h))
        ManaSymbol.MonocoloredHybrid t -> ofType t
        ManaSymbol.Phyrexian c -> Set.singleton c
        ManaSymbol.HybridPhyrexian h -> Set.fromList [HybridPhyrexian.left h, HybridPhyrexian.right h]
        ManaSymbol.Snow -> Set.empty
        ManaSymbol.Variable -> Set.empty
   in foldMap symbol (ManaCost.unwrap printed)

-- | A deck list's colours: those of each card a line names, by `colours`.
deckColours :: (CardName.CardName -> Set.Set Color.Color) -> Text.Text -> Set.Set Color.Color
deckColours colours contents = foldMap colours (Set.fromList (fmap snd (entries contents)))

-- | How many cards a deck list's main deck holds: its lines before any
-- other section's header, and those under the main deck's.
mainCount :: Text.Text -> Natural
mainCount = sum . fmap fst . mainEntries

-- | A deck list's main deck ('entries'): its lines before any other
-- section's header, and those under the main deck's.
mainEntries :: Text.Text -> [(Natural, CardName.CardName)]
mainEntries contents = [(n, card) | (DeckList.Main, n, card) <- sectioned contents]

-- | A deck list's 'entries', each with the section the headers before it
-- name: the main deck before any.
sectioned :: Text.Text -> [(DeckList.Section, Natural, CardName.CardName)]
sectioned contents =
  let step (section, kept) line = case DeckList.sectionNamed (Text.strip line) of
        Just next -> (next, kept)
        Nothing -> (section, kept <> [(section, n, card) | (n, card) <- entries line])
   in snd (List.foldl' step (DeckList.Main, []) (Text.lines contents))

-- | A deck list's lines that name a card ("4 Forest", "1 Shock (M19) 156"),
-- each as its count and the card; other lines skipped.
entries :: Text.Text -> [(Natural, CardName.CardName)]
entries contents =
  let entry line = do
        let (count, rest) = Text.span Char.isDigit (Text.strip line)
        n <- Read.readMaybe (Text.unpack count)
        pure (n, CardName.MkCardName (Text.strip (fst (Text.breakOn (Text.pack " (") rest))))
   in Maybe.mapMaybe entry (Text.lines contents)

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

-- | The lobby after an event, the value typed being `typed`. A pick goes to
-- the seat being chosen for and never moves it on; picking your deck drops
-- each bot's deck under another format, and a bot is offered only its
-- 'opponents'.
update :: Event -> Text.Text -> Lobby -> Lobby
update event typed lobby
  | paused lobby && abandons event typed lobby = lobby {abandoning = Just (event, typed)}
  | otherwise = settled lobby (updated event typed lobby)

-- | Whether this event, the value typed being `typed`, done, abandons a game
-- in progress: starting a game, or inviting a player, whose link needs the
-- data channels the game set aside holds (Parun.Browser.aside).
abandons :: Event -> Text.Text -> Lobby -> Bool
abandons event typed lobby = case event of
  MkStart -> Maybe.isJust (playingOn lobby) || Either.isRight (startable lobby)
  _ -> nextLink (updated event typed lobby) /= nextLink lobby

-- | The lobby once the game in progress is abandoned, and the event asked
-- about and its value typed, to do next; nothing where none was asked
-- about.
abandoned :: Lobby -> Maybe (Lobby, (Event, Text.Text))
abandoned lobby = (,) lobby {paused = False, abandoning = Nothing} <$> abandoning lobby

-- | 'update' before any ready flag is cleared; a game played on changes
-- only as 'seatedOn' has it, each invite following it ('invitedFor'), by
-- its clock, and by an invite's reply code and you being ready.
updated :: Event -> Text.Text -> Lobby -> Lobby
updated event typed lobby
  | Just table <- playingOn lobby = case event of
      MkClock -> fresh
      MkCancel -> fresh
      MkReplyCode _ -> fresh
      MkReady -> fresh
      _ -> invitedFor lobby {playingOn = Just (seatedOn event typed table)}
  | otherwise = fresh
  where
    fresh = newGameUpdated event typed lobby

-- | 'updated' for a new game.
newGameUpdated :: Event -> Text.Text -> Lobby -> Lobby
newGameUpdated event typed lobby = case event of
  MkPick name
    | Foldable.notElem name (decks lobby) -> lobby
    | MkTheirs i <- choosing lobby ->
        if Foldable.notElem name (opponents lobby) then lobby else lobby {theirs = [if j == i then Just name else deck | (j, deck) <- zip [0 ..] (theirs lobby)]}
    | otherwise -> lobby {mine = Just name, theirs = fmap (Monad.mfilter (together lobby name)) (theirs lobby)}
  MkChoose slot
    | MkTheirs i <- slot, Map.member i (invites lobby) -> lobby
    | otherwise -> lobby {choosing = slot}
  MkBrowse e -> browsed (DeckBrowser.update e typed) lobby
  MkRandom -> lobby
  MkAdd occupant
    | i < mostOthers ->
        let added = lobby {theirs = theirs lobby <> [Nothing], choosing = MkTheirs i}
         in case occupant of
              MkBot -> added
              MkInvited -> added {choosing = choosing lobby, invites = Map.insert i (newInvite (nextLink lobby)) (invites lobby), nextLink = nextLink lobby + 1}
              MkHere sitting -> added {locals = Map.insert i sitting (locals lobby)}
    | otherwise -> lobby
    where
      i = length (theirs lobby)
  MkDrop i
    | removable lobby i ->
        let moved j = if j > i then j - 1 else j
            placed place = if place > i + 1 then place - 1 else place
         in lobby
              { theirs = [deck | (j, deck) <- zip [0 ..] (theirs lobby), j /= i],
                choosing = case choosing lobby of
                  MkTheirs j | j == i -> MkMine | otherwise -> MkTheirs (moved j)
                  MkMine -> MkMine,
                invites = Map.mapKeys moved (Map.delete i (invites lobby)),
                locals = Map.mapKeys moved (Map.delete i (locals lobby)),
                bots = Map.mapKeys moved (Map.delete i (bots lobby)),
                -- Yours is the table's first place, before each in 'theirs'.
                teams = Map.mapKeys placed (Map.delete (i + 1) (teams lobby)),
                emperors = placed <$> Map.filter (/= i + 1) (emperors lobby),
                -- The archenemy unseated, yours is the archenemy's seat.
                options = (options lobby) {archenemy = (\place -> if place == i + 1 then 0 else placed place) <$> archenemy (options lobby)}
              }
    | otherwise -> lobby
  MkStart -> lobby
  MkOpenBuilder -> lobby
  MkEdit _ -> lobby
  MkReplyCode i -> lobby {invites = Map.adjust (\invite -> invite {replyCode = typed}) i (invites lobby)}
  MkConnect _ -> lobby
  MkReady -> lobby {ready = not (ready lobby)}
  MkFill _ -> lobby
  MkTakeSeat _ -> lobby
  MkDifficulty i -> maybe lobby (\d -> styled i (\(_, p) -> (d, p))) (Opponent.named Opponent.difficultyName (Text.unpack typed))
  MkPersona i -> maybe lobby (\p -> styled i (\(d, _) -> (d, p))) (Opponent.named Opponent.personaName (Text.unpack typed))
  MkAbandon -> lobby
  MkResume -> lobby
  MkCancel -> lobby {abandoning = Nothing}
  MkAnte -> optioned (\o -> o {ante = not (ante o)})
  MkPlanechase -> optioned (\o -> o {planechase = not (planechase o)})
  MkSinglePlanarDeck -> optioned (\o -> o {singlePlanarDeck = not (singlePlanarDeck o)})
  MkArchenemy -> optioned (\o -> o {archenemy = maybe (Just 0) (const Nothing) (archenemy o)})
  MkArchenemySeat
    | archenemyGame lobby, Just place <- Read.readMaybe (Text.unpack typed), place >= 0, Int.toNaturalSaturating place < seatCount lobby -> optioned (\o -> o {archenemy = Just place})
    | otherwise -> lobby
  MkAttackOption -> maybe lobby (\a -> optioned (\o -> o {attackOption = a})) (valued attackOptions)
  MkRangeOfInfluence -> maybe lobby (\r -> optioned (\o -> o {rangeOfInfluence = r})) (valued ranges)
  MkDeployCreatures -> optioned (\o -> o {deployCreatures = not (deployCreatures o)})
  MkClock -> maybe lobby (\c -> optioned (\o -> o {clock = c})) (valued Clock.choices)
  MkVariant -> maybe lobby (\v -> (optioned (\o -> o {variant = v})) {teams = Map.empty, emperors = Map.empty}) (valued variants)
  MkTeam place
    | place >= 0 && Int.toNaturalSaturating place < seatCount lobby, Just n <- Read.readMaybe (Text.unpack typed), Just team <- Integer.toNatural n, team < teamChoices lobby -> lobby {teams = Map.insert place team (teams lobby)}
    | otherwise -> lobby
  MkEmperorSeat team
    | variantOf lobby == MkEmperor, Just place <- Read.readMaybe (Text.unpack typed), place `elem` membersOf lobby team -> lobby {emperors = Map.insert team place (emperors lobby)}
    | otherwise -> lobby
  where
    optioned f = lobby {options = f (options lobby)}
    valued :: [(a, (String, String))] -> Maybe a
    valued among = Maybe.listToMaybe [option | (option, (value, _)) <- among, value == Text.unpack typed]
    styled i f
      | removable lobby i = lobby {bots = Map.insert i (f (botAt i (bots lobby))) (bots lobby)}
      | otherwise = lobby

-- | Whether the seat at this place in 'theirs' may be unseated: it is
-- seated.
removable :: Lobby -> Int -> Bool
removable lobby i = i >= 0 && i < length (theirs lobby)

-- | The lobby once the page of the player invited to this slot said this
-- (Parun.Web.Link): their deck, but in a game played on, whose decks are
-- fixed, and whether they are ready.
heard :: Int -> Link.Message -> Lobby -> Lobby
heard i message lobby =
  let adjust f = lobby {invites = Map.adjust f i (invites lobby)}
   in settled lobby $ case message of
        Link.MkHello name contents
          | Maybe.isNothing (playingOn lobby) -> adjust (\invite -> invite {guestDeck = Just (name, contents)})
        Link.MkReady on -> adjust (\invite -> invite {guestReady = on})
        _ -> lobby

-- | An invited slot's link made, connected, or ended, saying why.
linked :: Int -> (Invite -> Invite) -> Lobby -> Lobby
linked i f lobby = settled lobby lobby {invites = Map.adjust f i (invites lobby)}

-- | The lobby after a change, every ready flag and Start's refusal cleared
-- where the match-up changed: a deck, a seat, an invite or an option.
settled :: Lobby -> Lobby -> Lobby
settled before after
  | matchupOf before == matchupOf after = after
  | otherwise = (unready after) {refusal = Nothing}
  where
    matchupOf l = (mine l, theirs l, locals l, options l, teams l, emperors l, playingOn l, fmap (\invite -> (connected invite, fmap fst (guestDeck invite), fmap snd (guestDeck invite))) (invites l))

-- | The lobby with every ready flag cleared.
unready :: Lobby -> Lobby
unready lobby = lobby {ready = False, invites = fmap (\invite -> invite {guestReady = False}) (invites lobby)}

-- | The lobby a Rematch of a game with players invited returns to: the
-- decks played, your own first, each bot's slot in `invited` held by its
-- player, still connected over the link the game used (its number there),
-- each in `here` played on this screen as it was, and nobody ready.
rematch :: NonEmpty.NonEmpty (Text.Text, Text.Text) -> Map.Map Int Int -> Map.Map Int Link.Sitting -> Lobby
rematch (yours NonEmpty.:| rest) invited here =
  new
    { mine = Just (fst yours),
      theirs = [if Map.member i invited then Nothing else Just name | (i, (name, _)) <- zip [0 ..] rest],
      invites = Map.fromList [(i, (newInvite n) {connected = True, guestDeck = Just seat}) | (i, seat) <- zip [0 ..] rest, Just n <- [Map.lookup i invited]],
      nextLink = foldr (max . (+ 1)) 0 invited,
      locals = Map.withoutKeys here (Map.keysSet invited),
      -- Your deck's and each bot's name, as their lists give them, until
      -- the decks are read again.
      browser = (browser new) {DeckBrowser.titles = foldMap DeckBrowser.titleIn (yours : [seat | (i, seat) <- zip [0 ..] rest, Map.notMember i invited])}
    }

-- | Whether a game with the players invited starts: there is one, the
-- table can play the variant ('settings'), your deck is chosen, but in a
-- game played on, whose decks are fixed, and every player is ready
-- ('guestsReady').
starts :: Lobby -> Bool
starts lobby =
  not (Map.null (invites lobby))
    && Either.isRight (settings lobby)
    && ready lobby
    && (Maybe.isJust (mine lobby) || Maybe.isJust (playingOn lobby))
    && guestsReady lobby

-- | Whether every player invited is connected, with a deck ('guestDecked'),
-- and ready, so that you being ready starts the game.
guestsReady :: Lobby -> Bool
guestsReady lobby = all (\invite -> connected invite && guestDecked lobby invite && guestReady invite) (invites lobby)

-- | Whether this invited player has the deck they play: one they chose, or
-- in a game played on, whose decks are fixed, the seat's own.
guestDecked :: Lobby -> Invite -> Bool
guestDecked lobby invite = Maybe.isJust (playingOn lobby) || Maybe.isJust (guestDeck invite)

-- | The match-up as the player invited to this slot is shown it, each seat
-- with its team where the game has teams (CR 808.1), each deck coloured by
-- its name (`named`) or, a guest's, by its list (`listed`); in a game
-- played on, 'playOnPlaces'.
placesFor :: (Text.Text -> Set.Set Color.Color) -> (Text.Text -> Set.Set Color.Color) -> Int -> Lobby -> [Link.Place]
placesFor named listed i lobby
  | Just table <- playingOn lobby = playOnPlaces i lobby table
  | otherwise = newGamePlaces named listed i lobby

-- | A game played on as the player invited to the seat at place `i` at the
-- table is shown it: each seat by its name, with its deck as the replay
-- names it, uncoloured; whether the host and each player invited is ready;
-- and each seat a bot or the goldfish plays marked a bot's.
playOnPlaces :: Int -> Lobby -> PlayOn -> [Link.Place]
playOnPlaces i lobby table =
  [ Link.MkPlace (if place == i then Text.pack "You" else Wording.player pid) deck readiness botted Set.empty
  | (place, deck) <- zip [0 ..] (places table),
    let pid = PlayerId.MkPlayerId (Int.toNaturalSaturating place)
        hosting = pid == Link.host (begun table)
        readiness = if hosting then Just (ready lobby) else guestReady <$> Map.lookup place (invites lobby)
        botted =
          not hosting && case Link.fillingAt (begun table) pid of
            Link.MkFilledByBot {} -> True
            Link.MkFilledByGoldfish -> True
            Link.MkFilledHere _ -> False
            Link.MkFilledByInvite -> False
  ]

-- | 'placesFor' a new game.
newGamePlaces :: (Text.Text -> Set.Set Color.Color) -> (Text.Text -> Set.Set Color.Color) -> Int -> Lobby -> [Link.Place]
newGamePlaces named listed i lobby =
  Link.MkPlace (onTeam 0 (Text.pack "Player 1")) (maybe Link.unchosen (DeckBrowser.label (DeckBrowser.titles (browser lobby))) (mine lobby)) (Just (ready lobby)) False (foldMap named (mine lobby))
    : [ case Map.lookup j (invites lobby) of
          Just invite -> Link.MkPlace (onTeam (j + 1) (if j == i then Text.pack "You" else Text.pack (seatName (MkTheirs j)))) (maybe Link.unchosen (\deck -> DeckBrowser.label (DeckBrowser.titleIn deck) (fst deck)) (guestDeck invite)) (Just (guestReady invite)) False (foldMap (listed . snd) (guestDeck invite))
          Nothing -> Link.MkPlace (onTeam (j + 1) (Text.pack (seatName (MkTheirs j)))) (maybe (Text.pack "Random deck") (DeckBrowser.label (DeckBrowser.titles (browser lobby))) held) Nothing (Map.notMember j (locals lobby)) (foldMap named held)
      | (j, held) <- zip [0 ..] (theirs lobby)
      ]
  where
    onTeam place name
      | teamed lobby = name <> Text.pack ", " <> Wording.team (TeamId.MkTeamId (teamAt lobby place)) <> Text.pack (if crowned place then ", emperor" else "")
      | otherwise = name
    crowned place = Emperors.isEmperor (seatEmperors lobby) (Command.idOf (Int.toNaturalSaturating place + 1))

-- | The game's options as an invited player is told them, each a label and
-- its value as 'optionsView' shows them: in a game played on, the clock, its
-- only option.
told :: Lobby -> [(Text.Text, Text.Text)]
told lobby
  | Maybe.isJust (playingOn lobby) = [timed]
  | otherwise =
      [(Text.pack "Variant", labelIn variants (variantOf lobby)) | multiplayer (seatCount lobby), not (archenemyGame lobby)]
        <> [(Text.pack "Play for ante", yesNo (ante chosen')), (Text.pack "Planechase", yesNo (planechase chosen'))]
        <> [(Text.pack "Single planar deck", yesNo (singlePlanarDeck chosen')) | planechase chosen']
        <> [(Text.pack "Archenemy", maybe (Text.pack "No") (\place -> Text.pack ("Player " <> show (place + 1))) (archenemy chosen'))]
        <> [timed]
        <> [(Text.pack "Deploy creatures", yesNo (deployCreatures chosen')) | deployOffered lobby]
        <> ( if chosesAttacks lobby
               then [(Text.pack "Attacks", labelIn attackOptions (attackOption chosen')), (Text.pack "Range of influence", labelIn ranges (rangeOfInfluence chosen'))]
               else []
           )
  where
    timed = (Text.pack "Clock", labelIn Clock.choices (clock chosen'))
    chosen' = options lobby
    yesNo on = Text.pack (if on then "Yes" else "No")
    labelIn :: (Eq a) => [(a, (String, String))] -> a -> Text.Text
    labelIn choices option = Text.pack (maybe "" snd (List.lookup option choices))

-- | Your deck, to start a game with; or why it may not: the problems a
-- check found with it, or none while there is no deck.
startable :: Lobby -> Either [Text.Text] Text.Text
startable lobby = case mine lobby of
  Nothing -> Left []
  Just name -> maybe (Right name) Left (Map.lookup name (DeckBrowser.illegal (browser lobby)))

-- | The deck chosen for a slot, if any.
chosen :: Lobby -> Slot -> Maybe Text.Text
chosen lobby slot = case slot of
  MkMine -> mine lobby
  MkTheirs i -> Monad.join (Maybe.listToMaybe (drop i (theirs lobby)))

-- | The decks the seat being chosen for may play: every deck for yours, its
-- 'opponents' for a bot's.
offered :: Lobby -> Seq.Seq Text.Text
offered lobby = if choosing lobby == MkMine then decks lobby else opponents lobby

-- | The decks shown to the seat being chosen for (DeckBrowser.shown).
shown :: Lobby -> [Text.Text]
shown lobby = DeckBrowser.shown (offered lobby) (browser lobby)

-- | A seat's name, as the game will call it (Parun.Play.Wording.player):
-- "You", else "Player 2" by its seat, whoever plays it.
seatName :: Slot -> String
seatName slot = case slot of
  MkMine -> "You"
  MkTheirs i -> "Player " <> show (i + 2)

-- | What stops Start, or Ready with players invited: the decks unread, your
-- deck unchosen or breaking its format, or a table the variant refuses
-- ('settings'); nothing for a game played on, its decks the replay's. A bot
-- always has a deck to draw, since your deck is among its 'opponents'.
blocking :: Lobby -> Maybe String
blocking lobby
  | Maybe.isJust (playingOn lobby) = Nothing
  | DeckBrowser.reading (browser lobby) = Just "Reading the decks\x2026"
  | Maybe.isNothing (mine lobby) = Just "Choose your deck to start."
  | Left (_ : _) <- startable lobby = Just "Your deck breaks its format:"
  | Left why <- settings lobby = Just why
  | otherwise = Nothing

-- | Why the game may not start yet: what is 'blocking', else who invited
-- is still to connect, choose or be ready, which Ready does not wait on.
waiting :: Lobby -> Maybe String
waiting lobby = case blocking lobby of
  Just why -> Just why
  Nothing -> case [(i, invite) | (i, invite) <- Map.toList (invites lobby), not (connected invite && guestDecked lobby invite && guestReady invite)] of
    (i, invite) : _
      | not (connected invite) -> Just (invitedName lobby i <> " is still to connect.")
      | not (guestDecked lobby invite) -> Just (invitedName lobby i <> " is choosing a deck.")
      | otherwise -> Just (invitedName lobby i <> " is not ready yet.")
    [] -> Nothing

-- | The name of the seat invite `i` takes: by its place in 'theirs', or in a
-- game played on by its place at the table (Parun.Play.Wording.player).
invitedName :: Lobby -> Int -> String
invitedName lobby i
  | Maybe.isJust (playingOn lobby) = Text.unpack (Wording.player (PlayerId.MkPlayerId (Int.toNaturalSaturating i)))
  | otherwise = seatName (MkTheirs i)

-- | Whether Start, or Ready with players invited, may be pressed.
pressable :: Lobby -> Bool
pressable = Maybe.isNothing . blocking

view :: Lobby -> Html.Html Event
view lobby = maybe (newGameView lobby) (playOnView lobby) (playingOn lobby)

-- | The lobby of a new game: the seats, the decks and the deck chosen.
newGameView :: Lobby -> Html.Html Event
newGameView lobby =
  Element.keyed
    "main"
    ["lobby", "new-game"]
    []
    [ (Text.pack "seats", seatsView lobby),
      (Text.pack "browser", browserView lobby),
      (Text.pack "detail", detailView lobby)
    ]

-- | The lobby of a game played on: each seat the replay has, by its
-- number, with its deck; yours marked, and each other's filling, a bot's
-- difficulty and persona, an invited player's link (inviteSteps) and
-- whether they are ready, and the way to take it, but an invited player's;
-- the clock, its only option, the others being the replay's; and Start, or
-- Ready with players invited, at the foot.
playOnView :: Lobby -> PlayOn -> Html.Html Event
playOnView lobby table =
  Element.el "main" ["lobby", "new-game", "play-on"] [] . pure . Element.el "section" ["seats"] [Element.attr "aria-label" (Text.pack "Seats")] $
    [ Element.el "h1" [] [] [Element.string "Play on"],
      Element.el "p" ["muted"] [] [Element.string "From the replay's position, each seat with the deck it had there."],
      Element.el "span" ["eyebrow"] [] [Element.string "At the table"]
    ]
      <> List.intersperse (Element.el "span" ["eyebrow", "versus"] [] [Element.string "versus"]) (zipWith seat [0 ..] (places table))
      <> [ Element.el "fieldset" ["game-options"] [] [Element.el "legend" ["eyebrow"] [] [Element.string "Game options"], clockMenu lobby],
           startView lobby
         ]
  where
    game = begun table
    seat :: Int -> Text.Text -> Html.Html Event
    seat place deck =
      let pid = PlayerId.MkPlayerId (Int.toNaturalSaturating place)
          yours = pid == Link.host game
          filling = Link.fillingAt game pid
          field = Text.pack ("play-on-" <> show place)
          avatar
            | yours = Element.string "Y"
            | otherwise = case filling of
                Link.MkFilledByBot {} -> Icon.icon [] Icon.MkBot
                Link.MkFilledHere Link.MkYours -> Element.string "Y"
                Link.MkFilledHere Link.MkHandedOver -> Element.string "P"
                Link.MkFilledByGoldfish -> Element.string "G"
                Link.MkFilledByInvite -> Element.string "P"
          invited = Map.lookup place (invites lobby)
       in Element.el "div" (["lobby-seat", "play-on-seat"] <> ["on" | yours]) [] $
            Element.el
              "div"
              ["seat-pick"]
              []
              [ Element.el
                  "span"
                  ["seat-head"]
                  []
                  ( [Element.el "span" ["avatar"] [] [avatar], Element.el "strong" ["seat-name"] [] [Element.text (Wording.player pid)]]
                      <> [Element.el "span" ["chip", "choosing"] [] [Element.string "You"] | yours]
                      <> [Element.el "span" ["chip"] [] [Element.string (if guestReady invite then "Ready \x2713" else "Not ready")] | Just invite <- [invited], connected invite]
                  ),
                Element.el "span" ["seat-deck"] [] [Element.el "span" ["seat-deck-name"] [] [Element.text deck]]
              ]
              : if yours
                then []
                else
                  [ Element.el
                      "div"
                      ["seat-filling"]
                      []
                      [ Element.el "label" [] [Element.attr "for" field] [Element.string "Played by"],
                        Element.el
                          "select"
                          ["deck-menu"]
                          [Element.attr "id" field, Element.on "change" (MkFill place)]
                          [Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | sameKind kind filling]) [Element.string label] | (kind, (value, label)) <- fillingChoices]
                      ]
                  ]
                    <> [BotSettings.botView ("play-on-" <> show place) (MkDifficulty place) (MkPersona place) (level, style) | Link.MkFilledByBot level style <- [filling]]
                    <> foldMap (inviteSteps place) invited
                    <> [Element.el "div" ["seat-actions"] [] [Element.el "button" ["quiet", "take-seat"] [Element.attr "type" (Text.pack "button"), Element.on "click" (MkTakeSeat place), Element.attr "aria-label" (Text.pack "Take " <> Wording.player pid)] [Element.string "Take this seat"]] | Maybe.isNothing invited]

-- | The menu of each seat's time on its clock (Clock.choices).
clockMenu :: Lobby -> Html.Html Event
clockMenu lobby = optionMenu "option-clock" MkClock "Clock" [(value, label, option == clock (options lobby)) | (option, (value, label)) <- Clock.choices]

-- | A game option's menu: its field's id, the event its change sends, its
-- label, and each choice's value, label and whether it is chosen.
optionMenu :: String -> Event -> String -> [(String, String, Bool)] -> Html.Html Event
optionMenu field event label choices =
  Element.el
    "div"
    ["game-option"]
    []
    [ Element.el "label" [] [Element.attr "for" (Text.pack field)] [Element.string label],
      Element.el
        "select"
        ["deck-menu"]
        [Element.attr "id" (Text.pack field), Element.on "change" event]
        [Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | on]) [Element.string text] | (value, text, on) <- choices]
    ]

-- | The seats: yours, then each bot's with how it plays, invited player's,
-- player's here or one you play too, each with its team where the game has
-- teams; the way to unseat each that is
-- 'removable'; a seat to add, by who fills it, while there is room; the
-- game's options; and Start at the foot, saying what it waits on.
seatsView :: Lobby -> Html.Html Event
seatsView lobby =
  Element.el "section" ["seats"] [Element.attr "aria-label" (Text.pack "Seats")] $
    [ Element.el "h1" [] [] [Element.string "New game"],
      Element.el "span" ["eyebrow"] [] [Element.string "At the table"],
      seat MkMine (Element.string "Y") (mine lobby) (teamMenu 0) []
    ]
      <> concat
        [ Element.el "span" ["eyebrow", "versus"] [] [Element.string "versus"]
            : case Map.lookup i (invites lobby) of
              Just invite -> [inviteView i invite (teamMenu (i + 1) <> remove i)]
              Nothing -> case Map.lookup i (locals lobby) of
                Just sitting -> [seat (MkTheirs i) (Element.string (if sitting == Link.MkYours then "Y" else "P")) held (teamMenu (i + 1)) (remove i)]
                Nothing -> [seat (MkTheirs i) (Icon.icon [] Icon.MkBot) held (BotSettings.botView (show i) (MkDifficulty i) (MkPersona i) (botAt i (bots lobby)) : teamMenu (i + 1)) (remove i)]
        | (i, held) <- zip [0 ..] others
        ]
      <> [ Element.el
             "div"
             ["seat-tools"]
             [Element.attr "role" (Text.pack "group"), Element.attr "aria-labelledby" (Text.pack "add-seat")]
             [ Element.el "span" ["eyebrow"] [Element.attr "id" (Text.pack "add-seat")] [Element.string (if null others then "Add a seat, or play alone" else "Add a seat")],
               Element.el
                 "div"
                 ["seat-choices"]
                 []
                 [ Element.button ["add-bot"] (MkAdd MkBot) [Element.string "Bot"],
                   Element.button ["add-here"] (MkAdd (MkHere Link.MkHandedOver)) [Element.string "Player here"],
                   Element.button ["add-invite"] (MkAdd MkInvited) [Element.string "Invite a player"],
                   Element.button ["add-yours"] (MkAdd (MkHere Link.MkYours)) [Element.string "Yourself"]
                 ]
             ]
         | length others < mostOthers
         ]
      <> [optionsView lobby, startView lobby]
  where
    others = theirs lobby
    -- The seat's team where the game has teams, by its place at the table.
    teamMenu place =
      [ Element.el
          "div"
          ["seat-team"]
          []
          [ Element.el "label" [] [Element.attr "for" (Text.pack ("team-" <> show place))] [Element.string "Team"],
            Element.el
              "select"
              ["deck-menu"]
              [Element.attr "id" (Text.pack ("team-" <> show place)), Element.on "change" (MkTeam place)]
              [Element.el "option" [] ([Element.attr "value" (Text.pack (show n))] <> [Element.attr "selected" Text.empty | n == teamAt lobby place]) [Element.text (Wording.team (TeamId.MkTeamId n))] | n <- [0 .. teamChoices lobby - 1]]
          ]
      | teamed lobby
      ]
    remove i = [Element.el "button" ["quiet", "drop-seat"] [Element.attr "type" (Text.pack "button"), Element.on "click" (MkDrop i), Element.attr "aria-label" (Text.pack ("Remove " <> seatName (MkTheirs i)))] [Element.string "Remove"] | removable lobby i]
    seat which avatar held below extra =
      let on = choosing lobby == which
       in Element.el
            "div"
            (["lobby-seat"] <> ["on" | on])
            []
            $ Element.el
              "button"
              ["seat-pick"]
              [Element.attr "type" (Text.pack "button"), Element.on "click" (MkChoose which), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false"))]
              [ Element.el
                  "span"
                  ["seat-head"]
                  []
                  ( [Element.el "span" ["avatar"] [] [avatar], Element.el "strong" ["seat-name"] [] [Element.string (seatName which)]]
                      <> [Element.el "span" ["chip", "bot"] [] [Element.string "Bot"] | MkTheirs i <- [which], Map.notMember i (locals lobby)]
                      <> [Element.el "span" ["chip", "choosing"] [] [Element.string "Choosing a deck"] | on, Maybe.isNothing held]
                  ),
                Element.el "span" ["seat-deck"] [] $ case held of
                  Just name -> [DeckBrowser.pips (Map.findWithDefault Set.empty name (DeckBrowser.palette (browser lobby))), Element.el "span" ["seat-deck-name"] [] [Element.text (snd (DeckBrowser.describe (DeckBrowser.titles (browser lobby)) name))]]
                  Nothing -> [Element.el "span" ["seat-deck-name"] [] [Element.string (if which == MkMine then "No deck yet" else "Random deck")]],
                Element.el "span" ["seat-note", "muted"] [] [Element.string (maybe (if which == MkMine then "Choose one to start" else "Drawn when the game starts") (DeckBrowser.counted (browser lobby)) held)]
              ]
              : below
                <> [Element.el "div" ["seat-actions"] [] extra | not (null extra)]

-- | The game's options: at three seats or more the variant, but in an
-- Archenemy game, and in Emperor each team's emperor; ante, Planechase
-- and, while it is on, the single planar deck, Archenemy and, while it is
-- on, the archenemy's seat, each seat's clock, where it may choose it
-- ('deployOffered') CR 804's deploy creatures option, and where the game
-- chooses them ('chosesAttacks') CR 806.2's attack option and range of
-- influence.
optionsView :: Lobby -> Html.Html Event
optionsView lobby =
  Element.el "fieldset" ["game-options"] [] $
    [Element.el "legend" ["eyebrow"] [] [Element.string "Game options"]]
      <> [optionMenu "option-variant" MkVariant "Variant" [(value, label, option == variant chosen') | (option, (value, label)) <- variants] | multiplayer (seatCount lobby), not (archenemyGame lobby)]
      <> [ optionMenu ("emperor-" <> show team) (MkEmperorSeat team) (Text.unpack (Wording.team (TeamId.MkTeamId team)) <> "'s emperor") [(show place, seatName (if place == 0 then MkMine else MkTheirs (place - 1)), Just place == emperorOf lobby team) | place <- membersOf lobby team]
         | variantOf lobby == MkEmperor,
           team <- teamsSeated lobby
         ]
      <> [ toggle "option-ante" MkAnte (ante chosen') "Play for ante" "Each player antes a random card from their library; the winner owns them all.",
           toggle "option-planechase" MkPlanechase (planechase chosen') "Planechase" "Each seat plays its list's planar deck, or one dealt from the planes and phenomena."
         ]
      <> [toggle "option-single-planar-deck" MkSinglePlanarDeck (singlePlanarDeck chosen') "Single planar deck" "Every seat's planar deck is shuffled into one." | planechase chosen']
      <> [toggle "option-archenemy" MkArchenemy (archenemyGame lobby) "Archenemy" "One seat, the archenemy, plays its list's scheme deck, or one dealt from the schemes, against every other."]
      <> [ optionMenu "option-archenemy-seat" MkArchenemySeat "The archenemy" [(show place, seatName (if place == 0 then MkMine else MkTheirs (place - 1)), Just place == archenemy chosen') | place <- [0 .. Natural.toIntSaturating (seatCount lobby) - 1]]
         | archenemyGame lobby
         ]
      <> [clockMenu lobby]
      <> [toggle "option-deploy-creatures" MkDeployCreatures (deployCreatures chosen') "Deploy creatures" "Each creature can tap to give itself to a teammate, as a sorcery." | deployOffered lobby]
      <> ( if chosesAttacks lobby
             then
               [ optionMenu "option-attack" MkAttackOption "Attacks" [(value, label, option == attackOption chosen') | (option, (value, label)) <- attackOptions],
                 optionMenu "option-range" MkRangeOfInfluence "Range of influence" [(value, label, option == rangeOfInfluence chosen') | (option, (value, label)) <- ranges]
               ]
             else []
         )
  where
    chosen' = options lobby
    toggle :: String -> Event -> Bool -> String -> String -> Html.Html Event
    toggle field event on label note =
      Element.el
        "div"
        ["game-option"]
        []
        [ Element.el "input" [] ([Element.attr "id" (Text.pack field), Element.attr "type" (Text.pack "checkbox"), Element.attr "aria-describedby" (Text.pack (field <> "-note")), Element.on "change" event] <> [Element.attr "checked" Text.empty | on]) [],
          Element.el "label" [] [Element.attr "for" (Text.pack field)] [Element.string label],
          Element.el "p" ["muted"] [Element.attr "id" (Text.pack (field <> "-note"))] [Element.string note]
        ]

-- | Start, or Ready with players invited, and what it waits on; else,
-- asked about, whether to abandon the game in progress.
startView :: Lobby -> Html.Html Event
startView lobby = case abandoning lobby of
  Just asked ->
    Element.el
      "section"
      ["panel", "confirm"]
      [Element.attr "role" (Text.pack "alertdialog"), Element.attr "aria-label" (Text.pack "Abandon the game in progress?")]
      [ Element.el "p" [] [] [Element.string "Starting a new game abandons the one in progress, which Replays won't keep."],
        Element.el
          "div"
          ["choices"]
          []
          [ Element.button ["primary", "resume"] MkResume [Element.string "Resume"],
            Element.button [] MkAbandon [Element.string (if fst asked == MkStart then "Abandon and play" else "Abandon and invite")],
            Element.button [] MkCancel [Element.string "Cancel"]
          ]
      ]
  Nothing -> starting lobby

-- | Start, or Ready with players invited, and what it waits on.
starting :: Lobby -> Html.Html Event
starting lobby =
  let ready'
        | Maybe.isJust (playingOn lobby) = "Every seat is filled. Ready to play on."
        | null (theirs lobby) = "Just you: a goldfish game. Ready to start."
        | otherwise = "Every seat has a deck. Ready to start."
   in Element.el "div" ["lobby-start"] [] $
        [ if Map.null (invites lobby)
            then
              Element.el
                "button"
                ["primary", "start"]
                ([Element.attr "type" (Text.pack "button"), Element.on "click" MkStart, Element.attr "aria-describedby" (Text.pack "start-note")] <> [Element.attr "disabled" Text.empty | not (pressable lobby)])
                [Element.string "Start game", Element.el "kbd" ["keys-only"] [Element.attr "aria-hidden" (Text.pack "true")] [Element.string "\x21b5"]]
            else
              -- The game starts once every player is ready.
              Element.el
                "button"
                (["primary", "start", "ready"] <> ["is-ready" | ready lobby])
                ([Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if ready lobby then "true" else "false")), Element.on "click" MkReady, Element.attr "aria-describedby" (Text.pack "start-note")] <> [Element.attr "disabled" Text.empty | not (pressable lobby)])
                [Element.string (if ready lobby then "Ready \x2713 \x00b7 waiting for every player" else if guestsReady lobby then "Start game" else "Ready")],
          Element.el "p" ["start-note", "muted"] [Element.attr "id" (Text.pack "start-note"), Element.attr "aria-live" (Text.pack "polite")] [Element.string (Maybe.fromMaybe ready' (waiting lobby))]
        ]
          <> [Element.el "ul" ["problems", "refusal"] [] [Element.el "li" [] [] [Element.text p] | p <- problems] | Left problems@(_ : _) <- [startable lobby]]
          <> [Element.el "p" ["problem", "deal-refusal"] [Element.attr "role" (Text.pack "alert")] [Element.text why] | why <- Maybe.maybeToList (refusal lobby)]

-- | The decks the seat being chosen for may play: the ways to add one, the
-- shelves, the filters, and the decks shown as tiles.
browserView :: Lobby -> Html.Html Event
browserView lobby =
  Element.keyed "section" ["browser"] [Element.attr "aria-label" (Text.pack "Decks")] $
    [ ( Text.pack "head",
        Element.el
          "div"
          ["browser-head"]
          []
          [ Element.el "h2" [] [] [Element.string ("Deck for " <> seatName (choosing lobby))],
            Element.button ["open-import"] (MkBrowse DeckBrowser.MkOpenImport) [Element.string "Import a deck"],
            Element.button ["open-builder"] MkOpenBuilder [Element.string "Build a deck"]
          ]
      )
    ]
      <> foldMap (pure . (,) (Text.pack "import") . fmap MkBrowse . DeckBrowser.importView) (DeckBrowser.importing browsing)
      <> [ (Text.pack "shelves", fmap MkBrowse (DeckBrowser.shelvesView browsing)),
           (Text.pack "filters", DeckBrowser.filtersView MkBrowse [Element.button ["random"] MkRandom [Element.string "Surprise me"]] browsing),
           (Text.pack "decks", DeckBrowser.resultsView MkPick ((== held) . Just) [Element.el "p" ["empty"] [] [Element.string "No saved decks yet: build or import one."]] (offered lobby) browsing)
         ]
  where
    browsing = browser lobby
    held = chosen lobby (choosing lobby)

-- | The deck of the seat being chosen for: its detail (DeckBrowser.detailBody),
-- and the ways to edit it, and a saved one's to copy it out and delete it.
detailView :: Lobby -> Html.Html Event
detailView lobby =
  Element.el "aside" ["detail"] [Element.attr "aria-label" (Text.pack "Deck chosen")] $
    Element.el "span" ["eyebrow"] [] [Element.string (if choosing lobby == MkMine then "Your deck" else seatName (choosing lobby) <> "'s deck")]
      : case chosen lobby (choosing lobby) of
        Nothing ->
          [ Element.el "h2" [] [] [Element.string (if choosing lobby == MkMine then "No deck yet" else "Random deck")],
            Element.el "p" ["muted"] [] [Element.string (if choosing lobby == MkMine then "Pick one of the decks to see it here." else "A deck sharing your deck's format, drawn when the game starts. Pick one to choose it.")]
          ]
        Just name ->
          DeckBrowser.detailBody (browser lobby) name
            <> [ Element.el "div" ["saved-tools"] [] $
                   if DeckBrowser.isSaved name
                     then [Element.button ["edit"] (MkEdit name) [Element.string "Edit"], Element.button ["export-deck"] (MkBrowse (DeckBrowser.MkExport name)) [Element.string "Export"], Element.button ["delete"] (MkBrowse (DeckBrowser.MkDelete name)) [Element.string "Delete"]]
                     else [Element.button ["edit"] (MkEdit name) [Element.string "Edit a copy"]]
               ]
            <> fmap (fmap MkBrowse) (DeckBrowser.exportedView (browser lobby) name)

-- | A slot a player from another browser takes: the link to send them, the
-- box their reply code goes in, and once connected their deck and whether
-- they are ready; the warning that the host sees every card; and `extra`,
-- the way to unseat it where there is one.
inviteView :: Int -> Invite -> [Html.Html Event] -> Html.Html Event
inviteView i invite extra =
  Element.el
    "div"
    ["lobby-seat", "invite"]
    [Element.attr "aria-label" (Text.pack ("Invited player, seat " <> show (i + 2)))]
    $ [ Element.el
          "div"
          ["seat-pick", "invited"]
          []
          [ Element.el
              "span"
              ["seat-head"]
              []
              ( [Element.el "span" ["avatar"] [] [Element.string "P"], Element.el "strong" ["seat-name"] [] [Element.string (seatName (MkTheirs i))]]
                  <> [Element.el "span" ["chip"] [] [Element.string (if guestReady invite then "Ready \x2713" else "Not ready")] | connected invite]
              ),
            Element.el "span" ["seat-deck"] [] [Element.el "span" ["seat-deck-name"] [] [Element.text (maybe (Text.pack (if connected invite then "Choosing a deck\x2026" else "Not connected")) (\deck -> snd (DeckBrowser.describe (DeckBrowser.titleIn deck) (fst deck))) (guestDeck invite))]],
            Element.el "span" ["seat-note", "muted"] [] [Element.string (if connected invite then "Chooses their own deck" else "Invited from another browser")]
          ]
      ]
      <> inviteSteps i invite
      <> [Element.el "div" ["seat-actions"] [] extra | not (null extra)]

-- | How invite `i` connects, until it does: the link to send, the box the
-- reply code goes in, and Connect; why the connection failed or ended; and
-- the warning that the host sees every card.
inviteSteps :: Int -> Invite -> [Html.Html Event]
inviteSteps i invite =
  ( if connected invite
      then []
      else case link invite of
        Nothing -> [Element.el "p" ["invite-step"] [] [Element.string "Making the link\x2026"]]
        Just url ->
          [ Element.el "label" [] [Element.attr "for" (field "invite-link")] [Element.string "1. Send them this link"],
            Element.copyable ("invite-link-" <> show i) url,
            Element.el "label" [] [Element.attr "for" (field "invite-reply")] [Element.string "2. Paste the code they send back"],
            Element.el "input" ["invite-reply"] [Element.attr "id" (field "invite-reply"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "spellcheck" (Text.pack "false"), Element.on "input" (MkReplyCode i)] [],
            Element.el "button" ["connect"] ([Element.attr "type" (Text.pack "button"), Element.on "click" (MkConnect i)] <> [Element.attr "disabled" Text.empty | Text.null (Text.strip (replyCode invite))]) [Element.string "Connect"]
          ]
  )
    <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (inviteProblem invite)]
    <> [Element.el "p" ["invite-warning"] [] [Element.string "Your browser runs the game, so you could see their hidden cards: invite only friends."]]
  where
    field name = Text.pack (name <> "-" <> show i)
