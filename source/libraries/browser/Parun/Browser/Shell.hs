-- | The web client as one loop (#9075): the page, the screen it shows, and
-- the data loaded so far, built chunk by chunk (Parun.Web.Loading) one
-- batch at a time while no tap waits.
module Parun.Browser.Shell where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Browser.Replay as Replay
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Loading as Loading
import qualified Parun.Web.Screen as Screen
import qualified System.Environment as Environment

data Shell = MkShell
  { page :: Browser.Page Screen.Event,
    loaded :: IORef.IORef Loaded
  }

-- | The data so far: which chunks arrived, and what has been built from
-- them, each absent until built.
data Loaded = MkLoaded
  { loading :: Loading.Loading,
    -- | Every card file, parsed only when looked up.
    cards :: Maybe [(FilePath, Either Text.Text Card.Card)],
    registry :: Maybe (Registry.Registry Identity.Identity)
  }

-- | What woke a screen's loop.
data Woke
  = MkTapped Screen.Event Text.Text
  | -- | The data changed; the screen redraws.
    MkChanged

-- | The page mounted, the home screen drawn at once, and the loop run; an
-- exception that escapes is shown on the page. A replay the page names
-- (parun_scenario) skips the home screen.
start :: IO ()
start = do
  named <- Environment.lookupEnv "parun_scenario"
  case named of
    Just name -> Replay.start mempty name
    Nothing -> do
      shell <- MkShell <$> Browser.mount <*> IORef.newIORef (MkLoaded Loading.new Nothing Nothing)
      Exception.catch (run shell) $ \problem ->
        Browser.display (page shell) $ message ["The page stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

run :: Shell -> IO ()
run shell = do
  outcome <- home shell False
  already <- Loading.arrived . loading <$> IORef.readIORef (loaded shell)
  case outcome of
    Home.MkGo Home.MkPlayBots -> Hotseat.start already
    Home.MkGo Home.MkReplays -> Replay.start already ""
    Home.MkBack -> run shell
    Home.MkStay _ -> run shell

-- | The home screen until it leads somewhere, a game paused or not.
home :: Shell -> Bool -> IO Home.Outcome
home shell paused = go Home.new {Home.paused = paused}
  where
    go shown = do
      now <- IORef.readIORef (loaded shell)
      let current = shown {Home.cards = fmap Natural.length (cards now), Home.problem = Loading.problem (loading now)}
      Browser.display (page shell) (fmap Screen.MkHome (Home.view current))
      woke <- await shell
      case woke of
        MkTapped (Screen.MkHome event) _ -> case Home.update event current of
          Home.MkStay next -> go next
          outcome -> pure outcome
        _ -> go current

-- | A tap waiting, else a chunk arrived, else one batch of loading work,
-- else whatever comes next: a waiting tap is never kept behind work.
await :: Shell -> IO Woke
await shell = do
  waiting <- Browser.pending (page shell)
  if waiting
    then uncurry MkTapped <$> Browser.next (page shell)
    else do
      chunks <- Browser.arrivals (page shell)
      if not (null chunks)
        then do
          IORef.modifyIORef' (loaded shell) $ \l -> l {loading = foldr (Loading.arrive . arrival) (loading l) chunks}
          pure MkChanged
        else do
          worked <- work shell
          if worked
            then Browser.pause >> pure MkChanged
            else do
              input <- Browser.nextInput (page shell)
              case input of
                Browser.MkTap event value -> pure (MkTapped event value)
                Browser.MkArrival a -> do
                  IORef.modifyIORef' (loaded shell) $ \l -> l {loading = Loading.arrive (arrival a) (loading l)}
                  pure MkChanged
  where
    arrival a = case a of
      Browser.MkArrived name -> Right name
      Browser.MkFailed name -> Left name

-- | Asks the worker for what a screen waits on, first.
need :: Shell -> Loading.Need -> IO ()
need shell wanted = do
  now <- IORef.readIORef (loaded shell)
  let (next, chunks) = Loading.demand wanted (loading now)
  IORef.writeIORef (loaded shell) now {loading = next}
  mapM_ Browser.want chunks

-- | One batch of loading work, if any is ready to do.
work :: Shell -> IO Bool
work shell = do
  now <- IORef.readIORef (loaded shell)
  let ready n = Loading.ready n (loading now)
  case () of
    _
      | ready Loading.MkCards,
        Maybe.isNothing (cards now) -> do
          root <- Registry.defaultRoot
          bundles <- Registry.bundlePaths >>= traverse ByteString.readFile
          let parsed = Registry.loadBundles root bundles
          _ <- Exception.evaluate (length parsed)
          IORef.writeIORef (loaded shell) now {cards = Just parsed}
          pure True
      | ready Loading.MkRules,
        Just parsed <- cards now,
        Maybe.isNothing (registry now) -> do
          built <- Registry.defaultRegistry parsed
          IORef.writeIORef (loaded shell) now {registry = Just built}
          pure True
      | otherwise -> pure False

message :: [String] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Element.string t] | t <- said]

-- | The card count, for a screen to show.
cardCount :: Loaded -> Maybe Natural
cardCount = fmap Natural.length . cards
