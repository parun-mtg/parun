-- | A page driven through the wasm backend's JavaScript FFI: Haskell shows
-- views and waits for the events their handlers take. The page's script
-- (@source/web/page.js@) applies the patches and reports events, through the
-- Web Worker the engine runs in (@source/web/engine.js@).
module Parun.Browser where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified GHC.Clock as Clock
import qualified Parun.Extra.Integer as Integer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Trustworthy.Wasm as Prim
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Route as Route
import qualified Parun.Web.Wire as Wire

-- | The view the page shows, the events its handlers took that no one
-- has waited for yet, each with the value of the element it came from, the
-- data chunks that arrived since last read, what the page's data
-- channels said since, what takes a tap while a channel is waited on
-- (meanwhile), whether what they say is held for a game set aside
-- (aside), and when, on the monotonic clock in seconds, waiting gives up
-- (within).
data Page msg = MkPage
  { shown :: IORef.IORef (Maybe (Html.Html msg)),
    taken :: IORef.IORef (Seq.Seq (msg, Text.Text)),
    arrived :: IORef.IORef (Seq.Seq Arrival),
    heard :: IORef.IORef (Seq.Seq Linked),
    tapped :: IORef.IORef (Maybe ((msg, Text.Text) -> IO ())),
    held :: IORef.IORef Bool,
    due :: IORef.IORef (Maybe Double)
  }

-- | What a data channel to another browser said (page.js), by the number
-- the engine gave it: "offer" with the link to send (linkHost), "reply"
-- with the code to send back (linkJoin), "open", "message" with a line, or
-- "closed".
data Linked = MkLinked
  { link :: Int,
    kind :: Text.Text,
    said :: Text.Text
  }
  deriving (Eq, Show)

-- | A chunk of the data directory (engine.js) merged into it, or failed:
-- "decks", "cards-0" to "cards-3", "rules" or "scenarios".
data Arrival
  = MkArrived Text.Text
  | MkFailed Text.Text
  deriving (Eq, Show)

-- | What woke the page's loop: a tap, a chunk, or a data channel.
data Input msg
  = MkTap msg Text.Text
  | MkArrival Arrival
  | MkLink Linked

-- | A page showing nothing yet, its events listened for.
mount :: IO (Page msg)
mount = do
  page <- MkPage <$> IORef.newIORef Nothing <*> IORef.newIORef Seq.empty <*> IORef.newIORef Seq.empty <*> IORef.newIORef Seq.empty <*> IORef.newIORef Nothing <*> IORef.newIORef False <*> IORef.newIORef Nothing
  listen =<< wrap (dispatch page)
  pure page

-- | Brings the page to the view: the patches from the last one, or the whole
-- view the first time.
display :: Page msg -> Html.Html msg -> IO ()
display page html = do
  before <- IORef.readIORef $ shown page
  IORef.writeIORef (shown page) $ Just html
  json <- Exception.evaluate . Wire.patches $ maybe (Seq.singleton $ Patch.MkReplace Seq.empty html) (`Diff.diff` html) before
  applyJson $ Prim.toJSString json

-- | The next event the shown view's handlers take, waiting for one.
next :: Page msg -> IO (msg, Text.Text)
next page = do
  queued <- IORef.atomicModifyIORef' (taken page) $ \events -> case Seq.viewl events of
    event Seq.:< rest -> (rest, Just event)
    Seq.EmptyL -> (events, Nothing)
  maybe (waitOn page >> next page) pure queued

-- | The next chunk arrived, else the next tap, else what a data channel
-- said, waiting for any.
nextInput :: Page msg -> IO (Input msg)
nextInput page = do
  collect page
  chunk <- IORef.atomicModifyIORef' (arrived page) $ \chunks -> case Seq.viewl chunks of
    c Seq.:< rest -> (rest, Just c)
    Seq.EmptyL -> (chunks, Nothing)
  case chunk of
    Just c -> pure (MkArrival c)
    Nothing -> do
      queued <- pending page
      if queued
        then fmap (uncurry MkTap) (next page)
        else takeLinked page >>= maybe (waitOn page >> nextInput page) (pure . MkLink)

-- | The chunks arrived since last asked, without waiting.
arrivals :: Page msg -> IO [Arrival]
arrivals page = do
  collect page
  fmap Foldable.toList . IORef.atomicModifyIORef' (arrived page) $ \cs -> (Seq.empty, cs)

-- | Asks the worker to fetch this chunk next (engine.js), the scenarios
-- only once asked for.
want :: Text.Text -> IO ()
want = wantChunk . Prim.toJSString . Text.unpack

-- | Saves a deck list under this slug: the worker writes it beneath the
-- deck root at once (engine.js), and the page keeps it for later visits.
saveDeck :: Text.Text -> Text.Text -> IO ()
saveDeck slug contents = saveJs (Prim.toJSString (Text.unpack slug)) (Prim.toJSString (Text.unpack contents))

-- | Writes a line to the browser's console.
logLine :: Text.Text -> IO ()
logLine = logJs . Prim.toJSString . Text.unpack

-- | Deletes the deck list saved under this slug, from the worker and the
-- page alike.
forgetDeck :: Text.Text -> IO ()
forgetDeck = forgetJs . Prim.toJSString . Text.unpack

-- | Keeps a game as Parun.Web.Link.saved writes it, in place of
-- any kept before: the page keeps it for a reload to pick up (engine.js).
keepGame :: Text.Text -> IO ()
keepGame = keepGameJs . Prim.toJSString . Text.unpack

-- | Adds a reply's line (Parun.Web.Link.savedReply) to the game kept.
keepReply :: Text.Text -> IO ()
keepReply = keepReplyJs . Prim.toJSString . Text.unpack

-- | Forgets the game kept, once it is over.
forgetGame :: IO ()
forgetGame = forgetGameJs

-- | How the address bar moves to a screen's query (Parun.Web.Route).
data Moving
  = -- | A new entry of the browser's history, which back leaves.
    MkPush
  | -- | In place of the entry shown.
    MkReplace
  deriving (Eq, Show)

-- | Puts this query in the address bar, unless it is there already.
locate :: Moving -> Text.Text -> IO ()
locate moving query = case moving of
  MkPush -> pushJs (Prim.toJSString (Text.unpack query))
  MkReplace -> replaceJs (Prim.toJSString (Text.unpack query))

-- | The game the page kept when it loaded, empty if none.
keptGame :: IO Text.Text
keptGame = fmap (Text.pack . Prim.fromJSString) keptGameJs

-- | The games this browser finished (Parun.Web.History), each its key and
-- its summary, as the worker read them when the page loaded and kept since.
playedGames :: IO [(Text.Text, Text.Text)]
playedGames = fmap (fmap (fmap (Text.drop 1) . Text.breakOn (Text.pack "\t")) . filter (not . Text.null) . Text.lines . Text.pack . Prim.fromJSString) playedJs

-- | The scenario file of the finished game of this key, empty if none.
playedScenario :: Text.Text -> IO Text.Text
playedScenario = fmap (Text.pack . Prim.fromJSString) . playedScenarioJs . Prim.toJSString . Text.unpack

-- | Keeps a finished game under its key: its summary line and its scenario
-- file, which the worker keeps for later visits (engine.js).
keepPlayed :: Text.Text -> Text.Text -> Text.Text -> IO ()
keepPlayed k summary scenario = keepPlayedJs (Prim.toJSString (Text.unpack k)) (Prim.toJSString (Text.unpack summary)) (Prim.toJSString (Text.unpack scenario))

-- | Saves this text to the player's device as a file of this name (page.js).
download :: Text.Text -> Text.Text -> IO ()
download name contents = downloadJs (Prim.toJSString (Text.unpack name)) (Prim.toJSString (Text.unpack contents))

-- | Forgets the finished game of this key.
forgetPlayed :: Text.Text -> IO ()
forgetPlayed = forgetPlayedJs . Prim.toJSString . Text.unpack

-- | Forgets every finished game, those another tab kept too.
clearPlayed :: IO ()
clearPlayed = clearPlayedJs

-- | The page's clock: milliseconds since the Unix epoch, and how many
-- minutes it runs ahead of UTC.
clock :: IO (Integer, Integer)
clock = (,) <$> fmap (round :: Double -> Integer) nowJs <*> fmap toInteger aheadJs

-- | Moves the chunks the worker named since last asked onto the page's own
-- queue.
collect :: Page msg -> IO ()
collect page = do
  names <- fmap (Text.lines . Text.pack . Prim.fromJSString) takeArrivals
  let arrival n = maybe (MkArrived n) MkFailed (Text.stripPrefix (Text.pack "!") n)
  Monad.unless (null names) . IORef.atomicModifyIORef' (arrived page) $ \cs -> (cs <> Seq.fromList (fmap arrival names), ())

-- | What a data channel said first since last asked, without waiting;
-- nothing while a game is set aside (aside).
takeLinked :: Page msg -> IO (Maybe Linked)
takeLinked page = do
  holding <- IORef.readIORef (held page)
  if holding then pure Nothing else takeLinkedWith page (const True)

-- | What this data channel said first since last asked, waiting for it;
-- what the others say stays for later, and each tap taken meanwhile goes to
-- the handler 'meanwhile' gives, if any.
nextLinked :: Page msg -> Int -> IO Linked
nextLinked page n = takeLinkedWith page ((== n) . link) >>= maybe waiting pure
  where
    waiting = do
      handler <- IORef.readIORef (tapped page)
      queued <- pending page
      case handler of
        Just handle | queued -> next page >>= handle
        _ -> waitOn page
      nextLinked page n

-- | Runs `action`, each tap taken while it waits on a data channel
-- (nextLinked) given to `handle`: the page stays live while another
-- browser decides.
meanwhile :: Page msg -> ((msg, Text.Text) -> IO ()) -> IO a -> IO a
meanwhile page handle action = do
  before <- IORef.atomicModifyIORef' (tapped page) $ \h -> (Just handle, h)
  action `Exception.finally` IORef.writeIORef (tapped page) before

-- | Runs `action` with a game set aside: what every data channel says is
-- held for the game (takeLinked), and no tap goes to its waiting
-- (meanwhile), so the home screen can run while another browser decides.
aside :: Page msg -> IO a -> IO a
aside page action = do
  before <- IORef.atomicModifyIORef' (held page) (\h -> (True, h))
  waiter <- IORef.atomicModifyIORef' (tapped page) (\h -> (Nothing, h))
  action `Exception.finally` (IORef.writeIORef (held page) before >> IORef.writeIORef (tapped page) waiter)

takeLinkedWith :: Page msg -> (Linked -> Bool) -> IO (Maybe Linked)
takeLinkedWith page wanted = do
  received <- fmap (Text.lines . Text.pack . Prim.fromJSString) takeLinks
  let parsed = foldMap (either (const []) pure . (Common.parse Monad.>=> linkedOf)) received
  Monad.unless (null parsed) . IORef.atomicModifyIORef' (heard page) $ \ls -> (ls <> Seq.fromList parsed, ())
  IORef.atomicModifyIORef' (heard page) $ \ls -> case Seq.findIndexL wanted ls of
    Just i -> (Seq.deleteAt i ls, Seq.lookup i ls)
    Nothing -> (ls, Nothing)
  where
    linkedOf value = do
      fields <- Common.asObject value
      MkLinked
        <$> (Common.field "seat" fields >>= Common.asInteger >>= maybe (Left (Text.pack "not a channel")) Right . Integer.toInt)
        <*> (Common.field "kind" fields >>= Common.asText)
        <*> (Common.field "text" fields >>= Common.asText)

-- | Data channel `n` as one end of a link (Parun.Web.Link), each message
-- stamped with this build and one from another build refused.
channel :: Page msg -> Text.Text -> Int -> Link.Channel IO
channel page build n = Link.MkChannel (linkSend n . Link.encode build) receive
  where
    receive = do
      heard' <- nextLinked page n
      case Text.unpack (kind heard') of
        "message" -> pure (either (const Nothing) Just (Link.decode build (said heard')))
        "closed" -> pure Nothing
        _ -> receive

-- | Opens data channel `n`, for a seat a guest plays in a game at this
-- stage: page.js says "offer" with the link to send them once it is made,
-- naming the join screen for that stage (Parun.Web.Route).
linkHost :: Int -> Route.Stage -> IO ()
linkHost n stage = hostJs n (Prim.toJSString (Text.unpack (Route.render (Route.MkJoin stage))))

-- | Connects data channel `n` with the code its guest sent back.
linkAccept :: Int -> Text.Text -> IO ()
linkAccept n code = acceptJs n (Prim.toJSString (Text.unpack code))

-- | Answers a host's code on data channel `n`: page.js says "reply" with
-- the code to send back once it is made.
linkJoin :: Int -> Text.Text -> IO ()
linkJoin n code = joinJs n (Prim.toJSString (Text.unpack code))

-- | Sends a line over data channel `n`.
linkSend :: Int -> Text.Text -> IO ()
linkSend n line = sendJs n (Prim.toJSString (Text.unpack line))

-- | Closes data channel `n`, if open.
linkClose :: Int -> IO ()
linkClose = closeJs

-- | Whether an event taken waits to be read.
pending :: Page msg -> IO Bool
pending page = not . Seq.null <$> IORef.readIORef (taken page)

-- | Gives the page a turn: its events reach the handlers before this returns.
pause :: IO ()
pause = pausePromise >>= \() -> pure ()

-- | The events taken that `wanted` takes, from the first on until one it
-- does not, without waiting: the page given a turn first, so the taps sent
-- while the engine worked have arrived.
takeQueued :: Page msg -> (msg -> Maybe a) -> IO [a]
takeQueued page wanted = do
  pause
  IORef.atomicModifyIORef' (taken page) $ \events ->
    let (front, rest) = Seq.spanl (Maybe.isJust . wanted . fst) events
     in (rest, Maybe.mapMaybe (wanted . fst) (Foldable.toList front))

-- | Each event taken that no one has waited for yet given to `handle`, in
-- the order taken, without waiting.
drain :: Page msg -> ((msg, Text.Text) -> IO ()) -> IO ()
drain page handle = do
  queued <- pending page
  Monad.when queued (next page >>= handle >> drain page handle)

-- | Drops the events taken that no one has waited for yet, giving them in
-- the order taken.
flush :: Page msg -> IO [msg]
flush page = IORef.atomicModifyIORef' (taken page) $ \events -> (Seq.empty, fmap fst (Foldable.toList events))

dispatch :: Page msg -> Prim.JSString -> Prim.JSString -> Prim.JSString -> IO ()
dispatch page event path value = do
  html <- IORef.readIORef $ shown page
  case (html, Wire.eventPath . Text.pack $ Prim.fromJSString path) of
    (Just view, Just at) | Just msg <- Html.handler (Text.pack $ Prim.fromJSString event) at view -> do
      IORef.atomicModifyIORef' (taken page) $ \events -> (events Seq.|> (msg, Text.pack $ Prim.fromJSString value), ())
      wake
    _ -> pure ()

-- | Waits for the next tap or chunk without taking it.
idle :: Page msg -> IO ()
idle = waitOn

-- | Waiting given up at the deadline 'within' set.
data Expired = MkExpired
  deriving (Show)

instance Exception.Exception Expired

-- | Runs `action`, Nothing once the monotonic clock reaches `deadline`, in
-- seconds, while it waits on the page (waitOn): a timer wakes the wait
-- then, which gives up.
within :: Page msg -> Double -> IO a -> IO (Maybe a)
within page deadline action = do
  now <- Clock.getMonotonicTime
  before <- IORef.atomicModifyIORef' (due page) (\d -> (Just deadline, d))
  -- A little after, so the wait it wakes finds the deadline passed.
  timer <- armJs (max 0 (deadline - now) * 1000 + 10)
  Exception.handle (\MkExpired -> pure Nothing) (fmap Just action) `Exception.finally` (disarmJs timer >> IORef.writeIORef (due page) before)

-- | Waits as 'wait' does, giving up (Expired) where the deadline 'within'
-- set has passed, before or after.
waitOn :: Page msg -> IO ()
waitOn page = expire >> wait >> expire
  where
    expire = do
      deadline <- IORef.readIORef (due page)
      now <- Clock.getMonotonicTime
      Monad.when (any (now >=) deadline) (Exception.throwIO MkExpired)

-- | Waits until 'wake' is called: the waiting thread is suspended, and the
-- page goes on. The import's result is a thunk, forced here.
wait :: IO ()
wait = waitPromise >>= \() -> pure ()

foreign import javascript unsafe "parunApply($1)"
  applyJson :: Prim.JSString -> IO ()

foreign import javascript unsafe "parunListen($1)"
  listen :: Prim.JSVal -> IO ()

foreign import javascript "wrapper"
  wrap :: (Prim.JSString -> Prim.JSString -> Prim.JSString -> IO ()) -> IO Prim.JSVal

foreign import javascript safe "await new Promise((resolve) => { globalThis.parunWake = resolve; })"
  waitPromise :: IO ()

foreign import javascript safe "await new Promise((resolve) => setTimeout(resolve, 0))"
  pausePromise :: IO ()

foreign import javascript unsafe "globalThis.parunWake?.()"
  wake :: IO ()

foreign import javascript unsafe "setTimeout(() => globalThis.parunWake?.(), $1)"
  armJs :: Double -> IO Int

foreign import javascript unsafe "clearTimeout($1)"
  disarmJs :: Int -> IO ()

foreign import javascript unsafe "globalThis.parunTakeArrivals()"
  takeArrivals :: IO Prim.JSString

foreign import javascript unsafe "globalThis.parunWant($1)"
  wantChunk :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunSave($1, $2)"
  saveJs :: Prim.JSString -> Prim.JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
  logJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunForget($1)"
  forgetJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunTakeLinks()"
  takeLinks :: IO Prim.JSString

foreign import javascript unsafe "globalThis.parunLink({ op: 'host', seat: $1, query: $2 })"
  hostJs :: Int -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunLink({ op: 'accept', seat: $1, code: $2 })"
  acceptJs :: Int -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunLink({ op: 'join', seat: $1, code: $2 })"
  joinJs :: Int -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunLink({ op: 'send', seat: $1, text: $2 })"
  sendJs :: Int -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunLink({ op: 'close', seat: $1 })"
  closeJs :: Int -> IO ()

foreign import javascript unsafe "globalThis.parunKeepGame({ start: $1 })"
  keepGameJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunKeepGame({ reply: $1 })"
  keepReplyJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunKeepGame({})"
  forgetGameJs :: IO ()

foreign import javascript unsafe "globalThis.parunLocate($1, true)"
  pushJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunLocate($1, false)"
  replaceJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunKeptGame"
  keptGameJs :: IO Prim.JSString

foreign import javascript unsafe "globalThis.parunPlayed()"
  playedJs :: IO Prim.JSString

foreign import javascript unsafe "globalThis.parunPlayedScenario($1)"
  playedScenarioJs :: Prim.JSString -> IO Prim.JSString

foreign import javascript unsafe "globalThis.parunKeepPlayed($1, $2, $3)"
  keepPlayedJs :: Prim.JSString -> Prim.JSString -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunDownload($1, $2)"
  downloadJs :: Prim.JSString -> Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunForgetPlayed($1)"
  forgetPlayedJs :: Prim.JSString -> IO ()

foreign import javascript unsafe "globalThis.parunClearPlayed()"
  clearPlayedJs :: IO ()

foreign import javascript unsafe "Date.now()"
  nowJs :: IO Double

foreign import javascript unsafe "-new Date().getTimezoneOffset()"
  aheadJs :: IO Int
