-- | A page driven through the wasm backend's JavaScript FFI: Haskell shows
-- views and waits for the events their handlers take. The page's script
-- (@source/web/page.js@) applies the patches and reports events, through the
-- Web Worker the engine runs in (@source/web/engine.js@).
module Parun.Browser where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Trustworthy.Wasm as Prim
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Wire as Wire

-- | The view the page shows, the events its handlers took that no one
-- has waited for yet, each with the value of the element it came from, and
-- the data chunks that arrived since last read.
data Page msg = MkPage
  { shown :: IORef.IORef (Maybe (Html.Html msg)),
    taken :: IORef.IORef (Seq.Seq (msg, Text.Text)),
    arrived :: IORef.IORef (Seq.Seq Arrival)
  }

-- | A chunk of the data directory (engine.js) merged into it, or failed:
-- "decks", "cards-0" to "cards-3", "rules" or "scenarios".
data Arrival
  = MkArrived Text.Text
  | MkFailed Text.Text
  deriving (Eq, Show)

-- | What woke the page's loop: a tap, or a chunk.
data Input msg
  = MkTap msg Text.Text
  | MkArrival Arrival

-- | A page showing nothing yet, its events listened for.
mount :: IO (Page msg)
mount = do
  page <- MkPage <$> IORef.newIORef Nothing <*> IORef.newIORef Seq.empty <*> IORef.newIORef Seq.empty
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
  maybe (wait >> next page) pure queued

-- | The next chunk arrived, else the next tap, waiting for either.
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
      if queued then fmap (uncurry MkTap) (next page) else wait >> nextInput page

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

-- | Moves the chunks the worker named since last asked onto the page's own
-- queue.
collect :: Page msg -> IO ()
collect page = do
  names <- fmap (Text.lines . Text.pack . Prim.fromJSString) takeArrivals
  let arrival n = maybe (MkArrived n) MkFailed (Text.stripPrefix (Text.pack "!") n)
  Monad.unless (null names) . IORef.atomicModifyIORef' (arrived page) $ \cs -> (cs <> Seq.fromList (fmap arrival names), ())

-- | Whether an event taken waits to be read.
pending :: Page msg -> IO Bool
pending page = not . Seq.null <$> IORef.readIORef (taken page)

-- | Gives the page a turn: its events reach the handlers before this returns.
pause :: IO ()
pause = pausePromise >>= \() -> pure ()

-- | Drops the events taken that no one has waited for yet.
flush :: Page msg -> IO ()
flush page = IORef.atomicWriteIORef (taken page) Seq.empty

dispatch :: Page msg -> Prim.JSString -> Prim.JSString -> Prim.JSString -> IO ()
dispatch page event path value = do
  html <- IORef.readIORef $ shown page
  case (html, Wire.eventPath . Text.pack $ Prim.fromJSString path) of
    (Just view, Just at) | Just msg <- Html.handler (Text.pack $ Prim.fromJSString event) at view -> do
      IORef.atomicModifyIORef' (taken page) $ \events -> (events Seq.|> (msg, Text.pack $ Prim.fromJSString value), ())
      wake
    _ -> pure ()

-- | Waits for the next tap or chunk without taking it.
idle :: IO ()
idle = wait

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
