-- | A page driven through the wasm backend's JavaScript FFI: Haskell shows
-- views and waits for the events their handlers take. The page's script
-- (@source/web/page.js@) applies the patches and reports events, through the
-- Web Worker the engine runs in (@source/web/engine.js@).
module Parun.Browser where

import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Trustworthy.Wasm as Prim
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Wire as Wire

-- | The view the page shows, and the events its handlers took that no one
-- has waited for yet, each with the value of the element it came from.
data Page msg = MkPage
  { shown :: IORef.IORef (Maybe (Html.Html msg)),
    taken :: IORef.IORef (Seq.Seq (msg, Text.Text))
  }

-- | A page showing nothing yet, its events listened for.
mount :: IO (Page msg)
mount = do
  page <- MkPage <$> IORef.newIORef Nothing <*> IORef.newIORef Seq.empty
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
