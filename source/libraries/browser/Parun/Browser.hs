-- | Runs an app on the page through the wasm backend's JavaScript FFI. The
-- page's script (@source/web/parun.js@) applies the patches and reports events.
module Parun.Browser where

import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Trustworthy.Wasm as Prim
import qualified Parun.Web.App as App
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Wire as Wire

-- | Shows the app's first view, then hands the page the function its events
-- go to, which keeps the running app between them.
run :: App.App model msg -> model -> IO ()
run app m = do
  let running = App.start app m
  apply . Seq.singleton . Patch.MkReplace Seq.empty $ App.shown running
  ref <- IORef.newIORef running
  listen =<< wrap (dispatch app ref)

dispatch :: App.App model msg -> IORef.IORef (App.Running model msg) -> Prim.JSString -> Prim.JSString -> IO ()
dispatch app ref event path = case Wire.eventPath . Text.pack $ Prim.fromJSString path of
  Nothing -> pure ()
  Just at -> do
    running <- IORef.readIORef ref
    case App.dispatch app (Text.pack $ Prim.fromJSString event) at running of
      Nothing -> pure ()
      Just (next, patches) -> do
        IORef.writeIORef ref next
        apply patches

apply :: Seq.Seq (Patch.Patch msg) -> IO ()
apply = applyJson . Prim.toJSString . Wire.patches

foreign import javascript unsafe "parunApply($1)"
  applyJson :: Prim.JSString -> IO ()

foreign import javascript unsafe "parunListen($1)"
  listen :: Prim.JSVal -> IO ()

foreign import javascript "wrapper"
  wrap :: (Prim.JSString -> Prim.JSString -> IO ()) -> IO Prim.JSVal
