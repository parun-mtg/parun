{-# LANGUAGE Trustworthy #-}

-- | The JavaScript values Parun's browser glue passes over the wasm backend's
-- FFI, from @GHC.Wasm.Prim@, which its package does not mark Safe.
-- Trustworthy because each is an opaque handle or a pure conversion,
-- breaking neither type safety nor module abstraction. JSString's constructor
-- is exported because the FFI marshals a newtype only when it is in scope.
module Parun.Trustworthy.Wasm
  ( JSString (..),
    JSVal,
    fromJSString,
    toJSString,
  )
where

import GHC.Wasm.Prim
  ( JSString (JSString),
    JSVal,
    fromJSString,
    toJSString,
  )
