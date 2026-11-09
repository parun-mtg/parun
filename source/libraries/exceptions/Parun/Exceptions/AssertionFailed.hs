module Parun.Exceptions.AssertionFailed where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified GHC.Stack as Stack

-- | A test's assertion did not hold: its message, and the call stack it was
-- made from, innermost first.
data AssertionFailed = MkAssertionFailed
  { message :: Text.Text,
    frames :: [(String, Stack.SrcLoc)]
  }
  deriving (Eq, Show)

instance Exception.Exception AssertionFailed where
  displayException e = case frames e of
    [] -> Text.unpack $ message e
    _ -> Text.unpack (message e) <> "\n" <> Stack.prettyCallStack (Stack.fromCallSiteList $ frames e)
