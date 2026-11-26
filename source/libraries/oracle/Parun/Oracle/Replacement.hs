-- Replacement effects (CR 614) of static abilities as Oracle text words them.
module Parun.Oracle.Replacement where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Oracle.Target as Target
import qualified Parun.Oracle.Trigger as Trigger
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.TapState as TapState

-- | A static ability's replacement effect (CR 614.1): "This land enters
-- tapped." (CR 614.1d), or "... unless [condition]." when it applies only
-- while a condition fails ('Trigger.unless'); "This creature enters
-- prepared." (CR 723.3a). Any other replacement effect, or one with a zone
-- it functions from or a name, is not spelled out here.
replacement :: Target.Context -> PrintedReplacement.PrintedReplacement card ability effect -> [Text.Text]
replacement context r = case r of
  PrintedReplacement.MkPrintedReplacement condition (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.EntersTapState TapState.Tapped))) zones Nothing
    | Set.null zones -> case condition of
        Nothing -> [Target.self context <> Text.pack " enters tapped."]
        Just c -> do
          holding <- foldMap pure (Trigger.unless c)
          words' <- Trigger.condition context holding
          [Target.self context <> Text.pack " enters tapped unless " <> words' <> Text.pack "."]
  PrintedReplacement.MkPrintedReplacement Nothing (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource EntryRewrite.Prepared)) zones Nothing
    | Set.null zones -> [Target.self context <> Text.pack " enters prepared."]
  _ -> []
