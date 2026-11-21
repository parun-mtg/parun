module Parun.Types.TransformTrigger where

import qualified Parun.Types.ActingPermanent as ActingPermanent

-- | CR 701.27f: which permanent turned over, and the face it turned into.
data TransformTrigger = MkTransformTrigger
  { -- | The bearer (Howlpack Piper) or a permanent the Filter admits (Cult of
    -- the Waxing Moon), the Filter read against the characteristics the event
    -- sampled (Parun.Types.Transformed).
    permanent :: ActingPermanent.ActingPermanent,
    -- | CR 701.27f / 201.5: "transforms into [its own name]", the name the
    -- bearer has now, so a copy under another name (Olag) reads its own.
    --
    -- Unprinted: beside a Matching subject, which compares the watcher's name
    -- with the transformer's; Parun.FilterPositionLintSpec's "CR 701.27f no
    -- card writes intoItself beside a Matching subject" keeps it out.
    intoItself :: Bool
  }
  deriving (Eq, Ord, Show)

-- | "Whenever this permanent transforms": the bearer, into any face. The
-- codec's defaults.
defaultValue :: TransformTrigger
defaultValue =
  MkTransformTrigger
    { permanent = ActingPermanent.Self,
      intoItself = False
    }
