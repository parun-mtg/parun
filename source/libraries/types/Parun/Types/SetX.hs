module Parun.Types.SetX where

import qualified Parun.Types.NewX as NewX
import qualified Parun.Types.ObjectRef as ObjectRef

-- | The payload of Parun.Types.Effect's SetX arm: CR 107.3a's announced X of
-- the spells and abilities the reference names becomes the new value.
data SetX = MkSetX
  { value :: NewX.NewX,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
