module Parun.Types.SecretChoice where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

-- | The subtype a player secretly chose for an object: A Killer Among Us's
-- ruling (2024-02-02).
data SecretChoice = MkSecretChoice
  { player :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId,
    subtype :: Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
