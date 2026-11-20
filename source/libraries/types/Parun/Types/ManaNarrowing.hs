module Parun.Types.ManaNarrowing where

import qualified Data.Set as Set
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ProductionTag as ProductionTag

-- | CR 601.2h / 602.2b: the mana a payment's own spend rule admits, resolved
-- from its Parun.Types.ManaSpecification -- the types (Nothing for any), and
-- the production tags every mana must carry (CR 106.3). Throne of Eldraine's
-- chosen colour narrows the first, Security Rhox's Treasures the second.
--
-- Asked of the mana before CR 106.6's restrictions on the mana itself
-- (Parun.Engine.Mana.subjectAllows).
data ManaNarrowing = MkManaNarrowing
  { types :: Maybe (Set.Set ManaType.ManaType),
    tags :: Set.Set ProductionTag.ProductionTag
  }
  deriving (Eq, Ord, Show)

-- | A narrowing admitting every mana.
none :: ManaNarrowing
none = MkManaNarrowing Nothing Set.empty
