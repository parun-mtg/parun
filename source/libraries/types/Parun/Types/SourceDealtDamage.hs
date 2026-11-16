module Parun.Types.SourceDealtDamage where

import qualified Data.Set as Set
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient

-- | CR 120.1: which damage Parun.Types.EventShape's SourceDealtDamage folds over.
data SourceDealtDamage keyword = MkSourceDealtDamage
  { -- | Nothing for any damage; Just Combat for CR 510.2's combat damage alone.
    kind :: Maybe DamageKind.DamageKind,
    -- | The recipients admitted, any one sufficing, as Parun.Types.DamageTrigger's
    -- are; empty for damage to anything.
    recipients :: Set.Set (DamageRecipient.DamageRecipient keyword)
  }
  deriving (Eq, Ord, Show)
