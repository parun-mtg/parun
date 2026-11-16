module Parun.Types.SourceDealtDamage where

import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.RecipientKind as RecipientKind

-- | CR 120.1: which damage Parun.Types.EventShape's SourceDealtDamage folds over.
data SourceDealtDamage = MkSourceDealtDamage
  { -- | Nothing for any damage; Just Combat for CR 510.2's combat damage alone.
    kind :: Maybe DamageKind.DamageKind,
    recipient :: RecipientKind.RecipientKind
  }
  deriving (Eq, Ord, Show)
