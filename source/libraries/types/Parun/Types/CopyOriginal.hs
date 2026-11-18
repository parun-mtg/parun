module Parun.Types.CopyOriginal where

import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.ObjectRef as ObjectRef

-- | What Parun.Types.BecomeCopy's subject becomes a copy of (CR 707.1).
--
-- Parametric in @card@ for Parun.Types.Create's reason: Defined's card is card
-- data nested inside card data.
data CopyOriginal card
  = -- | An object in the game (Unstable Shapeshifter's "that creature").
    OfObject ObjectRef.ObjectRef
  | -- | CR 108.1 / 707.2: the card the Oracle reference gives this name
    -- (Transcantation's "a copy of Lightning Bolt").
    Named CardName.CardName
  | -- | A card the effect itself defines, whose characteristics are the copiable
    -- values (Bombardment's "red sorceries named Missile").
    Defined card
  deriving (Eq, Ord, Show)
