module Parun.Types.NamedCopySource where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.FromReference as FromReference

-- | Where the names of Parun.Types.OfferNamedCopy's copies come from (CR
-- 707.13).
data NamedCopySource
  = -- | One of these names not yet chosen for the source, its controller choosing
    -- (Garth One-Eye).
    Unchosen (NonEmpty.NonEmpty CardName.CardName)
  | -- | CR 108.1: distinct cards of the Oracle reference the filter admits, chosen
    -- at random (Jhoira of the Ghitu Avatar's "three instant cards chosen at
    -- random").
    AtRandom FromReference.FromReference
  deriving (Eq, Ord, Show)
