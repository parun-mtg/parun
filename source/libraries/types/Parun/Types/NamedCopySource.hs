module Parun.Types.NamedCopySource where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.ObjectRef as ObjectRef

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
  | -- | CR 202.1 / 608.2h: for each object the ref names, as it is or last
    -- existed, distinct reference cards the filter admits with that object's
    -- mana cost, chosen at random; an object with no mana cost names none
    -- (Maelstrom Archangel Avatar's "a random card with the same mana cost as
    -- that creature").
    SameManaCostAs ObjectRef.ObjectRef FromReference.FromReference
  deriving (Eq, Ord, Show)
