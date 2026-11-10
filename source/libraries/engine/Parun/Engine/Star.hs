-- CR 208.2's printed star: substituting the value a characteristic-defining
-- ability supplies into a box, and asking whether a box holds one. Structural
-- only -- nothing here reads a board, which Parun.Engine.Quantity's evaluation
-- does.
--
-- A module of its own for a MODULE CYCLE rather than for cohesion, as
-- Parun.Engine.QuantitySlot is. Both functions lived in Parun.Engine.Quantity
-- until CR 709.4c's split-card merge came to ask them: that merge is in
-- Parun.Engine.Card, which Parun.Engine.Game imports, and Parun.Engine.Quantity
-- reads a board and so sits above Game.
module Parun.Engine.Star where

import qualified Data.Functor.Identity as Identity
import qualified Parun.Engine.Children as Children
import Parun.Types.Quantity (Quantity)
import qualified Parun.Types.Quantity as Quantity

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through a calculation's operands so 1+* becomes
-- 1+<the count>. No card prints one under anything but a Plus -- Malignus' star
-- is the whole P/T box and its CDA carries the halving.
--
-- No descent into a count's per-member number or a payload aimed elsewhere:
-- each reads against ANOTHER object, where a star would be that object's box
-- and not this one's.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  _ -> Identity.runIdentity (Children.quantityChildren Children.leaves {Children.term = Identity.Identity . substituteStar star} quantity)

-- Does a printed box hold CR 208.2's star anywhere inside it? A calculation
-- descends for substituteStar's reason: *+1 is a star box, and the star is what
-- a characteristic-defining ability fills in later -- proved by the scenario
-- consuming-blob-ooze-token-keeps-its-star-plus-one-box.
--
-- Asked by Parun.Engine.Resolve.Common.bakeTokenCharacteristics, which must tell a star
-- (keep it -- CR 208.2's value arrives at layer 7a, so there is nothing to settle
-- at creation) from a computed box (settle it -- CR 111.3 defines the token's
-- values once, as the effect resolves), and by Parun.Engine.Card's CR 709.4c
-- merge, which asks it of each half's box to find the half whose
-- characteristic-defining ability defines that box.
containsStar :: Quantity -> Bool
containsStar quantity = case quantity of
  Quantity.Star -> True
  Quantity.Arithmetic arithmetic -> any containsStar arithmetic
  Quantity.AtMost atMost -> any containsStar atMost
  -- No descent into a Count, nor into AgainstSlot or AgainstExiledPile:
  -- CR 208.2a's star is a printed box's own symbol, and each of those three
  -- reads its payload against ANOTHER object, where a star would be that
  -- object's box and not this one's.
  _ -> False
