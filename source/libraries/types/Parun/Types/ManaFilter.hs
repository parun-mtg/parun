module Parun.Types.ManaFilter where

import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ProductionTag as ProductionTag

-- | Which of the mana in a pool a card's text is about -- Upwelling's "unspent
-- mana" against Omnath, Locus of Mana's "unspent green mana". Also which
-- types CR 106.7's "could produce" admits (Parun.Types.CouldProduce).
--
-- A SEPARATE, tiny type rather than a use of Parun.Types.Filter. Every Filter atom
-- asks about an object (CR 109.1) or a player, and a Parun.Types.ManaUnit is
-- neither. This type exists to avoid widening Filter with
-- atoms meaningless for every other candidate it serves.
--
-- Arms rather than one Set of the six CR 106.1b types: Upwelling prints "unspent
-- mana" with no type named, and spelling that as an enumeration would put the
-- closed half's list of mana types into open-half card data.
--
-- Not a predicate over CR 106.6's spending restriction, which lands on
-- Parun.Types.ManaUnit and is a Parun.Types.Filter over the SPELL rather than
-- anything this type could ask about a unit.
data ManaFilter
  = -- | Every unit in the pool, whatever its type. Upwelling.
    Any
  | -- | CR 106.1a / 106.1b: the units of exactly this type. Omnath, Locus of
    -- Mana's green.
    OfType ManaType.ManaType
  | -- | The complement of OfType: every unit that is not of this type.
    -- Celestial Dawn's "other mana", which its own sentence defines by
    -- exclusion from the white its first clause spoke about; Edgewalker's
    -- "colored mana" is the complement of colorless.
    --
    -- A COMPLEMENT OF ONE TYPE and not a general `Not ManaFilter`. Every
    -- printed "other mana" is worded against a single named type, so recursion
    -- would buy nothing Parun.Engine.ManaFilter -- which interprets this whole
    -- type in one comparison -- could use, and CR 106.1b's closed list means a
    -- wider complement is always writable the long way.
    NotOfType ManaType.ManaType
  | -- | CR 106.3: the units whose production carried this tag -- Draugr
    -- Necromancer's "mana from snow sources". A bare type carries none, so a
    -- position asking about one (Parun.Types.CouldProduce, a reduction's
    -- confinement) refuses it at decode.
    Produced ProductionTag.ProductionTag
  deriving (Eq, Ord, Show)
