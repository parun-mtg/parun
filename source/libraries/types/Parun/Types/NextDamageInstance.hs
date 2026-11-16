module Parun.Types.NextDamageInstance where

import qualified Data.Sequence as Seq
import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.NextDamageOutcome as NextDamageOutcome
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 615.8's "the next time a specific source would deal damage": over whom,
-- from which source, for how long, what it does to that one INSTANCE of damage,
-- whatever its size, and CR 615.5's additional effect riding a prevention. The
-- shield is then used up.

-- Parametric in the effect for Parun.Types.PreventNextDamage's reason.
data NextDamageInstance effect = MkNextDamageInstance
  { duration :: Duration.Duration,
    -- | PRINTED, as Parun.Types.PreventNextDamage's @kind@ is: Forcefield says
    -- "would deal COMBAT damage". Nothing takes combat and noncombat alike.
    kind :: Maybe DamageKind.DamageKind,
    -- | The objects or players this RESOLUTION names, on the side @direction@
    -- says: the recipients (Deflecting Palm's "to you", Honorable Passage's "to
    -- any target"), one CR 615.8 shield each, or the SOURCE (Awe Strike's
    -- "target creature"), whose shield then covers every recipient. Nothing for
    -- a shield describing its recipients instead, in the two fields below, read
    -- as Parun.Types.PreventNextDamage reads its own.
    ref :: Maybe ObjectRef.ObjectRef,
    -- | CR 611.2c's LIVE description of the objects ONE shield covers --
    -- Shadowbane's "creatures you control" -- as
    -- Parun.Types.PreventNextDamage's field of the same name. Beside DealtBy it
    -- is the far END of the event instead, conjoined onto each source's row.
    whatRecipient :: Maybe (Filter.Filter Keyword.Keyword),
    -- | The player half of that description -- Shadowbane's "to you" --
    -- disjoined with the field above on the one row installed. Read beside
    -- DealtBy too: Mercenaries' "deal damage to you".
    whoRecipient :: Maybe PlayerRelation.PlayerRelation,
    -- | Which side of the damage event @ref@ names, as
    -- Parun.Types.PreventAllDamage's field of the same name. DealtTo for every
    -- shield but the by-direction ones, so the key is elided.
    direction :: DamageDirection.DamageDirection,
    -- | CR 609.7a's "a source of your choice", as the PROPERTIES the chosen
    -- source must have, as Parun.Types.PreventAllDamage's field of the same
    -- name. Deflecting Palm says only "a source", so its Filter is the trivial
    -- `And []` -- "any source of your choice" is still a choice. Nothing asks no
    -- one: a shield whose source is CR 601.2c's target (DealtBy) or CR 609.7b's
    -- description (@whatSource@). Not read beside DealtBy.
    --
    -- BOTH halves of CR 615.9, exactly as the sibling shields carry it: it
    -- narrows the candidates offered, and it is written into
    -- Parun.Types.DamagePattern.whatSource so CR 609.7b's recheck happens at the
    -- damage event rather than at the choice.
    chosenSource :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 609.7b's property-named source, which nobody chooses -- Circle of
    -- Solace's "a creature of the chosen type" -- as
    -- Parun.Types.PreventAllDamage's field of the same name, read on both
    -- sides of @direction@. `And []`, the elided default, says nothing.
    whatSource :: Filter.Filter Keyword.Keyword,
    -- | CR 615.5's additional effect -- Reverse Damage's "you gain life equal to
    -- the damage prevented this way". Empty for a shield with no such clause,
    -- so the key is elided rather than written as an empty array.
    riders :: Seq.Seq effect,
    -- | What happens to the instance. Elided when it is prevented.
    outcome :: NextDamageOutcome.NextDamageOutcome
  }
  deriving (Eq, Ord, Show)
