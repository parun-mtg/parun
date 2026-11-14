module Parun.Types.NextDamageInstance where

import qualified Data.Sequence as Seq
import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.NextDamageOutcome as NextDamageOutcome
import qualified Parun.Types.ObjectRef as ObjectRef

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
    -- "target creature"), whose shield then covers every recipient.
    --
    -- Required, where the two shields beside this one make it optional: every
    -- card in data/cards/ of this shape names one end outright. A card
    -- describing its recipients by characteristic instead would want
    -- Parun.Types.PreventNextDamage's @whatRecipient@ and @whoRecipient@ pair
    -- here too.
    ref :: ObjectRef.ObjectRef,
    -- | Which side of the damage event @ref@ names, as
    -- Parun.Types.PreventAllDamage's field of the same name. DealtTo for every
    -- shield but Awe Strike's, so the key is elided.
    direction :: DamageDirection.DamageDirection,
    -- | CR 609.7a's "a source of your choice", as the PROPERTIES the chosen
    -- source must have. Required on the DealtTo side: CR 615.8's shield is about
    -- "the next time a SPECIFIC source would deal damage", so there is no shape
    -- of this rule that watches every source. Deflecting Palm says only "a
    -- source", so its Filter is the trivial `And []` -- "any source of your
    -- choice" is still a choice. Not read beside DealtBy, whose source is CR
    -- 601.2c's target (Parun.Types.DamageDirection).
    --
    -- BOTH halves of CR 615.9, exactly as the sibling shields carry it: it
    -- narrows the candidates offered, and it is written into
    -- Parun.Types.DamagePattern.whatSource so CR 609.7b's recheck happens at the
    -- damage event rather than at the choice.
    chosenSource :: Filter.Filter Keyword.Keyword,
    -- | CR 615.5's additional effect -- Reverse Damage's "you gain life equal to
    -- the damage prevented this way". Empty for a shield with no such clause,
    -- so the key is elided rather than written as an empty array.
    riders :: Seq.Seq effect,
    -- | What happens to the instance. Elided when it is prevented.
    outcome :: NextDamageOutcome.NextDamageOutcome
  }
  deriving (Eq, Ord, Show)
