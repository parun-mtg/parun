module Parun.Types.KeywordExpansion where

import Data.Set (Set)
import Numeric.Natural (Natural)
import Parun.Types.ActivatedAbility (ActivatedAbility)
import qualified Parun.Types.AttachRestriction as AttachRestriction
import Parun.Types.Card (Card)
import qualified Parun.Types.CardType as CardType
import Parun.Types.CastingPermission (CastingPermission)
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.GrantedAbility as GrantedAbility
import Parun.Types.ReplacementEffect (ReplacementEffect)
import Parun.Types.TriggeredAbility (TriggeredAbility)

-- | CR 702: everything rule 702 mints for one keyword, one field per kind of
-- thing minted. Parun.Engine.Keyword.expansionOf is the one exhaustive answer,
-- and the rosters there read one field each.
--
-- The fields are LAZY, so a reader of one field never builds the others: an
-- arm is a record update of 'mempty', and a field it does not name stays a
-- thunk.
data KeywordExpansion = MkKeywordExpansion
  { -- | CR 603: the triggered abilities the keyword, held that many times,
    -- states (CR 113.2c: one per instance unless its rule says otherwise).
    triggered :: Natural -> [TriggeredAbility Card (GrantedAbility.GrantedAbility Card)],
    -- | CR 602.1 / 113.6b: the activated abilities it states from a hand.
    fromHand :: [ActivatedAbility Card (GrantedAbility.GrantedAbility Card)],
    -- | CR 602.1 / 113.6b: the activated abilities it states from a graveyard.
    fromGraveyard :: [ActivatedAbility Card (GrantedAbility.GrantedAbility Card)],
    -- | CR 602.1: the activated abilities it states, held that many times, on
    -- the battlefield.
    onBattlefield :: Natural -> [ActivatedAbility Card (GrantedAbility.GrantedAbility Card)],
    -- | CR 601.3: the casting permissions it grants the face of those card
    -- types.
    permissions :: Set CardType.CardType -> [CastingPermission],
    -- | CR 614: the replacement effects it states, held that many times.
    replacements :: Natural -> [ReplacementEffect Card (GrantedAbility.GrantedAbility Card) (Effect.Effect Card (GrantedAbility.GrantedAbility Card))],
    -- | CR 508.1c / 509.1b: the combat restrictions it states.
    combatRestrictions :: [CombatRestriction.CombatRestriction],
    -- | CR 701.3a / 101.2: the prohibitions on what may become attached to it.
    attachRestrictions :: [AttachRestriction.AttachRestriction]
  }

-- | Field by field, so a new field cannot be added without -Werror naming
-- this site.
instance Semigroup KeywordExpansion where
  left <> right =
    MkKeywordExpansion
      { triggered = triggered left <> triggered right,
        fromHand = fromHand left <> fromHand right,
        fromGraveyard = fromGraveyard left <> fromGraveyard right,
        onBattlefield = onBattlefield left <> onBattlefield right,
        permissions = permissions left <> permissions right,
        replacements = replacements left <> replacements right,
        combatRestrictions = combatRestrictions left <> combatRestrictions right,
        attachRestrictions = attachRestrictions left <> attachRestrictions right
      }

-- | A keyword that mints nothing, the answer for every field an arm leaves
-- unnamed.
instance Monoid KeywordExpansion where
  mempty =
    MkKeywordExpansion
      { triggered = mempty,
        fromHand = [],
        fromGraveyard = [],
        onBattlefield = mempty,
        permissions = mempty,
        replacements = mempty,
        combatRestrictions = [],
        attachRestrictions = []
      }
