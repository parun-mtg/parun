-- CR 612.1 text-changing effects: rewriting every card-text position an
-- effect can name, one traversal per type. Pure over the text; the projection
-- that decides whether a rewrite applies is Parun.Engine.Projection. Split out
-- of it for size.
module Parun.Engine.Projection.Rewrite where

import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Children as Children
import qualified Parun.Engine.Filter as Filter
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Engine.TextWord as TextWord.Engine
import qualified Parun.Types.AbilityActivatedTrigger as AbilityActivatedTrigger
import qualified Parun.Types.AbilityAddsMana as AbilityAddsMana
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationProhibition as ActivationProhibition
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AffectedUnless as AffectedUnless
import qualified Parun.Types.AloneUnless as AloneUnless
import qualified Parun.Types.AlternativeCost as AlternativeCost
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.AmassKind as AmassKind
import qualified Parun.Types.Arithmetic as Arithmetic
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachR as AttachR
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttachRewrite as AttachRewrite
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.AttachmentTrigger as AttachmentTrigger
import qualified Parun.Types.AttackCompany as AttackCompany
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackLimitUnless as AttackLimitUnless
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.AttackTrigger as AttackTrigger
import qualified Parun.Types.AttackerTarget as AttackerTarget
import qualified Parun.Types.AttackerTrigger as AttackerTrigger
import qualified Parun.Types.BecomesTapState as BecomesTapState
import qualified Parun.Types.BecomesTargetedTrigger as BecomesTargetedTrigger
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.BlockCount as BlockCount
import qualified Parun.Types.BlockPermission as BlockPermission
import qualified Parun.Types.BlockRequirement as BlockRequirement
import qualified Parun.Types.BlockTrigger as BlockTrigger
import qualified Parun.Types.CantAttackPlayer as CantAttackPlayer
import qualified Parun.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Parun.Types.CantBlockCreatures as CantBlockCreatures
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.CausedBy as CausedBy
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.ChosenCardInHand as ChosenCardInHand
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Condition as Condition.Type
import qualified Parun.Types.CopyClause as CopyClause
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CostAddition as CostAddition
import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostReduction as CostReduction
import qualified Parun.Types.CostScale as CostScale
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.CounterHolder as CounterHolder
import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.CounterProhibition as CounterProhibition
import qualified Parun.Types.CounterR as CounterR
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.CounterTrigger as CounterTrigger
import qualified Parun.Types.CrewRestriction as CrewRestriction
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.DamageR as DamageR
import qualified Parun.Types.DamageRewrite as DamageRewrite
import qualified Parun.Types.DamageTrigger as DamageTrigger
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DeclarationTrigger as DeclarationTrigger
import qualified Parun.Types.DestructionR as DestructionR
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.DiscardSubject as DiscardSubject
import qualified Parun.Types.DiscardTrigger as DiscardTrigger
import qualified Parun.Types.DrawR as DrawR
import qualified Parun.Types.DrawRewrite as DrawRewrite
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.EachCardInExiledPile as EachCardInExiledPile
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.EachCardInHand as EachCardInHand
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntersTrigger as EntersTrigger
import qualified Parun.Types.EntersWith as EntersWith
import qualified Parun.Types.EntriesTrigger as EntriesTrigger
import qualified Parun.Types.EntryFlip as EntryFlip
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRestriction as EntryRestriction
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.ExileFromGraveyard as ExileFromGraveyard
import qualified Parun.Types.ExploitTrigger as ExploitTrigger
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.FreeCastFromHand as FreeCastFromHand
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Parun.Types.FullText as FullText
import qualified Parun.Types.GameLossR as GameLossR
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HexproofExemption as HexproofExemption
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.KeywordRiderGrant as KeywordRiderGrant
import qualified Parun.Types.LeavesTrigger as LeavesTrigger
import qualified Parun.Types.LeavesZoneTrigger as LeavesZoneTrigger
import qualified Parun.Types.LeavingPermanent as LeavingPermanent
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LimitUnless as LimitUnless
import qualified Parun.Types.LoyaltyGrant as LoyaltyGrant
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaRider as ManaRider
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Modal as Modal.Type
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PerCreature as PerCreature
import qualified Parun.Types.PerTarget as PerTarget
import qualified Parun.Types.PerformerR as PerformerR
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Parun.Types.PermanentIsDealtDamage as PermanentIsDealtDamage
import qualified Parun.Types.PermanentsDealCombatDamageToPlayer as PermanentsDealCombatDamageToPlayer
import qualified Parun.Types.PhaseR as PhaseR
import qualified Parun.Types.PhaseRewrite as PhaseRewrite
import qualified Parun.Types.PlacesSticker as PlacesSticker
import qualified Parun.Types.PlaneswalkR as PlaneswalkR
import qualified Parun.Types.PlaneswalkRewrite as PlaneswalkRewrite
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.PlaysLand as PlaysLand
import qualified Parun.Types.PlotFromZone as PlotFromZone
import qualified Parun.Types.Plus as Plus
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.PutIntoZoneTrigger as PutIntoZoneTrigger
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.RandomCardInGraveyard as RandomCardInGraveyard
import qualified Parun.Types.RemovalCount as RemovalCount
import Parun.Types.ReplacementEffect (ReplacementEffect)
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.RevealedThisWay as RevealedThisWay
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.SacrificeToEnter as SacrificeToEnter
import qualified Parun.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SpellCast as SpellCast
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.Subtype as Subtype.Type
import qualified Parun.Types.TapForMana as TapForMana
import qualified Parun.Types.TappedForManaTrigger as TappedForManaTrigger
import qualified Parun.Types.TappedPermanent as TappedPermanent
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TargetedSubject as TargetedSubject
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.TokenPattern as TokenPattern
import qualified Parun.Types.TokenR as TokenR
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.TransformTrigger as TransformTrigger
import qualified Parun.Types.TriggerCondition as TriggerCondition
import Parun.Types.TriggeredAbility (TriggeredAbility)
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnUpR as TurnUpR
import qualified Parun.Types.TurnUpRewrite as TurnUpRewrite
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.WordFamily as WordFamily
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR

-- CR 613.1f's grant carries a whole quoted ability, and a card's abilities are
-- written against a whole Card (CR 707.8a).
type Modification = Modification.Modification (GrantedAbility.GrantedAbility Card.Type.Card)

-- Apply text-changes to a modification's subtype and colour words (CR
-- 612.1/612.2).
--
-- CR 612.2 gates each arm: the arm's family is fixed by its constructor, and the
-- PAIR's family is read off the word being replaced, which is why no family tag
-- rides on the stored ChangeWord. Exhaustive rather than a catch-all, which
-- is what had let GainKeyword go unrewritten while carrying a land-type word.
-- Descends into the quoted ability a CR 613.1f grant carries, through
-- rewriteGrantedAbility below -- which is mutually recursive with this, since
-- that ability's own clauses hold effects that can grant again.
rewriteModification :: [(TextWord.TextWord, TextWord.TextWord)] -> Modification -> Modification
rewriteModification pairs m =
  let apply1 acc (from, to) =
        let -- `inFamily f` is CR 612.2's gate.
            swap inFamily s = case (from, to) of
              (TextWord.SubtypeWord f, TextWord.SubtypeWord t) | s == f && inFamily f -> t
              _ -> s
            colors = Set.map (TextWord.Engine.colorAfter [(from, to)])
         in case acc of
              Modification.LandSubtype mode s -> Modification.LandSubtype mode (swap Subtype.isLandType s)
              -- CR 612.2's other named example: a Turn to Frog's Frog on the stack.
              Modification.CreatureSubtype mode s -> Modification.CreatureSubtype mode (swap Subtype.isCreatureType s)
              -- Holds no word to swap: it names CR 205.3m's list, not a member of it.
              Modification.AddEveryCreatureSubtype -> acc
              -- Names a card type's whole subtype set (CR 205.1a), not a member
              -- of it, so no word to swap for the same reason.
              Modification.LoseEverySubtype _ -> acc
              -- CR 612.1 through its Filter, printed on the granter (CR 612.3).
              Modification.SetCreatureSubtypesOfLastCardExiledWith f -> Modification.SetCreatureSubtypesOfLastCardExiledWith (Filter.rewrite [(from, to)] f)
              -- Deliberately unrewritten. CR 612.2 changes only a word used in the
              -- correct way, and this arm carries no family to check the word against;
              -- the two family-tagged arms above are where a land-type or creature-type
              -- word rides, and Parun.EffectLintSpec keeps those words out of this one.
              Modification.AddSubtype _ -> acc
              -- The removal's word, unrewritten for the same reason.
              Modification.RemoveSubtype _ -> acc
              -- CR 702.14a: "[type]walk" holds a land-type word, so a hacked Lord of
              -- Atlantis grants swampwalk. The GRANTER's text is what this reads, which
              -- is CR 612.3. Filter.rewriteKeyword since the word is inside a Filter; no
              -- family gate is restated there -- the word's use is its family.
              Modification.GainKeyword k -> Modification.GainKeyword (Filter.rewriteKeyword [(from, to)] k)
              -- Carries no word: the keyword is rule 702's and the cost is the
              -- RECEIVER's mana cost, so neither half is text a pair could name --
              -- ChosenSubtype's answer for the same reason.
              Modification.GainKeywordAtManaCost _ -> acc
              -- CR 612.1 through the granted enchant's own Filter, which is text
              -- printed on the GRANTER (CR 612.3) exactly as the keyword above is.
              -- rewriteTargetSlot is the same descent a mode's target slots take.
              Modification.GainEnchant slot -> Modification.GainEnchant (rewriteTargetSlot [(from, to)] slot)
              Modification.LoseEnchant slot -> Modification.LoseEnchant (rewriteTargetSlot [(from, to)] slot)
              -- CR 612.1 through a flash permission's condition, printed on the
              -- granter (CR 612.3); every other arm carries no word.
              Modification.GainCastingPermission permission -> Modification.GainCastingPermission (rewriteCastingPermission [(from, to)] permission)
              -- CR 612.1 through the names' own Filter, printed on the granter (CR
              -- 612.3) as the enchant's is.
              Modification.AddNamesMatching f -> Modification.AddNamesMatching (Filter.rewrite [(from, to)] f)
              -- A name is no subtype word (CR 612.2).
              Modification.SetName _ -> acc
              Modification.InsertNameWords _ -> acc
              -- CR 612.1 over the whole quoted ability: the words are printed on the
              -- GRANTER, so a text change affecting it rewrites them before the grant.
              Modification.GainAbility a -> Modification.GainAbility (rewriteGrantedAbility [(from, to)] a)
              -- The same descent into CR 612.6's extra text, printed on the granter;
              -- a PlayerRef names no subtype word.
              Modification.HasFullText ft -> Modification.HasFullText ft {FullText.alsoHas = fmap (rewriteGrantedAbility [(from, to)]) (FullText.alsoHas ft)}
              -- Carries no word: rule 702.165a's grant names the SOURCE, and the
              -- abilities it will hand over are that object's copiable values as the
              -- ability was put on the stack (CR 702.165d, Parun.Engine.Resolve.Effect's
              -- expandGrant).
              Modification.GainAbilitiesOfSource _ -> acc
              -- CR 612.1 through the Filter naming the exiled cards, printed on the
              -- granter (CR 612.3) as AddNamesMatching's is, and through the extra
              -- restrictions, printed there too. The abilities are the cards', read
              -- at projection time. Unproven: no card in data/cards/ gives such a
              -- grant a subtype or colour word.
              Modification.GainCardAbilities e ->
                Modification.GainCardAbilities
                  e
                    { CardAbilities.cards = Filter.rewrite [(from, to)] (CardAbilities.cards e),
                      CardAbilities.restrictions = fmap (rewriteRestriction [(from, to)]) (CardAbilities.restrictions e)
                    }
              -- Carries no word: the abilities are the stickers', read at projection
              -- time.
              Modification.GainAbilitiesOfStickers -> acc
              -- Carries no word: the type is read off the source at projection time.
              Modification.ChosenSubtype {} -> acc
              -- A control op carries no subtype word either.
              Modification.SetController _ -> acc
              Modification.SetControllerToSource -> acc
              -- An ability wipe names nothing at all, and neither does a P/T switch.
              Modification.LoseAllAbilities -> acc
              -- Names an ABILITY of the same card, which is no subtype word.
              Modification.LoseNamedAbility _ -> acc
              -- CR 612.3 through the removal's own keyword, exactly as GainKeyword
              -- above: "loses islandwalk" is text printed on the REMOVER, so a text
              -- change affecting it swaps the land-type word before the removal is
              -- read. Scarwood Hag's "loses forestwalk" is the printing that carries
              -- one, and it is not in the pool: every written keyword a card here
              -- removes is payload-free -- Sky Tether's flying, Melira's infect -- so
              -- this arm is a regression fence where the hacked Lord of Atlantis proves
              -- the grant above.
              Modification.LoseKeyword k -> Modification.LoseKeyword (Filter.rewriteKeyword [(from, to)] k)
              -- Nothing to rewrite, and not for want of a descent: CR 612.1 swaps a
              -- WORD, and a KeywordFamily holds none -- "all landwalk abilities" names
              -- CR 702.14a's generic term, which has no land type in it. A Hack on
              -- Hammerheim changes what its removal reaches not at all.
              Modification.LoseKeywordFamily _ -> acc
              -- CR 612.5's exchange names no word to swap: its two sides are object
              -- ids in the effect's affected set, so there is nothing here to rewrite.
              Modification.ExchangeTextBoxes -> acc
              Modification.SwitchPowerToughness -> acc
              -- Nothing to rewrite: two bare markers naming no subtype word.
              Modification.AssignCombatDamage _ -> acc
              Modification.GrantsStationToughness -> acc
              -- Nor here: an option names no word either.
              Modification.MayAssignCombatDamage _ -> acc
              -- CR 612.1 through the amount, ModifyPowerToughness's descent below.
              Modification.Intensify n -> Modification.Intensify (rewriteQuantity [(from, to)] n)
              -- CR 612.1 through both boxes: a Quantity.Count carries a Filter, so a
              -- hacked Aspect of Wolf counts the new type. rewriteQuantity is the same
              -- descent rewriteCondition and the CDA path take.
              Modification.SetBasePowerToughness pt ->
                Modification.SetBasePowerToughness
                  pt
                    { SetBasePowerToughness.power = fmap (rewriteQuantity [(from, to)]) (SetBasePowerToughness.power pt),
                      SetBasePowerToughness.toughness = fmap (rewriteQuantity [(from, to)]) (SetBasePowerToughness.toughness pt)
                    }
              Modification.ModifyPowerToughness pt ->
                Modification.ModifyPowerToughness
                  pt
                    { ModifyPowerToughness.power = rewriteQuantity [(from, to)] (ModifyPowerToughness.power pt),
                      ModifyPowerToughness.toughness = rewriteQuantity [(from, to)] (ModifyPowerToughness.toughness pt)
                    }
              -- CR 205.2a's card types are a different list from CR 205.3's subtypes, and
              -- CR 205.4a's supertypes a third, so these hold no word a pair could name.
              Modification.CardType {} -> acc
              Modification.LoseCardType _ -> acc
              Modification.AddSupertype _ -> acc
              Modification.RemoveSupertype _ -> acc
              -- The two words of a STORED text change are its own resolution's choice
              -- (CR 608.2d). A text changer's PRINTED clause is reached by rewriteEffect.
              Modification.ChangeWord {} -> acc
              -- CR 612.2's colour word used as a colour word: "is black", printed on
              -- the setter. A chosen colour is no word on the card.
              Modification.Colors mode cs -> Modification.Colors mode (colors cs)
              Modification.ChosenColor _ -> acc
   in List.foldl' apply1 m pairs

-- rewriteModification's sibling for the other half of a static ability. Under CR
-- 612.1 an ability's affected clause is rules text like any other, so a hacked
-- Kormus Bell animates Islands. Exhaustive over Affected: a new arm carrying a
-- Filter must break this build rather than silently keep the old word.
rewriteAffected :: [(TextWord.TextWord, TextWord.TextWord)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs a = case a of
  Affected.Matching f -> Affected.Matching (Filter.rewrite pairs f)
  Affected.MatchingAnywhere f -> Affected.MatchingAnywhere (Filter.rewrite pairs f)
  Affected.MatchingOffBattlefield f -> Affected.MatchingOffBattlefield (Filter.rewrite pairs f)
  Affected.AttachedPlayerControls f -> Affected.AttachedPlayerControls (Filter.rewrite pairs f)
  -- A frozen id set names no word (CR 611.2c), and an attachment or a rider
  -- names none.
  Affected.TheseObjects _ -> a
  Affected.Attached -> a
  Affected.PlayedThisWay _ -> a

-- CR 612.1's word swap over a PlayerEffect, rewriteModification's sibling
-- for the CR 613.10/613.11 axis. An Artificial Evolution resolved at an
-- Edgewalker moves "Cleric spells you cast cost {W}{B} less to cast" onto the new
-- word, because the word naming which spells the ability discounts is text
-- printed on the permanent like any other.
--
-- HERE, beside the other printed-text rewrites, and NOT in
-- Parun.Engine.PlayerEffect, which is otherwise the only module that may case on
-- this type: rewriteEffect's descent into a player effect has to reach it for
-- a restriction a RESOLUTION stores (Liliana, Untouched by Death's -3), and
-- Parun.Engine.PlayerEffect imports this module rather than the other way round.
-- The exception is the same one rewriteEffect itself takes on the Effect type:
-- this cases on STRUCTURE -- does this arm carry a Filter a swap could reach --
-- and never on which player effect it is, so no rule in the closed half learns an
-- effect's identity from it. Every other reader still asks a typed question of
-- Parun.Engine.PlayerEffect and never sees a constructor, and this module's own
-- other handling of the rows stays opaque: they ride
-- ProjectedCharacteristics.playerAbilities so a copy acquires them (CR 707.2a),
-- and nothing there looks inside one.
--
-- The shape Parun.Engine.CombatRestriction takes for a restriction -- destructure
-- the type, hand each inner value to the module that owns it -- with
-- Parun.Engine.Filter.rewrite doing the descent.
--
-- Exhaustive rather than a catch-all, for rewriteModification's stated reason: a
-- later arm that can hold a word must break this build instead of silently
-- keeping the printed one.
--
-- CR 612.2's family gate is not restated at the Filter descent, for the reason
-- Filter.rewrite's own comment gives: a HasSubtype atom may name a word of any
-- family, so the family the word is used AS is the family it belongs to, and the
-- exact lookup already asks CR 612.2's question. A Magical Hack's land-type pair
-- therefore leaves Edgewalker's Cleric alone.
rewritePlayerEffect :: [(TextWord.TextWord, TextWord.TextWord)] -> PlayerEffect.PlayerEffect -> PlayerEffect.PlayerEffect
rewritePlayerEffect pairs effect = case effect of
  -- The arms carrying a Filter, which is the only place in this type a subtype
  -- word can hide. Thalia's "noncreature spells", Vedalken Orrery's "spells",
  -- Prowling Serpopard's "creature spells", Heartstone's "activated abilities of
  -- creatures", Damping Engine's "artifact, creature, or enchantment spells",
  -- Oppressive Rays' "enchanted creature" and Yawgmoth's Will's "spells" and
  -- Omniscience's "spells" name none today; Edgewalker's "Cleric spells" does on
  -- the printed road, and Liliana, Untouched by Death's "Zombie spells" does on
  -- the stored one.
  --
  -- A cost modifier has a word in up to FIVE places: its own criterion
  -- ("nontoken Rebels"), the two target criteria (Dwarven Mauler's "that target
  -- this creature", Kopala's "that target a MERFOLK"), the count it repeats by
  -- (Sokka, Swordmaster's "for each ALLY you control") and the criterion inside
  -- each component it adds ("sacrifice a SWAMP"). All descend, which is
  -- Filter.rewriteCost's reading of CR 612.2 carried to a component added to a
  -- cost rather than printed in one. The subject's criteria (a rule-702
  -- designator, CR 605.1a's and CR 606.2's classifications) are no words CR
  -- 612.2 reaches; an addition's scale is Drought's "for each black mana
  -- symbol", a colour word used as one (CR 612.2).
  PlayerEffect.ModifyCost (CostModifier.MkCostModifier subject f targets perTarget perEach first change) ->
    let change' = case change of
          CostChange.Add (CostAddition.MkCostAddition components scale) -> CostChange.Add (CostAddition.MkCostAddition (fmap (Filter.rewriteComponent pairs) components) (rewriteCostScale pairs scale))
          CostChange.Increase _ -> change
          CostChange.Reduce _ -> change
     in PlayerEffect.ModifyCost (CostModifier.MkCostModifier subject (Filter.rewrite pairs f) (fmap (Filter.rewrite pairs) targets) (fmap (rewritePerTarget pairs) perTarget) (fmap (rewriteQuantity pairs) perEach) first change')
  -- No filter to rewrite: a rule-702 designator is not a word CR 612.1 swaps.
  PlayerEffect.AlternativeActivationCost _ -> effect
  PlayerEffect.CastAsThoughItHadFlash f first -> PlayerEffect.CastAsThoughItHadFlash (Filter.rewrite pairs f) first
  PlayerEffect.NextSpell f -> PlayerEffect.NextSpell (Filter.rewrite pairs f)
  PlayerEffect.MayPlayAsThoughItHadFlash f -> PlayerEffect.MayPlayAsThoughItHadFlash (Filter.rewrite pairs f)
  -- A rule-702 designator is no word CR 612.1 swaps; the two grants' Filters
  -- name the permanent, so they descend as CastAsThoughItHadFlash's does.
  PlayerEffect.KeywordRiderGrant grant -> PlayerEffect.KeywordRiderGrant grant {KeywordRiderGrant.permanents = Filter.rewrite pairs (KeywordRiderGrant.permanents grant)}
  PlayerEffect.LoyaltyGrant grant -> PlayerEffect.LoyaltyGrant grant {LoyaltyGrant.permanents = Filter.rewrite pairs (LoyaltyGrant.permanents grant)}
  PlayerEffect.MayTapForMana grant -> PlayerEffect.MayTapForMana (TapForMana.MkTapForMana (Filter.rewrite pairs (TapForMana.permanents grant)) (fmap (rewriteManaRestriction pairs) (TapForMana.restriction grant)))
  PlayerEffect.CantBeCountered (CounterProhibition.MkCounterProhibition countered counterers) -> PlayerEffect.CantBeCountered (CounterProhibition.MkCounterProhibition (Filter.rewrite pairs countered) (Filter.rewrite pairs counterers))
  -- CR 612.2's colour word in Autumn's Veil's "blue or black spells", and the
  -- word naming what is protected.
  PlayerEffect.RestrictTargeting r -> PlayerEffect.RestrictTargeting r {TargetRestriction.affected = Filter.rewrite pairs (TargetRestriction.affected r), TargetRestriction.aimers = Filter.rewrite pairs (TargetRestriction.aimers r)}
  PlayerEffect.TargetPastHexproof e -> PlayerEffect.TargetPastHexproof e {HexproofExemption.permanents = Filter.rewrite pairs (HexproofExemption.permanents e)}
  PlayerEffect.CantCastMatching f -> PlayerEffect.CantCastMatching (Filter.rewrite pairs f)
  PlayerEffect.PutTappedAsYouCascade f -> PlayerEffect.PutTappedAsYouCascade (Filter.rewrite pairs f)
  -- CR 101.2's play-side prohibition, narrowed by the same kind of Filter.
  PlayerEffect.CantPlayLands f -> PlayerEffect.CantPlayLands (Filter.rewrite pairs f)
  PlayerEffect.CastFrom grant -> PlayerEffect.CastFrom grant {CastFromZone.matching = Filter.rewrite pairs (CastFromZone.matching grant), CastFromZone.additionalCosts = fmap (Filter.rewriteComponent pairs) (CastFromZone.additionalCosts grant)}
  PlayerEffect.PlotFrom grant -> PlayerEffect.PlotFrom grant {PlotFromZone.matching = Filter.rewrite pairs (PlotFromZone.matching grant)}
  PlayerEffect.CastFromHandWithoutPayingManaCost grant -> PlayerEffect.CastFromHandWithoutPayingManaCost grant {FreeCastFromHand.spells = Filter.rewrite pairs (FreeCastFromHand.spells grant), FreeCastFromHand.amount = fmap (rewriteQuantity pairs) (FreeCastFromHand.amount grant)}
  -- CR 702.16a's quality where the CARD states it, so a subtype word in it is a
  -- word on the card and CR 612.1 swaps it; the chosen-name sibling below is the
  -- arm CR 612.2's second sentence keeps a swap off.
  PlayerEffect.HasProtectionFrom f -> PlayerEffect.HasProtectionFrom (Filter.rewrite pairs f)
  -- CR 612.2's colour word in "white mana" (Celestial Dawn, Sunglasses of Urza)
  -- and "green mana" (Omnath, Locus of Mana): rewriteManaFilter's reading.
  PlayerEffect.DontLoseUnspentMana f -> PlayerEffect.DontLoseUnspentMana (rewriteManaFilter pairs f)
  PlayerEffect.SpendManaAsThough clause -> PlayerEffect.SpendManaAsThough (rewriteSpendManaAsThough pairs clause)
  -- CR 612.1 through the source and recipient criteria, as the replacement
  -- arm's DamageR descent below. A REGRESSION FENCE: every printing in
  -- `data/cards/` names a card type or none there.
  PlayerEffect.DamageCantBePrevented p -> PlayerEffect.DamageCantBePrevented (rewriteDamagePattern pairs p)
  PlayerEffect.DamageCantBeRedirected p -> PlayerEffect.DamageCantBeRedirected (rewriteDamagePattern pairs p)
  -- The rest name no word a pair could reach. The two chosen-name arms carry
  -- nothing at all -- CR 201.4's names are read off the source's
  -- Object.chosenNames -- and CR 612.2's second sentence says a word swap could
  -- not touch a card name even if they did. A count and a player scope are not
  -- words either.
  PlayerEffect.CantCastSpells -> effect
  -- The designator is no word CR 612.1 swaps; the source Filter descends.
  PlayerEffect.CantActivateAbilities designator f kind -> PlayerEffect.CantActivateAbilities designator (Filter.rewrite pairs f) kind
  PlayerEffect.CantCastMoreThan _ -> effect
  PlayerEffect.PlayAdditionalLands _ -> effect
  PlayerEffect.NoMaximumHandSize -> effect
  PlayerEffect.SetMaximumHandSize _ -> effect
  PlayerEffect.IncreaseMaximumHandSize _ -> effect
  PlayerEffect.ReduceMaximumHandSize _ -> effect
  PlayerEffect.LoseLifeForUnspentMana -> effect
  PlayerEffect.MayPayLifeForMana {} -> effect
  -- CR 702.11d's quality is a word on the card, as for protection above.
  PlayerEffect.CantBeTargetedBy scope quality -> PlayerEffect.CantBeTargetedBy scope (fmap (Filter.rewrite pairs) quality)
  PlayerEffect.CantSearchLibraries _ -> effect
  PlayerEffect.CantBecomeMonarch -> effect
  PlayerEffect.StateBasedActionDoesntApply _ -> effect
  PlayerEffect.CantSetSchemesInMotion -> effect
  PlayerEffect.EntriesTrigger e -> PlayerEffect.EntriesTrigger e {EntriesTrigger.entrants = Filter.rewrite pairs (EntriesTrigger.entrants e)}
  PlayerEffect.AdditionalTriggers bearers -> PlayerEffect.AdditionalTriggers (Filter.rewrite pairs bearers)
  PlayerEffect.ControlledWhileSearching -> effect
  PlayerEffect.BlocksFromPiles -> effect
  PlayerEffect.YouChoose _ -> effect
  PlayerEffect.ExilesFoundWhileSearching -> effect
  PlayerEffect.CantAttackWithCreatures -> effect
  PlayerEffect.CastOnlyAtSorcerySpeed -> effect
  PlayerEffect.PlayLandsFrom _ -> effect
  -- A counter KIND is not a word CR 612.2's pairs could reach either.
  PlayerEffect.CantGetCounters _ -> effect
  -- Nor is a coin's face, or the two flags beside it.
  PlayerEffect.StateCoinFlip _ -> effect
  -- Nor is a die's size, the number it came up, or which of CR 706.2b's two
  -- buckets the modifier is in. Rule 706.2a's COST is text a swap reaches --
  -- Wall of Fortune's "tap an untapped Wall you control" names a subtype -- so
  -- it descends where the rest of the payload does not.
  PlayerEffect.ModifyDieRoll modified ->
    PlayerEffect.ModifyDieRoll modified {ModifiedRoll.cost = fmap (Filter.rewriteCost pairs) (ModifiedRoll.cost modified)}
  PlayerEffect.AdditionalVotes _ -> effect
  PlayerEffect.AdditionalSurveilCards _ -> effect
  PlayerEffect.CantGainLife -> effect
  PlayerEffect.CantLoseLife -> effect
  PlayerEffect.RevealsDraw _ -> effect
  PlayerEffect.Precluded _ -> effect
  PlayerEffect.Shows _ -> effect

-- CR 612.1: AddMana's and Firebend's shared payload, each quantity and Filter
-- in it rewritten.
rewriteManaAddition :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaAddition.ManaAddition -> ManaAddition.ManaAddition
rewriteManaAddition pairs addition =
  addition
    { ManaAddition.production = rewriteManaProduction pairs (ManaAddition.production addition),
      ManaAddition.count = rewriteQuantity pairs (ManaAddition.count addition),
      ManaAddition.restriction = fmap (rewriteManaRestriction pairs) (ManaAddition.restriction addition),
      ManaAddition.rider = fmap (rewriteManaRider pairs) (ManaAddition.rider addition),
      ManaAddition.whenSpent = fmap (\w -> w {WhenSpent.casts = Filter.rewrite pairs (WhenSpent.casts w)}) (ManaAddition.whenSpent addition)
    }

-- CR 612.1: the one production holding a Filter, CR 106.7's group, has it
-- rewritten.
rewriteManaProduction :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaProduction.ManaProduction -> ManaProduction.ManaProduction
rewriteManaProduction pairs production = case production of
  ManaProduction.OfType _ -> production
  ManaProduction.AnyColor _ -> production
  ManaProduction.Chosen -> production
  ManaProduction.SnowSymbol -> production
  ManaProduction.Noted -> production
  ManaProduction.CouldProduce could -> ManaProduction.CouldProduce could {CouldProduce.among = Filter.rewrite pairs (CouldProduce.among could)}
  ManaProduction.Pooled _ -> production
  ManaProduction.ProducedByTapped -> production
  ManaProduction.InCommanderIdentity -> production
  ManaProduction.Circled -> production

-- CR 612's word swap over an effect's AST: Parun.Engine.Children's traversal
-- at Identity, every child that can hold printed text rewritten through its
-- own walk. Left standing: slot names (the card's own namespace) and
-- PlayerRefs (seats, not words); a card a meld or a conjure names, whose text
-- is that card's and whose name CR 612.2 keeps out of reach; the keyword an
-- offered cast is offered by; a move's, an onset's, a player effect's and an
-- attack restriction's scopes, which print no word. A record update could
-- carry a new field through unrewritten; the traversal's positional match
-- makes a new field a build error there instead.
--
-- Proved, each by the test named: a Modification by Tidal Warrior and
-- Presence of Gond, a granted ability inside one by Parun.CounterspellSpec's
-- evolved Clavileño; a mana addition's count by Parun.ManaSpec's hacked Cabal
-- Coffers; an entry rider's keys by Parun.CounterspellSpec's Synthetic Warded
-- Homecoming; a floating replacement by its evolved Moonmist; a counter kind
-- by its Synthetic Warding Sigil and Synthetic Erode the Warding; the shields'
-- recipient and source Filters by data/scenarios/prevention's CR 612.1
-- scenarios (Synthetic Warding Chant, Pack Leader, the redirection's chosen
-- source). Every
-- other position is a regression fence: no card in data/cards/ writes a word
-- a text changer's family (Parun.Types.WordFamily) can swap there.
rewriteEffect :: [(TextWord.TextWord, TextWord.TextWord)] -> Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteEffect pairs effect = case effect of
  -- CR 612.1: a text changer's own restriction clause is text like any other.
  Effect.ChangeText (ChangeText.MkChangeText families forbidden slot duration) ->
    Effect.ChangeText (ChangeText.MkChangeText families (Set.map (swapTextWordIn families pairs) forbidden) slot (rewriteDuration pairs duration))
  -- CR 612.2a: amass's subtype is a printed word of CR 205.3m's family, and
  -- the token's own name follows it.
  Effect.Amass (Amass.MkAmass player quantity kind slot) ->
    Effect.Amass (Amass.MkAmass player (rewriteQuantity pairs quantity) (rewriteAmassKind pairs kind) slot)
  _ ->
    Identity.runIdentity
      ( Children.effectChildren
          Children.effectLeaves
            { Children.onFilter = Identity.Identity . Filter.rewrite pairs,
              Children.onQuantity = Identity.Identity . rewriteQuantity pairs,
              Children.onEffect = Identity.Identity . rewriteEffect pairs,
              Children.onObjectRef = Identity.Identity . rewriteObjectRef pairs,
              Children.onDuration = Identity.Identity . rewriteDuration pairs,
              Children.onCondition = Identity.Identity . rewriteCondition pairs,
              Children.onCard = Identity.Identity . rewriteCard pairs,
              Children.onAbility = Identity.Identity . rewriteGrantedAbility pairs,
              Children.onCost = Identity.Identity . Filter.rewriteCost pairs,
              Children.onCounterKind = Identity.Identity . Filter.rewriteCounterKind pairs,
              Children.onWhichCounters = Identity.Identity . Filter.rewriteWhichCounters pairs,
              Children.onMovedKinds = Identity.Identity . rewriteMovedKinds pairs,
              Children.onRemovalCount = Identity.Identity . rewriteRemovalCount pairs,
              Children.onModification = Identity.Identity . rewriteModification pairs,
              Children.onPlayerEffect = Identity.Identity . rewritePlayerEffect pairs,
              Children.onReplacement = Identity.Identity . rewriteReplacementEffect pairs,
              Children.onPayGate = Identity.Identity . rewritePayGate pairs,
              Children.onEntryRiders = Identity.Identity . rewriteEntryRiders pairs,
              Children.onPlacement = Identity.Identity . rewritePlacement pairs,
              Children.onCopyException = Identity.Identity . rewriteCopyException pairs,
              Children.onFaceDown = Identity.Identity . rewriteListing pairs,
              Children.onManaAddition = Identity.Identity . rewriteManaAddition pairs,
              Children.onChosenPermanents = Identity.Identity . rewriteChosenPermanents pairs
            }
          effect
      )

-- Swaps applied in order, as one table: each word any swap names, paired with
-- the word the whole sequence leaves it as, and dropped where that is itself.
-- A swap never crosses families, so a colour pair and a subtype pair never meet.
composeWordChanges :: [(TextWord.TextWord, TextWord.TextWord)] -> [(TextWord.TextWord, TextWord.TextWord)]
composeWordChanges pairs =
  let image s = List.foldl' (\x (from, to) -> if x == from then to else x) s pairs
   in Map.toList (Map.filterWithKey (/=) (Map.fromSet image (Set.fromList (fmap fst pairs))))

-- CR 612.2 over a mana type a card WRITES as a word -- "white mana", "spent as
-- though it were red mana" -- which is a colour word used as one, where a mana
-- symbol ("Add {W}") is no word (Glamerdye's ruling of 2008-08-01) and an effect
-- producing mana is never rewritten. Colorless is no colour (CR 105.4) and so no
-- colour word.
rewriteManaFilter :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaFilter.ManaFilter -> ManaFilter.ManaFilter
rewriteManaFilter pairs f = case f of
  ManaFilter.Any -> f
  ManaFilter.OfType t -> ManaFilter.OfType (rewriteWrittenManaType pairs t)
  ManaFilter.NotOfType t -> ManaFilter.NotOfType (rewriteWrittenManaType pairs t)

rewriteWrittenManaType :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaType.ManaType -> ManaType.ManaType
rewriteWrittenManaType pairs t = case t of
  ManaType.Colored c -> ManaType.Colored (TextWord.Engine.colorAfter pairs c)
  ManaType.Colorless -> t

-- Both halves of CR 609.4b's clause are written as words: Sunglasses of Urza's
-- "white mana as though it were red mana".
rewriteSpendManaAsThough :: [(TextWord.TextWord, TextWord.TextWord)] -> SpendManaAsThough.SpendManaAsThough -> SpendManaAsThough.SpendManaAsThough
rewriteSpendManaAsThough pairs clause =
  let asThough = SpendManaAsThough.asThough clause
      -- CR 609.4b's "mana of any color" (Celestial Dawn) names no colour word,
      -- and a set holding all five can only have been written that way.
      anyColor = all (\c -> Set.member (ManaType.Colored c) asThough) [minBound ..]
   in clause
        { SpendManaAsThough.which = rewriteManaFilter pairs (SpendManaAsThough.which clause),
          SpendManaAsThough.asThough = if anyColor then asThough else Set.map (rewriteWrittenManaType pairs) asThough,
          SpendManaAsThough.casting = fmap (Filter.rewrite pairs) (SpendManaAsThough.casting clause)
        }

-- CR 612.1 through a damage pattern's two criteria.
rewriteDamagePattern :: [(TextWord.TextWord, TextWord.TextWord)] -> DamagePattern.DamagePattern -> DamagePattern.DamagePattern
rewriteDamagePattern pairs p =
  p
    { DamagePattern.whatSource = Filter.rewrite pairs (DamagePattern.whatSource p),
      DamagePattern.whatRecipient = fmap (Filter.rewrite pairs) (DamagePattern.whatRecipient p)
    }

-- CR 612.2 over CostScale.PerColoredSymbol's colour, Drought's "black mana
-- symbol"; a regression fence, no test changing Drought's text.
rewriteCostScale :: [(TextWord.TextWord, TextWord.TextWord)] -> CostScale.CostScale -> CostScale.CostScale
rewriteCostScale pairs scale = case scale of
  CostScale.Once -> scale
  CostScale.PerColoredSymbol c -> CostScale.PerColoredSymbol (TextWord.Engine.colorAfter pairs c)

-- CR 612.2 over one word whose family a card's text names rather than a
-- constructor -- a ChangeText's forbidden-word set, an amass's creature type.
swapTextWordIn :: Set.Set WordFamily.WordFamily -> [(TextWord.TextWord, TextWord.TextWord)] -> TextWord.TextWord -> TextWord.TextWord
swapTextWordIn families pairs word =
  let step s (from, to) = if s == from && any (`TextWord.Engine.inFamily` from) families then to else s
   in List.foldl' step word pairs

-- An amass's subtype word (CR 612.2a); empower Jace prints none.
rewriteAmassKind :: [(TextWord.TextWord, TextWord.TextWord)] -> AmassKind.AmassKind -> AmassKind.AmassKind
rewriteAmassKind pairs kind = case kind of
  AmassKind.Army subtype -> AmassKind.Army (swapWordIn WordFamily.CreatureType pairs subtype)
  AmassKind.EmpowerJace -> AmassKind.EmpowerJace

-- swapTextWordIn over a subtype word of one family.
swapWordIn :: WordFamily.WordFamily -> [(TextWord.TextWord, TextWord.TextWord)] -> Subtype.Type.Subtype -> Subtype.Type.Subtype
swapWordIn family pairs s = case swapTextWordIn (Set.singleton family) pairs (TextWord.SubtypeWord s) of
  TextWord.SubtypeWord t -> t
  TextWord.ColorWord _ -> s

-- CR 612.1 through an ObjectRef. An InSlot names an object chosen at cast time,
-- and the player-naming arms hold no subtype word; only the Filters and the
-- Quantities can carry one -- the Filter that says what ends a walk of a library
-- included, and ChosenCardInGraveyard's, ChosenCardFromAmong's,
-- RandomCard's and RandomCardInGraveyard's counts beside the two library walks'.
rewriteObjectRef :: [(TextWord.TextWord, TextWord.TextWord)] -> ObjectRef.ObjectRef -> ObjectRef.ObjectRef
rewriteObjectRef pairs ref = case ref of
  ObjectRef.InSlot _ -> ref
  ObjectRef.EachMatching f -> ObjectRef.EachMatching (Filter.rewrite pairs f)
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s f) -> ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s (Filter.rewrite pairs f))
  ObjectRef.EachCardInYourHand -> ref
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s f) -> ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s (fmap (Filter.rewrite pairs) f))
  -- A REGRESSION FENCE rather than proven behaviour: the one printing that
  -- states this Filter is under a triggered ability no card in the pool changes
  -- the text of, so dropping the rewrite here leaves the suite green.
  ObjectRef.EachCardInYourLibrary f -> ObjectRef.EachCardInYourLibrary (fmap (Filter.rewrite pairs) f)
  ObjectRef.EachCardYouOwn f -> ObjectRef.EachCardYouOwn (Filter.rewrite pairs f)
  ObjectRef.EachCardInExiledPile e -> ObjectRef.EachCardInExiledPile e {EachCardInExiledPile.filter = fmap (Filter.rewrite pairs) (EachCardInExiledPile.filter e)}
  ObjectRef.EachCardEncodedOnSource f -> ObjectRef.EachCardEncodedOnSource (fmap (Filter.rewrite pairs) f)
  ObjectRef.EachSpell f -> ObjectRef.EachSpell (Filter.rewrite pairs f)
  ObjectRef.EachAbility f -> ObjectRef.EachAbility (Filter.rewrite pairs f)
  ObjectRef.EachOnStack f -> ObjectRef.EachOnStack (Filter.rewrite pairs f)
  -- Proved by the scenario
  -- cr-612-2-a-sleighted-disorder-burns-each-player-who-controls-a-green-creature.
  ObjectRef.Players p -> ObjectRef.Players p {PlayersWho.who = fmap (Filter.rewrite pairs) (PlayersWho.who p)}
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p c) -> ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p (rewriteQuantity pairs c))
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p f c) -> ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  -- Names a POSITION and a seat, and neither is a subtype word, so CR 612.1 has
  -- nothing to swap: the arms above reach one only through a Filter or a
  -- Quantity, and this one carries neither.
  ObjectRef.TopOfGraveyard _ -> ref
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s f n) -> ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s (Filter.rewrite pairs f) (rewriteHowMany pairs n))
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p f n) -> ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p (Filter.rewrite pairs f) (rewriteHowMany pairs n))
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n f c w p u r) -> ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n (Filter.rewrite pairs f) (rewriteHowMany pairs c) w p u r)
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n f) -> ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n (Filter.rewrite pairs f))
  -- A REGRESSION FENCE rather than proven behaviour, EachCardInYourLibrary's
  -- reason: no card in the pool narrows a random pick by a land type, and no
  -- printing changes the text of the ones that write this ref.
  ObjectRef.RandomCard (RandomCard.MkRandomCard z p f c) -> ObjectRef.RandomCard (RandomCard.MkRandomCard z p (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  -- The arm above's regression fence, for its reason: Ghoulraiser's Zombie is a
  -- creature type and no printing changes this trigger's text.
  ObjectRef.RandomCardInGraveyard (RandomCardInGraveyard.MkRandomCardInGraveyard s f c) -> ObjectRef.RandomCardInGraveyard (RandomCardInGraveyard.MkRandomCardInGraveyard s (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  ObjectRef.ChosenPermanents choice -> ObjectRef.ChosenPermanents (rewriteChosenPermanents pairs choice)
  ObjectRef.AttachedToBound (AttachedToBound.MkAttachedToBound slot f) -> ObjectRef.AttachedToBound (AttachedToBound.MkAttachedToBound slot (Filter.rewrite pairs f))
  ObjectRef.AttackedBy _ -> ref
  ObjectRef.FromAnywhere _ -> ref
  ObjectRef.AtRandom inner -> ObjectRef.AtRandom (rewriteObjectRef pairs inner)
  ObjectRef.AllOf refs -> ObjectRef.AllOf (fmap (rewriteObjectRef pairs) refs)

-- CR 612.1 through a battlefield choice: its Filter and its count are card
-- text, and its chooser names a seat, never a subtype.
rewriteChosenPermanents :: [(TextWord.TextWord, TextWord.TextWord)] -> ChosenPermanents.ChosenPermanents -> ChosenPermanents.ChosenPermanents
rewriteChosenPermanents pairs (ChosenPermanents.MkChosenPermanents f chooser count zone) =
  ChosenPermanents.MkChosenPermanents (Filter.rewrite pairs f) chooser (rewriteHowMany pairs count) zone

rewriteHowMany :: [(TextWord.TextWord, TextWord.TextWord)] -> HowMany.HowMany -> HowMany.HowMany
rewriteHowMany pairs count = case count of
  HowMany.Exactly n -> HowMany.Exactly (rewriteQuantity pairs n)
  HowMany.UpTo n -> HowMany.UpTo (fmap (rewriteQuantity pairs) n)

-- CR 612.1/612.2a through the CARD an Effect.Create or an Effect.CreateEmblem
-- defines its token or emblem with: the type line, the name, and the rules text.
-- The NAME's change is gated on the type line, since CR 612.2a licenses it only
-- where the word is used as a creature type. The RULES TEXT is walked
-- unconditionally, and an emblem is why: CR 114.3 leaves it no type line at all.
-- Recursive, and terminating because a Card is a finite first-order value and
-- every step descends into a strict subterm. Every FACE, since a card's printed
-- subtypes, name and text are per-face (CR 712.6).
rewriteCard :: [(TextWord.TextWord, TextWord.TextWord)] -> Card.Type.Card -> Card.Type.Card
rewriteCard pairs card = card {Card.Type.faces = fmap (rewriteFace pairs) (Card.Type.faces card)}

rewriteFace :: [(TextWord.TextWord, TextWord.TextWord)] -> Face.Face Card.Type.Card -> Face.Face Card.Type.Card
rewriteFace pairs face =
  let apply1 f (from, to) =
        let pair = [(from, to)]
            renamed = case swapSubtype (from, to) (Face.typeLine f) of
              Just (old, new, typeLine) ->
                f
                  { Face.typeLine = typeLine,
                    Face.name = rewriteTokenName old new (Face.name f)
                  }
              Nothing -> f
         in renamed
              { -- CR 612.2's colour word in the defining text ("a 1/1 black Rat
                -- token"), which is where a token's colour comes from (CR 111.3).
                Face.colorIndicator = Set.map (TextWord.Engine.colorAfter pair) (Face.colorIndicator renamed),
                -- CR 702.14a's land-type word inside a landwalk. Map.mapKeysWith
                -- (+), so two instances the rewrite collapses onto one keyword
                -- stay two instances.
                Face.keywords = Map.mapKeysWith (+) (Filter.rewriteKeyword pair) (Face.keywords renamed),
                -- CR 208.2a's star, unevaluated as at layer 3.
                Face.characteristicPT = fmap (rewriteCharacteristicPT pair) (Face.characteristicPT renamed),
                -- CR 101.1's ceiling on X, whose Quantity can Count a criterion
                -- naming a land type word. A regression fence: neither printing
                -- pairs a bounded X with one -- both say "the greatest toughness
                -- among creatures you control" -- so no test can falsify it.
                Face.maximumX = fmap (rewriteQuantity pair) (Face.maximumX renamed),
                Face.spell = rewriteModal pair (Face.spell renamed),
                Face.activatedAbilities = fmap (rewriteActivatedAbility pair) (Face.activatedAbilities renamed),
                -- CR 604.2's static ability, on the card a token or emblem is
                -- defined with. A regression fence rather than a proved behaviour,
                -- as Face.maximumX above is: a walk of every defined face in
                -- data/cards for a replacementEffects key (2026-08-21) found none
                -- at all, so no board can tell this line from its absence.
                -- Parun.ReplacementSpec's Dragonstorm Globe case is what proves
                -- rewritePrintedReplacement itself.
                Face.replacementEffects = fmap (rewritePrintedReplacement pair) (Face.replacementEffects renamed),
                Face.triggeredAbilities = fmap (rewriteTriggeredAbility pair) (Face.triggeredAbilities renamed),
                Face.delayedAbilities = fmap (rewriteTriggeredAbility pair) (Face.delayedAbilities renamed),
                Face.staticAbilities = fmap (rewriteStaticAbility pair) (Face.staticAbilities renamed)
              }
   in List.foldl' apply1 face pairs

-- A whole static ability under CR 612.1, for the defined-card walk above. A
-- permanent's own statics are reached piecemeal instead, being read per layer.
rewriteStaticAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> StaticAbility.StaticAbility (GrantedAbility.GrantedAbility Card.Type.Card) -> StaticAbility.StaticAbility (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteStaticAbility pairs sa =
  sa
    { StaticAbility.affected = rewriteAffected pairs (StaticAbility.affected sa),
      StaticAbility.condition = fmap (rewriteCondition pairs) (StaticAbility.condition sa),
      StaticAbility.modifications = fmap (rewriteModification pairs) (StaticAbility.modifications sa)
    }

-- A player static ability under CR 612.1: its clause and its effect, the two
-- halves Parun.Engine.PlayerEffect.printedRows rewrites.
rewritePlayerStaticAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> PlayerStaticAbility.PlayerStaticAbility -> PlayerStaticAbility.PlayerStaticAbility
rewritePlayerStaticAbility pairs ability =
  ability
    { PlayerStaticAbility.condition = fmap (rewriteCondition pairs) (PlayerStaticAbility.condition ability),
      PlayerStaticAbility.effect = rewritePlayerEffect pairs (PlayerStaticAbility.effect ability)
    }

-- CR 613.11's fifteen families under CR 612.1, each at the positions its
-- gatherer module rewrites (Parun.Engine.CombatRestriction and its siblings), so
-- the layer fold and the gatherers agree on where a word is.
rewriteRuleAbilities :: [(TextWord.TextWord, TextWord.TextWord)] -> RuleAbilities.RuleAbilities -> RuleAbilities.RuleAbilities
rewriteRuleAbilities pairs abilities =
  let affected = rewriteAffected pairs
      condition :: Maybe Condition.Type.Condition -> Maybe Condition.Type.Condition
      condition = fmap (rewriteCondition pairs)
   in RuleAbilities.MkRuleAbilities
        { RuleAbilities.activationProhibitions = fmap (\x -> x {ActivationProhibition.affected = affected (ActivationProhibition.affected x)}) (RuleAbilities.activationProhibitions abilities),
          RuleAbilities.attachRestrictions = fmap (\x -> x {AttachRestriction.affected = affected (AttachRestriction.affected x), AttachRestriction.attachers = Filter.rewrite pairs (AttachRestriction.attachers x)}) (RuleAbilities.attachRestrictions abilities),
          RuleAbilities.attackCosts = fmap (rewriteAttackCost pairs) (RuleAbilities.attackCosts abilities),
          RuleAbilities.rulePermissions = fmap (\x -> x {RulePermission.affected = affected (RulePermission.affected x), RulePermission.while = condition (RulePermission.while x)}) (RuleAbilities.rulePermissions abilities),
          RuleAbilities.attackRequirements = fmap (\x -> x {AttackRequirement.subject = affected (AttackRequirement.subject x), AttackRequirement.while = condition (AttackRequirement.while x)}) (RuleAbilities.attackRequirements abilities),
          RuleAbilities.blockCosts = fmap (rewriteBlockCost pairs) (RuleAbilities.blockCosts abilities),
          RuleAbilities.blockPermissions = fmap (\x -> x {BlockPermission.affected = affected (BlockPermission.affected x), BlockPermission.additional = fmap (rewriteQuantity pairs) (BlockPermission.additional x), BlockPermission.while = condition (BlockPermission.while x)}) (RuleAbilities.blockPermissions abilities),
          RuleAbilities.blockRequirements = fmap (\x -> x {BlockRequirement.subject = fmap affected (BlockRequirement.subject x), BlockRequirement.attacker = fmap affected (BlockRequirement.attacker x), BlockRequirement.while = condition (BlockRequirement.while x)}) (RuleAbilities.blockRequirements abilities),
          RuleAbilities.combatRestrictions = fmap (rewriteCombatRestriction pairs) (RuleAbilities.combatRestrictions abilities),
          RuleAbilities.counterRestrictions = fmap (\x -> x {CounterRestriction.affected = affected (CounterRestriction.affected x)}) (RuleAbilities.counterRestrictions abilities),
          RuleAbilities.crewRestrictions = fmap (\x -> x {CrewRestriction.affected = affected (CrewRestriction.affected x)}) (RuleAbilities.crewRestrictions abilities),
          RuleAbilities.entryRestrictions = fmap (\x -> x {EntryRestriction.affected = affected (EntryRestriction.affected x)}) (RuleAbilities.entryRestrictions abilities),
          RuleAbilities.sacrificeRestrictions = fmap (\x -> x {SacrificeRestriction.affected = affected (SacrificeRestriction.affected x)}) (RuleAbilities.sacrificeRestrictions abilities),
          RuleAbilities.targetRestrictions = fmap (\x -> x {TargetRestriction.affected = affected (TargetRestriction.affected x), TargetRestriction.aimers = Filter.rewrite pairs (TargetRestriction.aimers x)}) (RuleAbilities.targetRestrictions abilities),
          RuleAbilities.untapRestrictions = fmap (\x -> x {UntapRestriction.affected = affected (UntapRestriction.affected x), UntapRestriction.while = condition (UntapRestriction.while x)}) (RuleAbilities.untapRestrictions abilities)
        }

-- A CR 508.1h cost to attack under CR 612.1: who it taxes, what each share
-- costs and its gate. Its scope names no word. Parun.Engine.AttackCost.costsOn
-- applies it to the source's own costs.
rewriteAttackCost :: [(TextWord.TextWord, TextWord.TextWord)] -> AttackCost.AttackCost -> AttackCost.AttackCost
rewriteAttackCost pairs cost =
  cost
    { AttackCost.subject = rewriteAffected pairs (AttackCost.subject cost),
      AttackCost.perAttacker = rewritePerCreature pairs (AttackCost.perAttacker cost),
      AttackCost.while = fmap (rewriteCondition pairs) (AttackCost.while cost)
    }

-- A CR 509.1d cost to block under CR 612.1, rewriteAttackCost's twin plus the
-- attackers it is owed against. Parun.Engine.BlockCost.costsOn applies it.
rewriteBlockCost :: [(TextWord.TextWord, TextWord.TextWord)] -> BlockCost.BlockCost -> BlockCost.BlockCost
rewriteBlockCost pairs cost =
  cost
    { BlockCost.subject = rewriteAffected pairs (BlockCost.subject cost),
      BlockCost.perBlocker = rewritePerCreature pairs (BlockCost.perBlocker cost),
      BlockCost.attackers = fmap (Filter.rewrite pairs) (BlockCost.attackers cost),
      BlockCost.while = fmap (rewriteCondition pairs) (BlockCost.while cost)
    }

-- One taxed creature's share under CR 612.1.
rewritePerCreature :: [(TextWord.TextWord, TextWord.TextWord)] -> PerCreature.PerCreature -> PerCreature.PerCreature
rewritePerCreature pairs share = case share of
  PerCreature.Fixed cost -> PerCreature.Fixed (Filter.rewriteCost pairs cost)
  PerCreature.Counted quantity -> PerCreature.Counted (rewriteQuantity pairs quantity)

-- One CR 508.1c / 509.1b restriction under CR 612.1: its subject, its gate, its
-- amount and the other side of the pair it describes, as Parun.Engine.CombatRestriction
-- rewrites them.
-- Exhaustive, so a new arm has to say where its words are.
rewriteCombatRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> CombatRestriction.CombatRestriction -> CombatRestriction.CombatRestriction
rewriteCombatRestriction pairs restriction =
  let unlessOf :: Maybe Condition.Type.Condition -> Maybe Condition.Type.Condition
      unlessOf = fmap (rewriteCondition pairs)
      affectedUnless x = x {AffectedUnless.affected = rewriteAffected pairs (AffectedUnless.affected x), AffectedUnless.unless = unlessOf (AffectedUnless.unless x), AffectedUnless.amount = fmap (rewriteQuantity pairs) (AffectedUnless.amount x)}
      aloneUnless x = x {AloneUnless.affected = rewriteAffected pairs (AloneUnless.affected x), AloneUnless.alongside = fmap (Filter.rewrite pairs) (AloneUnless.alongside x), AloneUnless.unless = unlessOf (AloneUnless.unless x)}
   in case restriction of
        CombatRestriction.CantAttack x -> CombatRestriction.CantAttack (affectedUnless x)
        CombatRestriction.CantBlock x -> CombatRestriction.CantBlock (affectedUnless x)
        CombatRestriction.CantAttackAlone x -> CombatRestriction.CantAttackAlone (aloneUnless x)
        CombatRestriction.CantBlockAlone x -> CombatRestriction.CantBlockAlone (aloneUnless x)
        CombatRestriction.CantBeBlockedBy x ->
          CombatRestriction.CantBeBlockedBy
            x
              { CantBeBlockedBy.affected = rewriteAffected pairs (CantBeBlockedBy.affected x),
                CantBeBlockedBy.blockers = Filter.rewrite pairs (CantBeBlockedBy.blockers x),
                CantBeBlockedBy.unless = unlessOf (CantBeBlockedBy.unless x)
              }
        CombatRestriction.CantBlockCreatures x ->
          CombatRestriction.CantBlockCreatures
            x
              { CantBlockCreatures.affected = rewriteAffected pairs (CantBlockCreatures.affected x),
                CantBlockCreatures.attackers = Filter.rewrite pairs (CantBlockCreatures.attackers x),
                CantBlockCreatures.unless = unlessOf (CantBlockCreatures.unless x)
              }
        CombatRestriction.CantAttackPlayer x -> CombatRestriction.CantAttackPlayer x {CantAttackPlayer.affected = rewriteAffected pairs (CantAttackPlayer.affected x), CantAttackPlayer.unless = unlessOf (CantAttackPlayer.unless x)}
        CombatRestriction.CantAttackMoreThan x -> CombatRestriction.CantAttackMoreThan x {AttackLimitUnless.unless = unlessOf (AttackLimitUnless.unless x)}
        CombatRestriction.CantBlockMoreThan x -> CombatRestriction.CantBlockMoreThan x {LimitUnless.attackers = fmap (rewriteAffected pairs) (LimitUnless.attackers x), LimitUnless.unless = unlessOf (LimitUnless.unless x)}

-- CR 612.2a's name half, gated on both words being creature types -- CR 612.2
-- prohibits every other family. EVERY occurrence, since CR 111.4's derived name
-- holds one word per subtype and Artificial Evolution replaces all instances of
-- the word; whole ones only, since CR 612.2 changes a word only where it is used
-- as a creature type, and the same letters inside a longer word of a
-- card-authored name are not. Parun.CounterspellSpec's Ursine Rite pair is what
-- proves the boundary, and its Temporal Summons pair the two-word type.
rewriteTokenName :: Subtype.Type.Subtype -> Subtype.Type.Subtype -> CardName.CardName -> CardName.CardName
rewriteTokenName from to name = case (Subtype.creatureTypeWord from, Subtype.creatureTypeWord to) of
  (Just f, Just t) -> CardName.MkCardName (replaceWholeWord f t (CardName.unwrap name))
  _ -> name

-- Every whole-word occurrence of `from` in a text replaced by `to`. Not a
-- whitespace tokenizer, because CR 205.3m's Time Lord is two words: the needle is
-- matched entire and only its two edges are checked. A word character is
-- alphanumeric, or the hyphen or apostrophe CR 205.3m writes its own types with
-- (Assembly-Worker, C'tan, Shi'ar), so those join letters into one word rather
-- than dividing it.
--
-- Total on the words creatureTypeWord answers, every one of which is non-empty;
-- Text.breakOn is partial on an empty needle.
replaceWholeWord :: Text.Text -> Text.Text -> Text.Text -> Text.Text
replaceWholeWord from to =
  let edge = maybe True notWord
      notWord c = not (Char.isAlphaNum c || c == '\'' || c == '-')
      lastChar t = if Text.null t then Nothing else Just (Text.last t)
      go acc rest =
        let (before, match) = Text.breakOn from rest
            done = acc <> before
         in if Text.null match
              then done
              else
                let after = Text.drop (Text.length from) match
                 in if edge (lastChar done) && edge (fmap fst (Text.uncons after))
                      then go (done <> to) after
                      else go (done <> Text.take 1 match) (Text.drop 1 match)
   in go Text.empty

-- CR 612.1 through CR 106.6a's spending restriction: every half but
-- keywordActivations, a designator naming no quality, is a filter of
-- printed text. See the AddMana arm above for why this is a fence rather than a
-- proof.
rewriteManaRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaRestriction.ManaRestriction -> ManaRestriction.ManaRestriction
rewriteManaRestriction pairs restriction =
  restriction
    { ManaRestriction.casts = fmap (Filter.rewrite pairs) (ManaRestriction.casts restriction),
      ManaRestriction.activations = fmap (Filter.rewrite pairs) (ManaRestriction.activations restriction),
      ManaRestriction.unlocks = fmap (Filter.rewrite pairs) (ManaRestriction.unlocks restriction),
      ManaRestriction.turnsFaceUp = fmap (Filter.rewrite pairs) (ManaRestriction.turnsFaceUp restriction)
    }

-- CR 612.1 through CR 106.6a's other clause: the rider's condition says WHICH
-- objects it is about, and that is printed text too. Its effect
-- (Parun.Types.ManaRiderEffect) names no word a pair could reach.
rewriteManaRider :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaRider.ManaRider -> ManaRider.ManaRider
rewriteManaRider pairs rider = rider {ManaRider.condition = Filter.rewrite pairs (ManaRider.condition rider)}

-- CR 612.1 over an ACTIVATED ability printed on a permanent: the payload, CR
-- 702.178a's "as long as" gate, and the ACTIVATION COST (CR 118.1, CR 602.1a),
-- so a Magical Hack naming Forest moves which land Dark Heart of the Wood's cost
-- demands.
--
-- CR 602.5's "activate only if" rider is here too, being printed text like any
-- other: Parun.ManaSpec's "Nimbus Maze text change" is what proves it, a Magical
-- Hack naming Island moving which land the {W} route's rider counts.
rewriteActivatedAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteActivatedAbility pairs ability =
  ability
    { ActivatedAbility.modal = rewriteModal pairs (ActivatedAbility.modal ability),
      ActivatedAbility.condition = fmap (rewriteCondition pairs) (ActivatedAbility.condition ability),
      ActivatedAbility.cost = Filter.rewriteCost pairs (ActivatedAbility.cost ability),
      ActivatedAbility.restrictions = fmap (rewriteRestriction pairs) (ActivatedAbility.restrictions ability),
      -- CR 101.1's ceiling on this ability's X, exactly as Face.maximumX above
      -- carries a spell's. A regression fence for that field's reason: no
      -- printing pairs a bounded X on an activated ability with a Quantity
      -- naming a land type word, so no board can tell this line from its
      -- absence.
      ActivatedAbility.maximumX = fmap (rewriteQuantity pairs) (ActivatedAbility.maximumX ability),
      -- CR 107.3c's defined X, the ceiling's fence for its reason.
      ActivatedAbility.definedX = fmap (rewriteQuantity pairs) (ActivatedAbility.definedX ability)
    }

-- CR 612.1 over a GRANTED ability (CR 613.1f), whichever of CR 113.3's kinds it
-- is. The words are printed on the GRANTER, so a text change affecting that
-- permanent rewrites them before layer 6 hands them over.
rewriteGrantedAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> GrantedAbility.GrantedAbility Card.Type.Card -> GrantedAbility.GrantedAbility Card.Type.Card
rewriteGrantedAbility pairs granted = case granted of
  GrantedAbility.Activated a -> GrantedAbility.Activated (rewriteActivatedAbility pairs a)
  GrantedAbility.Triggered t -> GrantedAbility.Triggered (rewriteTriggeredAbility pairs t)
  GrantedAbility.Static sa -> GrantedAbility.Static (rewriteStaticAbility pairs sa)
  -- Left as written: a printed rule ability is rewritten where its gatherer
  -- reads it, off its source's text changes, and no card in data/cards quotes
  -- one in a grant this walk reaches. Backup's is expanded at resolution
  -- (Parun.Engine.Resolve.Common.expandGrant) and stored, where no rewrite runs.
  GrantedAbility.Rules rules -> GrantedAbility.Rules rules
  GrantedAbility.Replacement r -> GrantedAbility.Replacement (rewritePrintedReplacement pairs r)
  GrantedAbility.Player p -> GrantedAbility.Player (rewritePlayerStaticAbility pairs p)
  GrantedAbility.SelfCostReduction r ->
    GrantedAbility.SelfCostReduction
      r
        { CostReduction.perEach = rewriteQuantity pairs (CostReduction.perEach r),
          CostReduction.condition = fmap (rewriteCondition pairs) (CostReduction.condition r),
          CostReduction.whichTargets = fmap (Filter.rewrite pairs) (CostReduction.whichTargets r)
        }
  GrantedAbility.SelfAlternativeCost a ->
    GrantedAbility.SelfAlternativeCost
      a
        { AlternativeCost.condition = fmap (rewriteCondition pairs) (AlternativeCost.condition a),
          AlternativeCost.cost = Filter.rewriteCost pairs (AlternativeCost.cost a)
        }
  GrantedAbility.SelfSpendManaAsThough clause -> GrantedAbility.SelfSpendManaAsThough (rewriteSpendManaAsThough pairs clause)

-- CR 612.1 over a TRIGGERED ability printed on a permanent. Three parts, not
-- just the payload: the CR 603.8 condition is where the word usually is, and CR
-- 603.4's intervening "if" shares rewriteCondition.
rewriteTriggeredAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteTriggeredAbility pairs ability =
  ability
    { TriggeredAbility.condition = rewriteTriggerCondition pairs (TriggeredAbility.condition ability),
      TriggeredAbility.intervening = fmap (rewriteCondition pairs) (TriggeredAbility.intervening ability),
      TriggeredAbility.modal = rewriteModal pairs (TriggeredAbility.modal ability)
    }

-- CR 612.1 over a REPLACEMENT effect printed on a permanent, which CR 604.2
-- makes a static ability's continuous effect and so text in the same text box as
-- a triggered ability's. Two carriers: the ability's own "as long as" clause, and
-- the effect itself.
rewritePrintedReplacement :: [(TextWord.TextWord, TextWord.TextWord)] -> PrintedReplacement.PrintedReplacement Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> PrintedReplacement.PrintedReplacement Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewritePrintedReplacement pairs printed =
  printed
    { PrintedReplacement.condition = fmap (rewriteCondition pairs) (PrintedReplacement.condition printed),
      PrintedReplacement.effect = rewriteReplacementEffect pairs (PrintedReplacement.effect printed)
    }

-- CR 612.1 through the replacement effect itself: the EVENT PATTERN saying which
-- objects it watches, and the REWRITE it applies to one. Exhaustive rather than a
-- wildcard, in rewriteTriggerCondition's posture -- a later arm carrying a word
-- fails to compile here instead of silently keeping the printed one.
--
-- Classification, not identity: every arm is a CR 614.1 event class, and the
-- descent is by the field shapes those classes carry.
rewriteReplacementEffect :: [(TextWord.TextWord, TextWord.TextWord)] -> ReplacementEffect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ReplacementEffect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteReplacementEffect pairs effect = case effect of
  -- CR 400.3's owner and the destination Zone name no word; the moving object's
  -- Filter and the CR 603.11 linked trigger's effects do.
  ReplacementEffect.ZoneChangeR r ->
    ReplacementEffect.ZoneChangeR
      r
        { ZoneChangeR.matching =
            (ZoneChangeR.matching r)
              { ZoneChangePattern.whatObject = Filter.rewrite pairs (ZoneChangePattern.whatObject (ZoneChangeR.matching r))
              },
          ZoneChangeR.whenYouDo = fmap (rewriteEffect pairs) (ZoneChangeR.whenYouDo r)
        }
  ReplacementEffect.EntryR r ->
    ReplacementEffect.EntryR
      r
        { EntryR.matching = Filter.rewrite pairs (EntryR.matching r),
          EntryR.rewrite = rewriteEntryRewrite pairs (EntryR.rewrite r)
        }
  -- The pattern's two Filters, CR 614.9's printed destination, and CR 615.5's
  -- riders. The pattern's DamageKind, its PlayerRelation, its Recipient and its
  -- ObjectId are a rules category, a CR 109.5 relation and two baked identities,
  -- so none of the four holds a printed word.
  ReplacementEffect.DamageR r ->
    ReplacementEffect.DamageR
      r
        { DamageR.matching = rewriteDamagePattern pairs (DamageR.matching r),
          DamageR.rewrite = rewriteDamageRewrite pairs (DamageR.rewrite r),
          DamageR.riders = fmap (rewriteEffect pairs) (DamageR.riders r)
        }
  -- CR 614.1a's printed subject; the rewrite beside it is nullary.
  ReplacementEffect.DestructionR r ->
    ReplacementEffect.DestructionR r {DestructionR.matching = fmap (Filter.rewrite pairs) (DestructionR.matching r)}
  -- CR 122.1d's untap replacement: a nullary rewrite with no pattern beside it,
  -- so there is nothing to swap.
  ReplacementEffect.UntapR _ -> effect
  -- CR 614.1a / 120.4c: a LifeLossPattern is one CR 109.5 relation and one cause,
  -- and no arm of the rewrite names a Filter or a card. No printed word, so
  -- nothing to swap.
  ReplacementEffect.LifeLossR {} -> effect
  -- CR 614.1a / 119.10: a LifeGainR is one CR 109.5 relation and one rewrite
  -- naming no Filter or card. No printed word, so nothing to swap.
  ReplacementEffect.LifeGainR {} -> effect
  -- CR 614.1a: an InstructionR is one instruction, one CR 109.5 relation and one
  -- scaling or DrawInstead; no Filter and no card. LifeGainR's answer, and for
  -- its reason.
  ReplacementEffect.InstructionR {} -> effect
  -- CR 614.1a / 706.1: a DieRollR is one CR 109.5 relation and one nullary
  -- rewrite, so CR 612.1's text change has nothing to walk into.
  ReplacementEffect.DieRollR {} -> effect
  -- The Unstable FAQ: a PerformerR's Filter names a subtype (Steamflogger Boss's
  -- Rigger), which CR 612.1's text change reaches.
  ReplacementEffect.PerformerR row -> ReplacementEffect.PerformerR row {PerformerR.performer = Filter.rewrite pairs (PerformerR.performer row)}
  -- Naked Singularity's ruling of 2004-10-04: a text change reaches the land
  -- type a ManaR's Filter names.
  ReplacementEffect.ManaR r -> ReplacementEffect.ManaR r {ManaR.matching = Filter.rewrite pairs (ManaR.matching r)}
  -- CR 104.3: the pattern is one CR 109.5 relation; the effects run instead are
  -- the resolution road's, so they are swapped here too.
  ReplacementEffect.GameLossR r -> ReplacementEffect.GameLossR r {GameLossR.instead = fmap (rewriteEffect pairs) (GameLossR.instead r)}
  ReplacementEffect.PlaneswalkR r -> ReplacementEffect.PlaneswalkR r {PlaneswalkR.rewrite = case PlaneswalkR.rewrite r of PlaneswalkRewrite.ArrangeTopTwo -> PlaneswalkRewrite.ArrangeTopTwo; PlaneswalkRewrite.RunEffects effects -> PlaneswalkRewrite.RunEffects (fmap (rewriteEffect pairs) effects)}
  -- A DrawR's pattern is one CR 109.5 relation, but the REWRITE can hold effects
  -- or CR 400.11c's wish filter, which rewriteEffect swaps inside an Effect.FromOutsideTheGame on
  -- the resolution road -- so it has to be swapped here too, or the same sentence
  -- would read one way as a spell and another as a floating row.
  ReplacementEffect.DrawR r -> ReplacementEffect.DrawR r {DrawR.rewrite = rewriteDrawRewrite pairs (DrawR.rewrite r)}
  -- A DrawCountR is one CR 109.5 relation, one count and one nullary rewrite; no
  -- Filter and no card, so no printed word to swap. DrawR's answer.
  ReplacementEffect.DrawCountR {} -> effect
  ReplacementEffect.CounterR r ->
    ReplacementEffect.CounterR
      r
        { CounterR.matching =
            (CounterR.matching r)
              { CounterPattern.whichKind = fmap (Filter.rewriteCounterKind pairs) (CounterPattern.whichKind (CounterR.matching r)),
                CounterPattern.onWhat = Filter.rewrite pairs (CounterPattern.onWhat (CounterR.matching r))
              }
        }
  -- CR 612.1 through both printed halves: the pattern's Filter over what the
  -- token is (Queen Allenal of Ruadach's "creature tokens") and the appended
  -- token's own text, rewriteCard's descent as for a Create's token.
  ReplacementEffect.TokenR r ->
    ReplacementEffect.TokenR
      r
        { TokenR.matching = (TokenR.matching r) {TokenPattern.whatToken = Filter.rewrite pairs (TokenPattern.whatToken (TokenR.matching r))},
          TokenR.plus = fmap (fmap (rewriteCard pairs)) (TokenR.plus r)
        }
  ReplacementEffect.TurnUpR r ->
    ReplacementEffect.TurnUpR
      r
        { TurnUpR.matching = Filter.rewrite pairs (TurnUpR.matching r),
          TurnUpR.rewrite = case TurnUpR.rewrite r of
            TurnUpRewrite.WithCounters w -> TurnUpRewrite.WithCounters (rewriteWithCounters pairs w)
            TurnUpRewrite.MayAttachTo f -> TurnUpRewrite.MayAttachTo (Filter.rewrite pairs f)
            TurnUpRewrite.AsCopy c -> TurnUpRewrite.AsCopy (rewriteAsCopy pairs c)
        }
  ReplacementEffect.AttachR r ->
    ReplacementEffect.AttachR
      r
        { AttachR.matching = Filter.rewrite pairs (AttachR.matching r),
          AttachR.rewrite = case AttachR.rewrite r of
            AttachRewrite.AsCopy c -> AttachRewrite.AsCopy (rewriteAsCopy pairs c)
            AttachRewrite.ChooseQuality quality -> AttachRewrite.ChooseQuality quality
        }
  -- A PhasePattern is a PhaseSelector and a baked seat (CR 614.1b / 500.11), both
  -- rules categories rather than printed words; the effects a skip runs are not.
  ReplacementEffect.PhaseR r -> ReplacementEffect.PhaseR r {PhaseR.rewrite = case PhaseR.rewrite r of PhaseRewrite.Skip -> PhaseRewrite.Skip; PhaseRewrite.MaySkipFor effects -> PhaseRewrite.MaySkipFor (fmap (rewriteEffect pairs) effects)}

-- CR 612.1 through a damage REWRITE, rewriteEntryRewrite's twin one event class
-- over. CR 614.9's printed destination holds a Filter for a word to sit in,
-- the nested effects hold whatever an ability's clauses do, and CR 702.64a's
-- ceiling is a Quantity whose count may hold one; the rest are numbers, a
-- Scaling and a baked Recipient.
--
-- NO BOARD OBSERVES IT: the pool's one printed destination is
-- Filter.IsHostOfSource (Pariah), which names no subtype for CR 612.1 to swap,
-- the pool's nested programs (Kill-Suit Cultist, Synthetic Proxy Strike)
-- destroy a slot, which names none either, and the pool's one counted ceiling
-- (Shield of the Avatar) counts creatures, a card type -- so mutating any of
-- the three arms away leaves the suite green. All are the rule rather than a
-- proven behaviour: a card redirecting to "the enchanted Goblin" would prove
-- the first. Exhaustive rather than a wildcard, rewriteReplacementEffect's
-- posture.
rewriteDamageRewrite :: [(TextWord.TextWord, TextWord.TextWord)] -> DamageRewrite.DamageRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> DamageRewrite.DamageRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteDamageRewrite pairs rewrite = case rewrite of
  DamageRewrite.RedirectMatching f -> DamageRewrite.RedirectMatching (Filter.rewrite pairs f)
  DamageRewrite.Redirect _ -> rewrite
  DamageRewrite.RedirectNext _ _ -> rewrite
  DamageRewrite.PreventAll -> rewrite
  DamageRewrite.PreventRemovingShieldCounter -> rewrite
  DamageRewrite.PreventRemovingCounters _ -> rewrite
  DamageRewrite.PreventNext _ -> rewrite
  DamageRewrite.PreventAllBut _ -> rewrite
  DamageRewrite.PreventUpTo quantity -> DamageRewrite.PreventUpTo (rewriteQuantity pairs quantity)
  DamageRewrite.SetAmount _ -> rewrite
  DamageRewrite.Scale _ -> rewrite
  -- CR 614.1a's "instead [do something]", the payload shared with an ability's
  -- clauses -- EntryRewrite.RunEffects' arm exactly.
  DamageRewrite.RunEffects es -> DamageRewrite.RunEffects (fmap (rewriteEffect pairs) es)
  DamageRewrite.TurnFaceUp -> rewrite
  DamageRewrite.AlsoToSourceController -> rewrite
  DamageRewrite.HealSourceFirst -> rewrite

-- CR 612.1 through what a CR 614.1a draw replacement does. The wish filter is the
-- one printed word a draw rewrite can hold, and it is the SAME Filter
-- rewriteEffect swaps inside an Effect.FromOutsideTheGame, so the two roads into the
-- game agree about what a text change did (CR 400.11c).
--
-- NO BOARD OBSERVES IT, rewriteDamageRewrite's position above: the pool's
-- draw-replacing wishes are Ring of Ma'rûf's @And []@ and Synthetic Wishful
-- Djinn's comparison against the source's power, which name no subtype for
-- CR 612.1 to swap, so neutralising the Filter.rewrite here leaves the suite
-- green. The arm is the rule rather than a proven behaviour -- a card replacing
-- a draw with "a Goblin card you own from outside the game", under a
-- text-changing effect, would be what proves it. Exhaustive rather than a
-- wildcard, rewriteReplacementEffect's posture.
rewriteDrawRewrite :: [(TextWord.TextWord, TextWord.TextWord)] -> DrawRewrite.DrawRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> DrawRewrite.DrawRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteDrawRewrite pairs rewrite = case rewrite of
  DrawRewrite.GainLife _ -> rewrite
  DrawRewrite.FromOutsideTheGame payload -> DrawRewrite.FromOutsideTheGame payload {FromOutsideTheGame.filter = Filter.rewrite pairs (FromOutsideTheGame.filter payload)}
  -- CR 702.52a's dredge carries a COUNT and nothing CR 612.1 can swap.
  DrawRewrite.Dredge _ -> rewrite
  DrawRewrite.YouDraw -> rewrite
  DrawRewrite.OthersMayPayLife _ -> rewrite
  DrawRewrite.FromGraveyardOrLose -> rewrite
  DrawRewrite.RevealUntilChosenKind -> rewrite
  DrawRewrite.DrawInstead _ -> rewrite
  DrawRewrite.DiscardThenDrawOrMill -> rewrite
  DrawRewrite.KeepOneOfTopThenDraw quantity -> DrawRewrite.KeepOneOfTopThenDraw (rewriteQuantity pairs quantity)
  DrawRewrite.MaySkipFor effects -> DrawRewrite.MaySkipFor (fmap (rewriteEffect pairs) effects)

-- CR 612.1 through CR 707.9's "except ..." clause. Exhaustive for
-- rewriteReplacementEffect's reason.
--
-- NO BOARD OBSERVES IT, the ChoiceByCoinFlip arm's position below: what changes
-- is a keyword that CARRIES a word, a quoted ability, or CR 707.9b's subtype
-- clause, and no card in data/cards puts a text-changing effect on any of them --
-- no producer of the CR 707.9a arms names a word (Dack's Duplicate grants haste
-- and dethrone, Omni-Changeling changeling, Mercurial Pretender a bounce), and
-- Wall of Stolen Identity's "a Wall" would need an Artificial Evolution aimed at
-- it. The arms are the rule rather than a proven
-- behaviour -- an "except it has islandwalk", or that Artificial Evolution, would
-- be what proves them. CR 707.9b's remaining arms name no word a text
-- changer's family holds (Parun.Types.WordFamily): the pair is two literals,
-- the type clause names CR
-- 205.2a's card types, the supertype clauses CR 205.4a's supertypes, and the name
-- clause a name, which CR 612.2's last sentence puts out of reach outright.
rewriteCopyException :: [(TextWord.TextWord, TextWord.TextWord)] -> CopyException.CopyException (GrantedAbility.GrantedAbility Card.Type.Card) -> CopyException.CopyException (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteCopyException pairs exception = case exception of
  CopyException.SetPowerToughness _ -> exception
  CopyException.GainKeywords keywords -> CopyException.GainKeywords (Set.map (Filter.rewriteKeyword pairs) keywords)
  -- CR 707.9b's type clause names CARD types (CR 205.2a's list), and no text
  -- changer's family holds one (Parun.Types.WordFamily), so there is nothing here
  -- for a pair to change.
  CopyException.CardTypes {} -> exception
  -- CR 707.9b's subtype clause DOES name a word CR 612.2's swap reaches -- "a
  -- Wall in addition to its other types" is one of the "all instances of one
  -- creature type" Artificial Evolution changes -- so the pairs apply, exactly as
  -- they do to a Filter's HasSubtype atom (Filter.rewrite).
  CopyException.AddSubtypes subtypes -> CopyException.AddSubtypes (Set.map (TextWord.Engine.subtypeAfter pairs) subtypes)
  -- The same swap over the replacing clause's subtypes (Dino DNA's "Dinosaur").
  -- A REGRESSION FENCE: no test changes Dino DNA's text.
  CopyException.SetCreatureSubtypes subtypes -> CopyException.SetCreatureSubtypes (Set.map (TextWord.Engine.subtypeAfter pairs) subtypes)
  -- CR 205.4a's supertypes for the same reason CR 707.9b's card types are left
  -- alone, whichever direction the clause runs: no text changer's family holds a
  -- supertype (Parun.Types.WordFamily).
  CopyException.AddSupertypes _ -> exception
  CopyException.RemoveSupertypes _ -> exception
  -- CR 201.1's name is the one CR 612.2 rules out in so many words: "an effect
  -- that changes a color word or a subtype can't change a card name". The
  -- effects that do are CR 612.7-612.9's, in layer 3 (Modification's
  -- AddNamesMatching, SetName and InsertNameWords), rather than a rewrite of
  -- this clause.
  CopyException.SetName _ -> exception
  -- CR 612.2's colour word used as a colour word ("except it's white"). A
  -- REGRESSION FENCE: no test changes a colour exception's text. A missing mana
  -- cost carries no word.
  CopyException.SetColors colors -> CopyException.SetColors (Set.map (TextWord.Engine.colorAfter pairs) colors)
  CopyException.NoManaCost -> exception
  -- CR 707.9c's clause names a characteristic and no word, so CR 612.2's subtype
  -- swap has nothing to reach.
  CopyException.DontCopyColors -> exception
  -- CR 707.9a's "this ability" carries no word of its own. The ability it points
  -- at is the resolving one, which the projection already rewrote where it was
  -- read (rewriteTriggeredAbility).
  CopyException.GainThisAbility -> exception
  -- A quoted ability is card text like any other, so its subtype words move.
  CopyException.GainAbility granted -> CopyException.GainAbility (rewriteGrantedAbility pairs granted)

-- CR 612.1 through a copy rewrite, on either event class it rides (CR 614.1c,
-- 614.1e). The "which permanents" clause names a word, and so does CR 707.9a's
-- gained ability -- a keyword, and a keyword may carry one
-- (rewriteCopyException). CR 707.9e's additional counters take
-- rewriteWithCounters' descent, the same one the CR 614.1c row gets: the amount
-- is a Quantity and the kind may be a whole keyword. A REGRESSION FENCE, the
-- Create arm's reason: Altered Ego's amount is CR 601.2b's X and its kind is
-- +1/+1, neither of which holds a word, so neutralizing this half leaves the
-- suite green. The `tapped` flag and `zone` are passed through and hold no word
-- (Vesuva). CR 707.9g's linked trigger takes rewriteEffect's descent, the
-- duration rewriteDuration's, and the clauses rewriteCopyClause's.
rewriteAsCopy :: [(TextWord.TextWord, TextWord.TextWord)] -> AsCopy.AsCopy (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> AsCopy.AsCopy (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteAsCopy pairs c =
  c
    { AsCopy.eligible = Filter.rewrite pairs (AsCopy.eligible c),
      AsCopy.clauses = fmap (rewriteCopyClause pairs) (AsCopy.clauses c),
      AsCopy.whenYouDo = fmap (rewriteEffect pairs) (AsCopy.whenYouDo c),
      AsCopy.duration = fmap (rewriteDuration pairs) (AsCopy.duration c)
    }

-- CR 612.1 through one clause of a copy exception: its CR 707.9f condition,
-- its exceptions (rewriteCopyException) and its CR 707.9e counters
-- (rewriteWithCounters).
rewriteCopyClause :: [(TextWord.TextWord, TextWord.TextWord)] -> CopyClause.CopyClause (GrantedAbility.GrantedAbility Card.Type.Card) -> CopyClause.CopyClause (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteCopyClause pairs (CopyClause.MkCopyClause condition exceptions counters) =
  CopyClause.MkCopyClause (fmap (rewriteCondition pairs) condition) (fmap (rewriteCopyException pairs) exceptions) (fmap (rewriteWithCounters pairs) counters)

-- CR 612.1 through one CR 208.2b option's printed words: its keywords' and its
-- subtypes'.
rewriteEntryOption :: [(TextWord.TextWord, TextWord.TextWord)] -> EntryOption.EntryOption -> EntryOption.EntryOption
rewriteEntryOption pairs o =
  o
    { EntryOption.keywords = Set.map (Filter.rewriteKeyword pairs) (EntryOption.keywords o),
      EntryOption.subtypes = Set.map (TextWord.Engine.subtypeAfter pairs) (EntryOption.subtypes o)
    }

-- CR 612.1 through what a CR 614.1c/614.1d entry replacement does. Exhaustive for
-- rewriteReplacementEffect's reason.
rewriteEntryRewrite :: [(TextWord.TextWord, TextWord.TextWord)] -> EntryRewrite.EntryRewrite (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> EntryRewrite.EntryRewrite (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteEntryRewrite pairs rewrite = case rewrite of
  EntryRewrite.AsCopy c -> EntryRewrite.AsCopy (rewriteAsCopy pairs c)
  -- CR 702.14a's word again, this time inside a keyword an option grants, and
  -- CR 205.3's inside a subtype one adds.
  EntryRewrite.ChoiceOf os -> EntryRewrite.ChoiceOf (fmap (rewriteEntryOption pairs) os)
  -- The same word again, in an option a coin picks rather than a player. NO
  -- BOARD OBSERVES IT, and cannot: rewriteKeyword only ever changes a keyword
  -- that CARRIES a word (landwalk, typecycling, hexproof from, protection from),
  -- and Molten Sentry's two options grant haste and defender, which carry none.
  -- The arm is the rule (CR 612.1) rather than a proven behaviour -- an option
  -- granting islandwalk would be what proves it.
  EntryRewrite.ChoiceByCoinFlip f ->
    EntryRewrite.ChoiceByCoinFlip f {EntryFlip.heads = rewriteEntryOption pairs (EntryFlip.heads f), EntryFlip.tails = rewriteEntryOption pairs (EntryFlip.tails f)}
  -- CR 105.1's five colours, CR 305.6's five basic land types, CR 205.3m's
  -- creature types and CR 102.1's seats are the offers themselves, so none of
  -- these four prints a word the card chose.
  EntryRewrite.ChooseQuality _ -> rewrite
  EntryRewrite.ChoosePlayer _ -> rewrite
  -- CR 702.155b's offer is a range of NUMBERS, read off the entering Saga's own
  -- chapter symbols (CR 715.2d), so the rewrite prints no word either.
  EntryRewrite.ReadAhead -> rewrite
  -- CR 201.4a's restriction on which names may be named.
  EntryRewrite.ChooseCardNames f -> EntryRewrite.ChooseCardNames (Filter.rewrite pairs f)
  EntryRewrite.ChooseCardName f -> EntryRewrite.ChooseCardName (Filter.rewrite pairs f)
  EntryRewrite.WithCounters w -> EntryRewrite.WithCounters (rewriteWithCounters pairs w)
  -- EVERY part of the sentence: the counters take the arm above's descent and
  -- the keywords take CR 702.14a's word once more, this time in a keyword the
  -- entry clause grants outright. The keyword half is latent for
  -- ChoiceByCoinFlip's reason: Faerie Squadron grants flying, which carries no
  -- word, and an entry clause granting islandwalk would be what proves it. A
  -- quoted ability takes CopyException.GainAbility's descent, latent the same
  -- way: Degavolver's quoted ability prints no subtype.
  --
  -- Written out rather than as a record update, so a field added to the payload
  -- is a missing-field error here rather than a value the text change silently
  -- keeps.
  EntryRewrite.EntersWith e ->
    EntryRewrite.EntersWith
      EntersWith.MkEntersWith
        { EntersWith.counters = fmap (rewriteWithCounters pairs) (EntersWith.counters e),
          EntersWith.keywords = Set.map (Filter.rewriteKeyword pairs) (EntersWith.keywords e),
          EntersWith.abilities = fmap (rewriteGrantedAbility pairs) (EntersWith.abilities e)
        }
  EntryRewrite.UnderControlOf _ -> rewrite
  EntryRewrite.SacrificeAnyNumber s ->
    EntryRewrite.SacrificeAnyNumber
      s
        { SacrificeAnyNumber.each = rewriteQuantity pairs (SacrificeAnyNumber.each s),
          SacrificeAnyNumber.filter = Filter.rewrite pairs (SacrificeAnyNumber.filter s),
          SacrificeAnyNumber.kind = fmap (Filter.rewriteCounterKind pairs) (SacrificeAnyNumber.kind s)
        }
  -- CR 614.1c's "exile a [matching] card from your graveyard": Living Lore's says
  -- instant or sorcery, card types CR 612.1 reaches. That EntryR matches
  -- Filter.IsSource, so a text change reaches this row only on a spell, carried
  -- over by CR 400.7a before the entry loop (Parun.ReplacementSpec's Tidewalker
  -- case).
  EntryRewrite.ExileFromGraveyard exile -> EntryRewrite.ExileFromGraveyard exile {ExileFromGraveyard.filter = Filter.rewrite pairs (ExileFromGraveyard.filter exile), ExileFromGraveyard.count = rewriteHowMany pairs (ExileFromGraveyard.count exile), ExileFromGraveyard.kinds = fmap (Filter.rewriteCounterKind pairs) (ExileFromGraveyard.kinds exile), ExileFromGraveyard.each = rewriteQuantity pairs (ExileFromGraveyard.each exile)}
  -- CR 614.1a's "sacrifice a Forest instead": Heart of Yavimaya's names a land
  -- type CR 612.1 reaches. Latent: that EntryR matches Filter.IsSource, and a
  -- land is never a spell, so CR 400.7a carries no text change onto it before it
  -- enters.
  EntryRewrite.SacrificeToEnter s -> EntryRewrite.SacrificeToEnter s {SacrificeToEnter.filter = Filter.rewrite pairs (SacrificeToEnter.filter s)}
  -- CR 614.1c's "a creature you control it could be attached to": Grifter's
  -- Blade's names a card type CR 612.1 reaches. That EntryR matches
  -- Filter.IsSource, so a text change reaches this row only on a spell, carried
  -- over by CR 400.7a, as with Living Lore above.
  EntryRewrite.EntersAttachedTo f -> EntryRewrite.EntersAttachedTo (Filter.rewrite pairs f)
  -- CR 702.38a carries no Filter and no subtype of its own, so there is nothing
  -- here for CR 612.1 to change. A text change to the entering object's own
  -- creature types still reaches the offer, which Parun.Engine.Event reads off
  -- that object's CR 613 projection as the row applies.
  EntryRewrite.Amplify _ -> rewrite
  -- Rules 702.136a, 702.98a and 702.54a state these three whole, bloodthirst's
  -- number included, so the card prints no word for CR 612.1 to reach.
  EntryRewrite.Riot -> rewrite
  EntryRewrite.Unleash -> rewrite
  EntryRewrite.Bloodthirst _ -> rewrite
  EntryRewrite.Tribute _ -> rewrite
  -- CR 702.44a states sunburst whole as well, and its count is the entering
  -- object's CR 400.7d mana record rather than card text, so there is no word
  -- here for CR 612.1 to reach either.
  EntryRewrite.Sunburst -> rewrite
  EntryRewrite.ModularSunburst -> rewrite
  -- CR 702.150a states compleated whole, the symbol count being CR 118.13a's
  -- announced payment rather than card text, so there is no word here for CR
  -- 612.1 either.
  EntryRewrite.Compleated _ -> rewrite
  -- CR 614.1d's bare "enters tapped": a tap status.
  EntryRewrite.Tapped -> rewrite
  EntryRewrite.Prepared -> rewrite
  -- CR 614.1c's life price is a number. Its "reveal a [matching] card": Rustic
  -- Clachan's says Kithkin, a creature type word CR 612.2 licenses. Latent all
  -- the same: that EntryR matches Filter.IsSource, and a land is never a spell,
  -- so CR 400.7a carries no text change onto it before it enters.
  EntryRewrite.OrTapped (EntryPrice.PayLife _) -> rewrite
  EntryRewrite.OrTapped (EntryPrice.Reveal f) -> EntryRewrite.OrTapped (EntryPrice.Reveal (Filter.rewrite pairs f))
  EntryRewrite.OrTapped (EntryPrice.Behold f) -> EntryRewrite.OrTapped (EntryPrice.Behold (Filter.rewrite pairs f))
  EntryRewrite.EntersTransformed -> rewrite
  -- CR 614.1c's "as this enters, [do something]", the payload shared with an
  -- ability's clauses.
  EntryRewrite.RunEffects es -> EntryRewrite.RunEffects (fmap (rewriteEffect pairs) es)

-- CR 122.1b's keyword counter is the one counter kind holding a word. The amount
-- holds a second, on the same axis Effect.PutCounters' quantity does: CR 614.1c
-- admits "a number of +1/+1 counters equal to the number of creature cards in all
-- graveyards" (Undergrowth Scavenger), and a Count inside it names a card type or
-- a subtype CR 612.2 licenses swapping.
--
-- EVERY KIND on the row, since the row carries a map of them, see #2314.
-- 'Map.mapKeysWith' and not 'Map.mapKeys': the swap can map two kinds onto one --
-- a hexproof from Islands counter and a hexproof from Swamps counter, hacked
-- Island -> Swamp -- and 'Map.mapKeys' would keep an arbitrary survivor, losing a
-- row. CR 122.1's last sentence is why merging is what the rules ask for rather
-- than a convenience: counters with the same name are interchangeable, so the two
-- rows are one tally of that one kind. Projection's
-- Modification.ChangeWord arm above guards the same hazard the same way,
-- over a permanent's projected keywords instead of an entry row.
--
-- The combiner is Arithmetic.Plus, CR 208.2's composition: it needs no Num instance
-- (Parun.Types.Quantity has none, deliberately), and it leaves the two amounts
-- unevaluated exactly as the rewrite found them, so a Count that must be read
-- against a board is still read against the board the entry happens on.
-- 'Map.mapKeysWith' passes the value at the GREATER of the two original keys
-- first, so the map's own ascending order is what Plus's left and right hold --
-- addition commutes (Parun.Types.Plus), so this reads true rather than computing
-- anything different.
rewriteWithCounters :: [(TextWord.TextWord, TextWord.TextWord)] -> WithCounters.WithCounters -> WithCounters.WithCounters
rewriteWithCounters pairs w =
  WithCounters.MkWithCounters
    . Map.mapKeysWith (\greater lesser -> Quantity.Type.Arithmetic (Arithmetic.Plus (Plus.MkPlus lesser greater))) (Filter.rewriteCounterKind pairs)
    . fmap (rewriteQuantity pairs)
    $ WithCounters.counters w

-- The modal payload both abilities carry. Both halves of a mode: its clauses'
-- effects, and its TARGET SLOTS, whose Filter is the candidate set CR 601.2c
-- (imported by CR 602.2b) reads. The Pool is not an omission: it names a rules
-- category (CR 115) rather than a word printed on the card.
rewriteModal :: [(TextWord.TextWord, TextWord.TextWord)] -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteModal pairs modal =
  let rewriteClause c =
        c
          { Clause.effects = fmap (rewriteEffect pairs) (Clause.effects c),
            Clause.payGate = fmap (rewritePayGate pairs) (Clause.payGate c),
            Clause.condition = fmap (rewriteCondition pairs) (Clause.condition c)
          }
      rewriteMode m =
        m
          { Mode.clauses = fmap rewriteClause (Mode.clauses m),
            Mode.targetSlots = fmap (rewriteTargetSlot pairs) (Mode.targetSlots m)
          }
   in modal {Modal.Type.modes = fmap rewriteMode (Modal.Type.modes modal)}

-- CR 612.1 through the cost a clause offers as it resolves (CR 118.12).
-- Lithophage's "sacrifice this creature unless you sacrifice a Mountain" is the
-- producer: Magical Hack swaps the land type word, and CR 612.2 licenses it
-- because the word is used as a land type.
--
-- Written out field by field rather than as a record update, rewriteComponent's
-- posture one level up: `cost` and `perEach` are the fields a printed word can
-- reach -- `payer` names a player, `branch` and `obligation` name rules
-- categories, and `offeredAt` is CR 608.2e's ordinal -- so a later field carrying
-- one must fail to compile here instead of silently keeping the printed word.
--
-- `perEach` takes rewriteQuantity, rewriteEffect's own descent through a counted
-- amount: a gate scaled by "for each Elf you control" counts what a Magical Hack
-- made an Elf.
--
-- `basis` is kept: CR 118.6's described cost is a SLOT and an amount of mana
-- (Parun.Types.CostBasis), and rule 612.1 swaps neither -- the mana cost it
-- describes is read off the projection of the named object as the gate is
-- offered, so a word swap there lands through that object's own rewrite.
rewritePayGate :: [(TextWord.TextWord, TextWord.TextWord)] -> PayGate.PayGate -> PayGate.PayGate
rewritePayGate pairs gate =
  PayGate.MkPayGate
    { PayGate.payer = PayGate.payer gate,
      PayGate.cost = CostChoice.MkCostChoice (fmap (Filter.rewriteCost pairs) (CostChoice.unwrap (PayGate.cost gate))),
      PayGate.basis = PayGate.basis gate,
      PayGate.branch = PayGate.branch gate,
      PayGate.obligation = PayGate.obligation gate,
      PayGate.perEach = fmap (rewriteQuantity pairs) (PayGate.perEach gate),
      PayGate.x = fmap (rewriteQuantity pairs) (PayGate.x gate),
      PayGate.offeredAt = PayGate.offeredAt gate
    }

-- A single target slot under CR 612.1. Top-level because Parun.Engine.Resolve
-- needs the same rewrite over a resolving spell's slots (CR 608.2b).
--
-- The slot's `amount` is descended into for the filter's reason: CR 202.3's
-- computed bound is a Quantity, and a Quantity reaches a Count's Filter, so a
-- word swap finds text there exactly as it finds it at the top level. A
-- REGRESSION FENCE rather than a proved behaviour, bakeSlot's posture one module
-- over: no committed bound reaches a Filter -- the pool's bounds name either
-- Quantity.LifeChanged or Quantity.InSlot, and neither arm carries one --
-- so no board today tells the two readings apart. The slot's computed COUNT is
-- a Quantity on the same footing and is descended into beside it.
rewriteTargetSlot :: [(TextWord.TextWord, TextWord.TextWord)] -> TargetSlot.TargetSlot -> TargetSlot.TargetSlot
rewriteTargetSlot pairs slot =
  slot
    { TargetSlot.filter = fmap (Filter.rewrite pairs) (TargetSlot.filter slot),
      TargetSlot.asCast = fmap (Filter.rewrite pairs) (TargetSlot.asCast slot),
      TargetSlot.amount = fmap (rewriteQuantity pairs) (TargetSlot.amount slot),
      TargetSlot.count = SlotCount.mapQuantity (rewriteQuantity pairs) (TargetSlot.count slot),
      TargetSlot.divided = fmap (rewriteQuantity pairs) (TargetSlot.divided slot)
    }

-- CR 612.1 over a counter trigger's holder: the Filter, where there is one.
rewriteCounterHolder :: [(TextWord.TextWord, TextWord.TextWord)] -> CounterHolder.CounterHolder -> CounterHolder.CounterHolder
rewriteCounterHolder pairs holder = case holder of
  CounterHolder.Self -> holder
  CounterHolder.Permanent f -> CounterHolder.Permanent (Filter.rewrite pairs f)

-- CR 612.1 through a trigger's own condition. Exhaustive rather than a wildcard,
-- so a later condition carrying a Filter fails to compile here instead of
-- silently keeping the printed word.
rewriteTriggerCondition :: [(TextWord.TextWord, TextWord.TextWord)] -> TriggerCondition.TriggerCondition -> TriggerCondition.TriggerCondition
rewriteTriggerCondition pairs condition = case condition of
  TriggerCondition.StateIs c -> TriggerCondition.StateIs (rewriteCondition pairs c)
  -- Positional, so a later field fails to compile here rather than riding a
  -- record update unrewritten.
  TriggerCondition.Enters (EntersTrigger.MkEntersTrigger permanent batching) -> TriggerCondition.Enters (EntersTrigger.MkEntersTrigger (acting permanent) batching)
  TriggerCondition.LeavesZone (LeavesZoneTrigger.MkLeavesZoneTrigger card from to whose scope batching) -> TriggerCondition.LeavesZone (LeavesZoneTrigger.MkLeavesZoneTrigger (acting card) from to whose scope batching)
  TriggerCondition.PutIntoZone (PutIntoZoneTrigger.MkPutIntoZoneTrigger card from to scope duringResolution batching) -> TriggerCondition.PutIntoZone (PutIntoZoneTrigger.MkPutIntoZoneTrigger (acting card) from to scope duringResolution batching)
  -- The holder's Filter is rewritten and the counter kind is not: CR 612.1's
  -- pairs swap SUBTYPE words, and a counter kind names none.
  TriggerCondition.CountersChange (CounterTrigger.MkCounterTrigger kind motion holder threshold grouping) -> TriggerCondition.CountersChange (CounterTrigger.MkCounterTrigger kind motion (rewriteCounterHolder pairs holder) threshold grouping)
  -- The TurnScope is carried through untouched, not dropped: a rebuild that
  -- forgot the field would reset a text-changed trigger to firing every turn.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast subject scope fromZone ordinal window copies) -> TriggerCondition.SpellCast (SpellCast.MkSpellCast (acting subject) scope fromZone ordinal window copies)
  TriggerCondition.StepBegins {} -> condition
  -- Positional, so a field the record gains stops the build here: a kind and a
  -- card type name no word for Parun.Types.ChangeText's pairs to swap.
  TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger actor kind recipients) -> TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger (acting actor) kind recipients)
  TriggerCondition.IsDealtDamage watched -> TriggerCondition.IsDealtDamage watched {PermanentIsDealtDamage.dealt = (PermanentIsDealtDamage.dealt watched) {DealtDamage.recipient = acting (DealtDamage.recipient (PermanentIsDealtDamage.dealt watched))}}
  TriggerCondition.PlayerIsDealtDamage _ -> condition
  TriggerCondition.PermanentsDealCombatDamageToPlayer p -> TriggerCondition.PermanentsDealCombatDamageToPlayer p {PermanentsDealCombatDamageToPlayer.filter = Filter.rewrite pairs (PermanentsDealCombatDamageToPlayer.filter p)}
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> condition
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> condition
  TriggerCondition.PlayerTookInitiative -> condition
  TriggerCondition.SelfRevealedForMiracle -> condition
  TriggerCondition.RevealedThisWay r -> TriggerCondition.RevealedThisWay r {RevealedThisWay.card = Filter.rewrite pairs (RevealedThisWay.card r)}
  TriggerCondition.Discards (DiscardTrigger.MkDiscardTrigger subject way batching) -> case subject of
    DiscardSubject.Self -> condition
    DiscardSubject.Discarder payload -> TriggerCondition.Discards (DiscardTrigger.MkDiscardTrigger (DiscardSubject.Discarder payload {PlayerActsOn.filter = Filter.rewrite pairs (PlayerActsOn.filter payload)}) way batching)
  TriggerCondition.AbilityActivated t -> TriggerCondition.AbilityActivated t {AbilityActivatedTrigger.filter = Filter.rewrite pairs (AbilityActivatedTrigger.filter t)}
  TriggerCondition.BecomesTargeted c ->
    TriggerCondition.BecomesTargeted
      c
        { BecomesTargetedTrigger.subject = case BecomesTargetedTrigger.subject c of
            TargetedSubject.Permanent p -> TargetedSubject.Permanent (acting p)
            TargetedSubject.You -> TargetedSubject.You
            TargetedSubject.YouOrPermanent p -> TargetedSubject.YouOrPermanent (acting p)
        }
  TriggerCondition.PlayerDrawsNthCard {} -> condition
  TriggerCondition.PlayerBecomesMonarch _ -> condition
  TriggerCondition.Attacks t -> TriggerCondition.Attacks (rewriteAttackTrigger pairs t)
  TriggerCondition.Blocks t -> TriggerCondition.Blocks (rewriteBlockTrigger pairs t)
  -- Positional, so a later field fails to compile here rather than riding a
  -- record update unrewritten. A slot name is card data but not card TEXT, so
  -- no CR 612.1 swap reaches it.
  TriggerCondition.Leaves (LeavesTrigger.MkLeavesTrigger permanent destinations batching) ->
    TriggerCondition.Leaves
      ( LeavesTrigger.MkLeavesTrigger
          ( case permanent of
              LeavingPermanent.Matching f -> LeavingPermanent.Matching (Filter.rewrite pairs f)
              LeavingPermanent.Self -> permanent
              LeavingPermanent.Attached -> permanent
              LeavingPermanent.Haunted -> permanent
              LeavingPermanent.Bound _ -> permanent
          )
          destinations
          batching
      )
  TriggerCondition.Countered _ -> condition
  TriggerCondition.DamageToPlayerPrevented _ -> condition
  TriggerCondition.SelfPreventsDamage f -> TriggerCondition.SelfPreventsDamage (Filter.rewrite pairs f)
  TriggerCondition.LifeChanges _ -> condition
  TriggerCondition.SelfBecomesClassLevel _ -> condition
  TriggerCondition.SelfHalfUnlocked _ -> condition
  TriggerCondition.RoomFullyUnlocked _ -> condition
  TriggerCondition.AnyOf conditions -> TriggerCondition.AnyOf (fmap (rewriteTriggerCondition pairs) conditions)
  TriggerCondition.CausedBy narrowed -> TriggerCondition.CausedBy narrowed {CausedBy.condition = rewriteTriggerCondition pairs (CausedBy.condition narrowed)}
  TriggerCondition.TurnedFaceUp permanent -> TriggerCondition.TurnedFaceUp (acting permanent)
  -- CR 701.27f's face is a NAME, which CR 612.2's second sentence keeps a swap
  -- off, so only the subject is rewritten. SelfHalfUnlocked's answer for its own
  -- door.
  TriggerCondition.Transforms (TransformTrigger.MkTransformTrigger permanent into) -> TriggerCondition.Transforms (TransformTrigger.MkTransformTrigger (acting permanent) into)
  TriggerCondition.FaceDownPermanentLeavesRevealed -> condition
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d f) -> TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d (Filter.rewrite pairs f))
  TriggerCondition.PermanentActs acts ->
    TriggerCondition.PermanentActs
      acts {PermanentActed.permanent = acting (PermanentActed.permanent acts)}
  TriggerCondition.AttachedCreatureMentors -> condition
  TriggerCondition.BecomesTapState watched -> TriggerCondition.BecomesTapState watched {BecomesTapState.actor = acting (BecomesTapState.actor watched)}
  TriggerCondition.TappedForMana payload ->
    TriggerCondition.TappedForMana
      payload
        { TappedForManaTrigger.permanent = case TappedForManaTrigger.permanent payload of
            TappedPermanent.Attached -> TappedPermanent.Attached
            TappedPermanent.Matching f -> TappedPermanent.Matching (Filter.rewrite pairs f),
          TappedForManaTrigger.mana = rewriteManaSpecification pairs (TappedForManaTrigger.mana payload)
        }
  TriggerCondition.AbilityAddsMana payload -> TriggerCondition.AbilityAddsMana payload {AbilityAddsMana.source = Filter.rewrite pairs (AbilityAddsMana.source payload), AbilityAddsMana.mana = rewriteManaSpecification pairs (AbilityAddsMana.mana payload)}
  TriggerCondition.SelfManaAbilityResolves -> condition
  TriggerCondition.Exploits (ExploitTrigger.MkExploitTrigger exploiter exploited) -> TriggerCondition.Exploits (ExploitTrigger.MkExploitTrigger (acting exploiter) (Filter.rewrite pairs exploited))
  TriggerCondition.Crews _ -> condition
  TriggerCondition.PermanentSacrificed payload -> TriggerCondition.PermanentSacrificed payload {PlayerActsOn.filter = Filter.rewrite pairs (PlayerActsOn.filter payload)}
  TriggerCondition.SagaFinalChapterTriggers _ -> condition
  -- CR 603.7's slot name is card data but not card TEXT, so no CR 612.1 swap
  -- reaches it; what the slot holds is read off the projection instead.
  TriggerCondition.LosesControl _ -> condition
  TriggerCondition.RoomEntered _ -> condition
  TriggerCondition.PlayerActs _ -> condition
  TriggerCondition.PlayerLosesGame _ -> condition
  TriggerCondition.PlacesSticker p -> TriggerCondition.PlacesSticker p {PlacesSticker.object = Filter.rewrite pairs (PlacesSticker.object p)}
  TriggerCondition.PlayerPlaysLand payload -> TriggerCondition.PlayerPlaysLand payload {PlaysLand.filter = Filter.rewrite pairs (PlaysLand.filter payload)}
  TriggerCondition.PlayerManifestsDread _ -> condition
  TriggerCondition.PlayerFacesDilemma _ -> condition
  TriggerCondition.PlayerRollsResult _ -> condition
  TriggerCondition.Visit -> condition
  TriggerCondition.ChaosEnsues -> condition
  TriggerCondition.PlayerRollsPlaneswalker _ -> condition
  TriggerCondition.SelfTurnedUpOffDeck -> condition
  TriggerCondition.SelfPlaneswalkedAwayFrom -> condition
  TriggerCondition.FlipsCoin _ -> condition
  TriggerCondition.SelfBecomesPlotted -> condition
  TriggerCondition.AttachmentChanges t -> TriggerCondition.AttachmentChanges t {AttachmentTrigger.other = Filter.rewrite pairs (AttachmentTrigger.other t)}
  -- CR 603.12's reflexive carries nothing at all, so there is no subtype to swap.
  TriggerCondition.Reflexive -> condition
  where
    acting watched = case watched of
      ActingPermanent.Self -> ActingPermanent.Self
      ActingPermanent.Matching f -> ActingPermanent.Matching (Filter.rewrite pairs f)

-- CR 612.1 through both ends of a pairwise block trigger.
-- CR 612.1 through an attack trigger's Filters and condition, each of which
-- can name a creature type (CR 508.3c's "attacks with one or more Birds").
-- Relations name none.
rewriteAttackTrigger :: [(TextWord.TextWord, TextWord.TextWord)] -> AttackTrigger.AttackTrigger -> AttackTrigger.AttackTrigger
rewriteAttackTrigger pairs trigger = case trigger of
  AttackTrigger.Attacker (AttackerTrigger.MkAttackerTrigger creature target company frequency while_) ->
    AttackTrigger.Attacker
      ( AttackerTrigger.MkAttackerTrigger
          (Filter.rewrite pairs creature)
          ( case target of
              AttackerTarget.AnyTarget -> target
              AttackerTarget.Defending _ -> target
              AttackerTarget.Permanent f -> AttackerTarget.Permanent (Filter.rewrite pairs f)
              AttackerTarget.PlayerWithMostLife -> target
          )
          ( case company of
              AttackCompany.AnyCompany -> company
              AttackCompany.Alone {} -> company
              AttackCompany.WithAnother f -> AttackCompany.WithAnother (Filter.rewrite pairs f)
          )
          frequency
          (fmap (rewriteCondition pairs) while_)
      )
  AttackTrigger.Attacked _ -> trigger
  AttackTrigger.Declaration (DeclarationTrigger.MkDeclarationTrigger player attackers frequency while_) ->
    AttackTrigger.Declaration
      ( DeclarationTrigger.MkDeclarationTrigger
          player
          (fmap (\(AttackCount.MkAttackCount measure n f) -> AttackCount.MkAttackCount measure n (Filter.rewrite pairs f)) attackers)
          frequency
          (fmap (rewriteCondition pairs) while_)
      )
  AttackTrigger.Unblocked f -> AttackTrigger.Unblocked (Filter.rewrite pairs f)

rewriteBlockTrigger :: [(TextWord.TextWord, TextWord.TextWord)] -> BlockTrigger.BlockTrigger -> BlockTrigger.BlockTrigger
rewriteBlockTrigger pairs (BlockTrigger.MkBlockTrigger role creature count) =
  BlockTrigger.MkBlockTrigger role (Filter.rewrite pairs creature) $ case count of
    BlockCount.Once -> BlockCount.Once
    BlockCount.EachCreature others -> BlockCount.EachCreature (Filter.rewrite pairs others)
    BlockCount.AtLeast n others -> BlockCount.AtLeast n (Filter.rewrite pairs others)

-- CR 612.1 through a casting permission's one condition.
rewriteCastingPermission :: [(TextWord.TextWord, TextWord.TextWord)] -> CastingPermission.CastingPermission -> CastingPermission.CastingPermission
rewriteCastingPermission pairs permission = case permission of
  CastingPermission.CastAsThoughItHadFlash condition additional -> CastingPermission.CastAsThoughItHadFlash (fmap (rewriteCondition pairs) condition) additional
  CastingPermission.CastFromGraveyard family additional -> CastingPermission.CastFromGraveyard family (fmap (Filter.rewriteComponent pairs) additional)
  CastingPermission.PlayFromGraveyard -> permission
  CastingPermission.CastFromLibraryWhileSearching -> permission

-- CR 612.1 through Condition's predicate vocabulary. A Condition reached through
-- a CR 611.2b duration comes here by way of rewriteDuration below.
rewriteCondition :: [(TextWord.TextWord, TextWord.TextWord)] -> Condition.Type.Condition -> Condition.Type.Condition
rewriteCondition pairs condition = case condition of
  Condition.Type.Compares c ->
    Condition.Type.Compares
      c
        { Compares.measured = rewriteQuantity pairs (Compares.measured c),
          Compares.threshold = rewriteQuantity pairs (Compares.threshold c)
        }
  Condition.Type.Any conditions -> Condition.Type.Any (fmap (rewriteCondition pairs) conditions)
  Condition.Type.All conditions -> Condition.Type.All (fmap (rewriteCondition pairs) conditions)
  -- A phase or step names no subtype.
  Condition.Type.During _ -> condition

-- CR 612.1 through ONE clause of an activated ability's CR 602.5 "activate only
-- ..." rider, which is printed text like the rest of the ability.
--
-- Exhaustive rather than a wildcard, for rewriteTriggerCondition's reason: a new
-- arm carrying a Condition or a Filter must break this build. Only the board
-- condition and SpendOnly's printed colour hold a word -- every other arm names
-- a phase, a turn, the combat record, or a count of activations, and CR 612.1's
-- swap finds nothing in one to act on.
rewriteRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> ActivationRestriction.ActivationRestriction -> ActivationRestriction.ActivationRestriction
rewriteRestriction pairs restriction = case restriction of
  ActivationRestriction.OnlyIf condition -> ActivationRestriction.OnlyIf (rewriteCondition pairs condition)
  ActivationRestriction.SorcerySpeed -> restriction
  ActivationRestriction.DuringPhase _ -> restriction
  ActivationRestriction.DuringTurn _ -> restriction
  ActivationRestriction.AttackedThisStep -> restriction
  ActivationRestriction.AfterBlockersDeclared -> restriction
  ActivationRestriction.DuringCombatBefore _ -> restriction
  ActivationRestriction.BeforeStep _ -> restriction
  ActivationRestriction.TimesEver _ -> restriction
  ActivationRestriction.TimesEachTurn _ -> restriction
  ActivationRestriction.DuringThrow -> restriction
  ActivationRestriction.InstantSpeed -> restriction
  ActivationRestriction.SpendOnly specified -> ActivationRestriction.SpendOnly (rewriteManaSpecification pairs specified)

-- CR 612.2 over a printed colour of mana (Crimson Hellkite's "Spend only red
-- mana"); a regression fence, no test changing its text.
rewriteManaSpecification :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaSpecification.ManaSpecification -> ManaSpecification.ManaSpecification
rewriteManaSpecification pairs specified = case specified of
  ManaSpecification.AnyMana -> specified
  ManaSpecification.ChosenColor -> specified
  ManaSpecification.OfColor c -> ManaSpecification.OfColor (TextWord.Engine.colorAfter pairs c)

-- CR 612.1 through a Duration, which Parun.Types.Duration holds as the card
-- prints it: a CR 611.2b "for as long as ..." clause is rules text like any
-- other, so the words inside its Condition are reachable. Every other arm is a
-- turn-structure window and names none.
--
-- Exhaustive rather than a wildcard, for rewriteTriggerCondition's reason: a new
-- arm carrying a Condition or a Filter must break this build.
rewriteDuration :: [(TextWord.TextWord, TextWord.TextWord)] -> Duration.Duration -> Duration.Duration
rewriteDuration pairs duration = case duration of
  Duration.ForAsLongAs condition -> Duration.ForAsLongAs (rewriteCondition pairs condition)
  Duration.UntilEndOfTurn -> duration
  Duration.Indefinite -> duration
  Duration.Perpetual -> duration
  Duration.UntilYourNextTurn -> duration
  Duration.UntilNextStepOf _ -> duration
  Duration.UntilEndOfYourNextTurn -> duration
  -- A PlayerRef names its seat structurally -- by slot or by relation -- and
  -- carries no word CR 612.1 can swap, so the reference is left alone.
  Duration.UntilEndOfNextTurnOf _ -> duration
  Duration.DuringNextTurnOf _ -> duration
  Duration.DuringNextUntapStepOf _ -> duration
  Duration.DuringYourNextTurn -> duration
  Duration.DuringThatExtraTurn -> duration
  Duration.UntilEndOfCombat -> duration
  Duration.UntilEndOfCombatOnYourNextTurn -> duration
  -- CR 116.2c's price, which is a Cost and not a Condition. An activated
  -- ability's own cost is left alone by this descent for the same reason: no
  -- Cost arm carries a subtype word outside a Filter, and the Filters a cost's
  -- non-mana components hold are matched where the cost is paid.
  Duration.UntilPaid _ -> duration
  Duration.UntilUsed _ -> duration
  Duration.UntilEvent DurationEvent.SourceTurnedFaceDown -> duration
  Duration.UntilEvent DurationEvent.Planeswalked -> duration
  Duration.UntilEvent DurationEvent.SourceExiledCard -> duration
  Duration.UntilEvent DurationEvent.SourceLeftBattlefield -> duration
  Duration.UntilEvent (DurationEvent.BoundCardCast _) -> duration
  Duration.UntilEvent (DurationEvent.CardCast _) -> duration
  Duration.UntilEvent (DurationEvent.SpellCast criterion) -> Duration.UntilEvent (DurationEvent.SpellCast (Filter.rewrite pairs criterion))

-- CR 612.1 through the counters a CR 122.5 move carries: the count, and the
-- kind wherever the CARD names one (MovedKinds.kindOf's three arms), since CR
-- 122.1b's keyword counter carries a keyword. Every, Chosen, AnyNumber,
-- AtLeastOne, EachAbsentKind and UpToOneChosen name no kind at all.
--
-- A REGRESSION FENCE on the three kind-bearing arms rather than a proven
-- behaviour: every kind-naming move in data/cards/ names +1/+1, which carries no
-- word CR 612.2 can swap, so mutating those lines reddens nothing.
rewriteMovedKinds :: [(TextWord.TextWord, TextWord.TextWord)] -> MovedKinds.MovedKinds -> MovedKinds.MovedKinds
rewriteMovedKinds pairs kinds = case kinds of
  MovedKinds.Every -> kinds
  MovedKinds.Named kind quantity -> MovedKinds.Named (Filter.rewriteCounterKind pairs kind) (rewriteQuantity pairs quantity)
  MovedKinds.EveryOfKind kind -> MovedKinds.EveryOfKind (Filter.rewriteCounterKind pairs kind)
  MovedKinds.Chosen quantity -> MovedKinds.Chosen (rewriteQuantity pairs quantity)
  MovedKinds.AnyNumber -> kinds
  MovedKinds.AtLeastOne -> kinds
  MovedKinds.AnyNumberOfKind kind -> MovedKinds.AnyNumberOfKind (Filter.rewriteCounterKind pairs kind)
  MovedKinds.EachAbsentKind -> kinds
  MovedKinds.UpToOneChosen -> kinds

-- CR 612.1 through a RemovalCount: the count's Quantity, where it writes one.
rewriteRemovalCount :: [(TextWord.TextWord, TextWord.TextWord)] -> RemovalCount.RemovalCount -> RemovalCount.RemovalCount
rewriteRemovalCount pairs count = case count of
  RemovalCount.Exactly quantity -> RemovalCount.Exactly (rewriteQuantity pairs quantity)
  RemovalCount.UpTo quantity -> RemovalCount.UpTo (rewriteQuantity pairs quantity)
  RemovalCount.AnyNumber -> count
  RemovalCount.All -> count

-- CR 612.1 through a Quantity: a Count's Filter is where the subtype word hides,
-- and its Aggregation may name a further Quantity. Every remaining arm is a leaf.
rewritePlacement :: [(TextWord.TextWord, TextWord.TextWord)] -> LibraryPlacement.LibraryPlacement -> LibraryPlacement.LibraryPlacement
rewritePlacement pairs placement = case placement of
  LibraryPlacement.Stated _ -> placement
  LibraryPlacement.OwnerChooses -> placement
  LibraryPlacement.RandomOrder _ -> placement
  LibraryPlacement.ArrangedBy _ -> placement
  LibraryPlacement.Beneath quantity -> LibraryPlacement.Beneath (rewriteQuantity pairs quantity)
  LibraryPlacement.BeneathOrBottom quantity -> LibraryPlacement.BeneathOrBottom (rewriteQuantity pairs quantity)

-- A per-target cost change's filter and count, each descended as
-- rewriteEffect descends its own (CR 612.1).
rewritePerTarget :: [(TextWord.TextWord, TextWord.TextWord)] -> PerTarget.PerTarget -> PerTarget.PerTarget
rewritePerTarget pairs (PerTarget.MkPerTarget matching each) = PerTarget.MkPerTarget (Filter.rewrite pairs matching) (fmap (rewriteQuantity pairs) each)

-- CR 612.1 through a quantity: every Filter it nests (a Count's, an activation
-- tally's, a linked card's criterion) and every nested quantity, through
-- Parun.Engine.Children, plus the text-bearing leaves below. The keywords
-- TimesPaid and CastUsing designate are deliberately NOT rewritten:
-- Parun.Engine.Cast keys the record it stamps by the keyword the spell had as
-- CR 601.2b announced it (Cost.spellKeywords), which a later text change does
-- not reach.
rewriteQuantity :: [(TextWord.TextWord, TextWord.TextWord)] -> Quantity.Type.Quantity -> Quantity.Type.Quantity
rewriteQuantity pairs quantity = case quantity of
  -- Omnath, Locus of Mana's "each unspent green mana": rewriteManaFilter's
  -- reading.
  Quantity.Type.ManaCount count -> Quantity.Type.ManaCount count {ManaCount.filter = rewriteManaFilter pairs (ManaCount.filter count)}
  -- CR 700.5's "devotion to black": a colour word used as one (CR 612.2).
  Quantity.Type.Devotion devotion -> Quantity.Type.Devotion devotion {Devotion.colors = Set.map (TextWord.Engine.colorAfter pairs) (Devotion.colors devotion)}
  -- CR 903.4's "each color" enumerates CR 105.1's colours rather than printing
  -- colour words, and CR 700.8's party types are the rule's: the traversal
  -- leaves both alone.
  _ ->
    Identity.runIdentity
      ( Children.quantityChildren
          (Children.descendingQuantities (Identity.Identity . rewriteQuantity pairs))
            { Children.nest = Identity.Identity . Filter.rewrite pairs
            }
          quantity
      )

-- CR 612.1 over an entry row's counter AMOUNTS and KEYS -- "enters with a +1/+1
-- counter for each Goblin you control" is a Count like any other, and CR 122.1b's
-- keyword counter carries a keyword whose word rule 612 reaches.
-- rewriteWithCounters' collision combiner comes with the key rewrite: CR 122.1's
-- last sentence makes counters with the same name interchangeable, so a swap
-- that collides two keys merges the rows rather than keeping an arbitrary
-- survivor.
rewriteEntryRiders :: [(TextWord.TextWord, TextWord.TextWord)] -> EntryRiders.EntryRiders Quantity.Type.Quantity (GrantedAbility.GrantedAbility Card.Type.Card) -> EntryRiders.EntryRiders Quantity.Type.Quantity (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteEntryRiders pairs riders =
  riders
    { EntryRiders.counters =
        Map.mapKeysWith (\greater lesser -> Quantity.Type.Arithmetic (Arithmetic.Plus (Plus.MkPlus lesser greater))) (Filter.rewriteCounterKind pairs)
          . fmap (rewriteQuantity pairs)
          $ EntryRiders.counters riders,
      EntryRiders.faceDown = fmap (\state -> state {FaceDownState.listed = rewriteListing pairs (FaceDownState.listed state)}) (EntryRiders.faceDown riders),
      -- CR 612.1 over CR 303.4f's narrowing filter, EntersAttachedTo's rewrite.
      EntryRiders.attachedTo = fmap rewriteHost (EntryRiders.attachedTo riders),
      -- CR 612.1 over CR 611.2e's effects, ModifyTarget's modification rewrite.
      EntryRiders.characteristics = fmap (rewriteModification pairs) (EntryRiders.characteristics riders)
    }
  where
    rewriteHost host = case host of
      EntryHost.Specified _ -> host
      EntryHost.Chosen f -> EntryHost.Chosen (Filter.rewrite pairs f)

-- CR 612.1 / 612.2 over a CR 708.2 listing's subtypes and quoted abilities, both
-- printed on the card whose effect lists them. The text-change scenarios of an
-- evolved Cyber Conversion and a hacked Yedora, Grave Gardener prove the
-- subtype swap.
rewriteListing :: [(TextWord.TextWord, TextWord.TextWord)] -> FaceDownCharacteristics.FaceDownCharacteristics (GrantedAbility.GrantedAbility Card.Type.Card) -> FaceDownCharacteristics.FaceDownCharacteristics (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteListing pairs listed =
  listed
    { FaceDownCharacteristics.typeLine = List.foldl' (\typeLine pair -> maybe typeLine (\(_, _, swapped) -> swapped) (swapSubtype pair typeLine)) (FaceDownCharacteristics.typeLine listed) pairs,
      FaceDownCharacteristics.abilities = fmap (rewriteGrantedAbility pairs) (FaceDownCharacteristics.abilities listed)
    }

-- CR 612.2: one subtype swap over a type line holding the old word, with the
-- swapped pair, which a defined face's name follows too (CR 612.2a).
swapSubtype :: (TextWord.TextWord, TextWord.TextWord) -> TypeLine.TypeLine -> Maybe (Subtype.Type.Subtype, Subtype.Type.Subtype, TypeLine.TypeLine)
swapSubtype pair typeLine = case pair of
  (TextWord.SubtypeWord old, TextWord.SubtypeWord new)
    | Set.member old subtypes -> Just (old, new, typeLine {TypeLine.subtypes = Set.insert new (Set.delete old subtypes)})
  _ -> Nothing
  where
    subtypes = TypeLine.subtypes typeLine

-- CR 612.1 through CR 208.2a's characteristic-defining power and toughness. Both
-- boxes are rewritten rather than only the one a card fills, since seedCharacteristicPT
-- already keeps them as a pair.
rewriteCharacteristicPT :: [(TextWord.TextWord, TextWord.TextWord)] -> CharacteristicPT.CharacteristicPT -> CharacteristicPT.CharacteristicPT
rewriteCharacteristicPT pairs cda =
  CharacteristicPT.MkCharacteristicPT
    { CharacteristicPT.power = rewriteQuantity pairs (CharacteristicPT.power cda),
      CharacteristicPT.toughness = rewriteQuantity pairs (CharacteristicPT.toughness cda)
    }
