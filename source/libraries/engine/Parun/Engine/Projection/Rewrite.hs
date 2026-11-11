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
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationProhibition as ActivationProhibition
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AffectedUnless as AffectedUnless
import qualified Parun.Types.AloneUnless as AloneUnless
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.AmassKind as AmassKind
import qualified Parun.Types.Arithmetic as Arithmetic
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackLimitUnless as AttackLimitUnless
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.BlockPermission as BlockPermission
import qualified Parun.Types.BlockRequirement as BlockRequirement
import qualified Parun.Types.CantAttackPlayer as CantAttackPlayer
import qualified Parun.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Parun.Types.CantBlockCreatures as CantBlockCreatures
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.ChosenCardInHand as ChosenCardInHand
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.Condition as Condition.Type
import qualified Parun.Types.CopyClause as CopyClause
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.CrewRestriction as CrewRestriction
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.EachCardInExiledPile as EachCardInExiledPile
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.EachCardInHand as EachCardInHand
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.EntryRestriction as EntryRestriction
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.FullText as FullText
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LimitUnless as LimitUnless
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaRider as ManaRider
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Modal as Modal.Type
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PerCreature as PerCreature
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.Plus as Plus
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.RandomCardInGraveyard as RandomCardInGraveyard
import qualified Parun.Types.RemovalCount as RemovalCount
import Parun.Types.ReplacementEffect (ReplacementEffect)
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.Subtype as Subtype.Type
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.TriggerCondition as TriggerCondition
import Parun.Types.TriggeredAbility (TriggeredAbility)
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.WordFamily as WordFamily

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
-- Parun.Engine.PlayerEffect: rewriteEffect's descent into a player effect has
-- to reach it for a restriction a RESOLUTION stores (Liliana, Untouched by
-- Death's -3), and Parun.Engine.PlayerEffect imports this module rather than
-- the other way round. Children.playerEffectChildren cases on STRUCTURE and
-- never on which player effect it is, so no rule in the closed half learns an
-- effect's identity from it.
--
-- Children.playerEffectChildren's descent: every Filter takes Filter.rewrite
-- (Thalia's "noncreature spells", Edgewalker's "Cleric spells" on the printed
-- road, Liliana's "Zombie spells" on the stored one; a cost modifier's own
-- criterion, its target criteria, its per-target and per-each counts and each
-- component it adds), every Quantity rewriteQuantity, every cost and component
-- Filter.rewriteCost's reading (Wall of Fortune's "tap an untapped Wall"; a
-- component an effect adds), the mana a
-- clause keeps or spends rewriteManaFilter's colour word (Celestial Dawn's
-- "white mana"), and an addition's scale is Drought's "for each black mana
-- symbol", a colour word used as one (CR 612.2). A rule-702 designator, CR
-- 605.1a's and CR 606.2's classifications, a zone, a turn and a count name no
-- word CR 612.2 reaches; CR 201.4's chosen names are read off the source, and
-- CR 612.2's second sentence keeps a swap off a card name even if they were not.
--
-- CR 612.2's family gate is not restated at the Filter descent, for the reason
-- Filter.rewrite's own comment gives: a HasSubtype atom may name a word of any
-- family, so the family the word is used AS is the family it belongs to, and the
-- exact lookup already asks CR 612.2's question. A Magical Hack's land-type pair
-- therefore leaves Edgewalker's Cleric alone.
rewritePlayerEffect :: [(TextWord.TextWord, TextWord.TextWord)] -> PlayerEffect.PlayerEffect -> PlayerEffect.PlayerEffect
rewritePlayerEffect pairs =
  Identity.runIdentity
    . Children.playerEffectChildren
      Children.playerEffectLeaves
        { Children.playerFilter = Identity.Identity . Filter.rewrite pairs,
          Children.playerQuantity = Identity.Identity . rewriteQuantity pairs,
          Children.playerCost = Identity.Identity . Filter.rewriteCost pairs,
          Children.playerCostComponent = Identity.Identity . Filter.rewriteComponent pairs,
          Children.playerColor = Identity.Identity . TextWord.Engine.colorAfter pairs,
          Children.playerManaFilter = Identity.Identity . rewriteManaFilter pairs,
          Children.playerSpending = Identity.Identity . rewriteSpendManaAsThough pairs,
          Children.playerManaRestriction = Identity.Identity . rewriteManaRestriction pairs,
          Children.playerDamage = Identity.Identity . rewriteDamagePattern pairs
        }

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
rewriteCard pairs = Identity.runIdentity . Children.cardChildren (Identity.Identity . rewriteFace pairs)

-- One pair at a time: the name and type line first, together, since CR 612.2a
-- changes a token's name only where the word is its creature type; then the
-- rest through Parun.Engine.Children's traversal at Identity.
rewriteFace :: [(TextWord.TextWord, TextWord.TextWord)] -> Face.Face Card.Type.Card -> Face.Face Card.Type.Card
rewriteFace pairs face =
  let apply1 f (from, to) =
        let renamed = case swapSubtype (from, to) (Face.typeLine f) of
              Just (old, new, typeLine) ->
                f
                  { Face.typeLine = typeLine,
                    Face.name = rewriteTokenName old new (Face.name f)
                  }
              Nothing -> f
         in Identity.runIdentity (Children.faceChildren (faceRewrites [(from, to)]) renamed)
   in List.foldl' apply1 face pairs

-- CR 612.1 over a face's children. Left as written, as before this walk was
-- shared: its printed power and toughness, its rule-affecting rows, its costs
-- and permissions.
faceRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.FaceChildren Card.Type.Card Identity.Identity
faceRewrites pair =
  Children.faceLeaves
    { -- CR 702.14a's land-type word inside a landwalk. Two instances the
      -- rewrite collapses onto one keyword stay two instances.
      Children.faceKeyword = Identity.Identity . Filter.rewriteKeyword pair,
      -- CR 612.2's colour word in the defining text ("a 1/1 black Rat
      -- token"), which is where a token's colour comes from (CR 111.3).
      Children.faceColor = Identity.Identity . TextWord.Engine.colorAfter pair,
      -- CR 208.2a's star, unevaluated as at layer 3.
      Children.faceCharacteristicPT = Identity.Identity . rewriteCharacteristicPT pair,
      -- CR 101.1's ceiling on X, whose Quantity can Count a criterion
      -- naming a land type word. A regression fence: neither printing
      -- pairs a bounded X with one -- both say "the greatest toughness
      -- among creatures you control" -- so no test can falsify it.
      Children.faceQuantity = Identity.Identity . rewriteQuantity pair,
      Children.faceModal = Identity.Identity . rewriteModal pair,
      Children.faceActivated = Identity.Identity . rewriteActivatedAbility pair,
      -- CR 604.2's static ability, on the card a token or emblem is
      -- defined with. A regression fence rather than a proved behaviour,
      -- as faceQuantity above is: a walk of every defined face in
      -- data/cards for a replacementEffects key (2026-08-21) found none
      -- at all, so no board can tell this line from its absence.
      -- Parun.ReplacementSpec's Dragonstorm Globe case is what proves
      -- rewritePrintedReplacement itself.
      Children.faceReplacement = Identity.Identity . rewritePrintedReplacement pair,
      Children.faceTriggered = Identity.Identity . rewriteTriggeredAbility pair,
      Children.faceStatic = Identity.Identity . rewriteStaticAbility pair
    }

-- A whole static ability under CR 612.1, for the defined-card walk above. A
-- permanent's own statics are reached piecemeal instead, being read per layer.
-- The lingering duration (CR 611.2) names a window, not a word, and is left
-- alone, as are the zones it functions from.
rewriteStaticAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> StaticAbility.StaticAbility (GrantedAbility.GrantedAbility Card.Type.Card) -> StaticAbility.StaticAbility (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteStaticAbility pairs = Identity.runIdentity . Children.staticAbilityChildren (abilityRewrites pairs)

-- A player static ability under CR 612.1: its clause and its effect, the two
-- halves Parun.Engine.PlayerEffect.printedRows rewrites.
rewritePlayerStaticAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> PlayerStaticAbility.PlayerStaticAbility -> PlayerStaticAbility.PlayerStaticAbility
rewritePlayerStaticAbility pairs = Identity.runIdentity . Children.playerStaticAbilityChildren (abilityRewrites pairs)

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

-- CR 612.1 over an ability record's children: Parun.Engine.Children's
-- traversal at Identity, shared by every kind of ability below. Every Cost takes
-- Filter.rewriteCost (CR 118.1, CR 602.1a), so a Magical Hack naming Forest
-- moves which land Dark Heart of the Wood's cost demands. Every Condition takes
-- rewriteCondition: CR 702.178a's "as long as" gate, CR 603.4's intervening
-- "if", a reduction's or an alternative cost's. CR 602.5's "activate only if"
-- rider is printed text like any other: Parun.ManaSpec's "Nimbus Maze text
-- change" is what proves it, a Magical Hack naming Island moving which land the
-- {W} route's rider counts. A CR 603.8 trigger condition is where a triggered
-- ability's word usually is.
--
-- CR 101.1's ceiling on an activated ability's X and CR 107.3c's defined X are
-- Quantities, rewritten as faceRewrites' faceQuantity is. A regression fence: no
-- printing pairs a bounded or defined X on an activated ability with a Quantity
-- naming a land type word, so no board can tell those positions from their
-- absence.
--
-- Left as written, as before this walk was shared: an activated ability's
-- keyword stamp (an origin, CR 702), a trigger's "only once" rider and a static
-- ability's lingering duration (a window). A granted rule ability (CR 613.11) is rewritten where its
-- gatherer reads it, off its source's text changes, and no card in data/cards
-- quotes one in a grant this walk reaches. Backup's is expanded at resolution
-- (Parun.Engine.Resolve.Common.expandGrant) and stored, where no rewrite runs.
abilityRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.AbilityChildren Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) Identity.Identity
abilityRewrites pairs =
  Children.abilityLeaves
    { Children.abilityCost = Identity.Identity . Filter.rewriteCost pairs,
      Children.abilityQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.abilityModal = Identity.Identity . rewriteModal pairs,
      Children.abilityRestriction = Identity.Identity . rewriteRestriction pairs,
      Children.abilityCondition = Identity.Identity . rewriteCondition pairs,
      Children.abilityTrigger = Identity.Identity . rewriteTriggerCondition pairs,
      Children.abilityAffected = Identity.Identity . rewriteAffected pairs,
      Children.abilityModification = Identity.Identity . rewriteModification pairs,
      Children.abilityPlayerEffect = Identity.Identity . rewritePlayerEffect pairs,
      Children.abilityReplacement = Identity.Identity . rewriteReplacementEffect pairs,
      Children.abilityFilter = Identity.Identity . Filter.rewrite pairs,
      Children.abilitySpendMana = Identity.Identity . rewriteSpendManaAsThough pairs
    }

-- CR 612.1 over an ACTIVATED ability printed on a permanent.
rewriteActivatedAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteActivatedAbility pairs = Identity.runIdentity . Children.activatedAbilityChildren (abilityRewrites pairs)

-- CR 612.1 over a GRANTED ability (CR 613.1f), whichever of CR 113.3's kinds it
-- is. The words are printed on the GRANTER, so a text change affecting that
-- permanent rewrites them before layer 6 hands them over.
rewriteGrantedAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> GrantedAbility.GrantedAbility Card.Type.Card -> GrantedAbility.GrantedAbility Card.Type.Card
rewriteGrantedAbility pairs = Identity.runIdentity . Children.grantedAbilityChildren (abilityRewrites pairs)

-- CR 612.1 over a TRIGGERED ability printed on a permanent: its condition, its
-- intervening "if" and its payload.
rewriteTriggeredAbility :: [(TextWord.TextWord, TextWord.TextWord)] -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteTriggeredAbility pairs = Identity.runIdentity . Children.triggeredAbilityChildren (abilityRewrites pairs)

-- CR 612.1 over a REPLACEMENT effect printed on a permanent, which CR 604.2
-- makes a static ability's continuous effect and so text in the same text box as
-- a triggered ability's. Two carriers: the ability's own "as long as" clause, and
-- the effect itself.
rewritePrintedReplacement :: [(TextWord.TextWord, TextWord.TextWord)] -> PrintedReplacement.PrintedReplacement Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> PrintedReplacement.PrintedReplacement Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewritePrintedReplacement pairs = Identity.runIdentity . Children.printedReplacementChildren (abilityRewrites pairs)

-- CR 612.1 through the replacement effect itself: Parun.Engine.Children's
-- traversal at Identity. Every Filter takes Filter.rewrite (the moving,
-- entering, destroyed, turned-up or attached object; a token pattern's;
-- Steamflogger Boss's Rigger performer, per the Unstable FAQ; Naked
-- Singularity's land type, per its 2004-10-04 ruling; a printed redirection
-- destination; CR 400.11c's wish filter, the same Filter rewriteEffect swaps
-- inside an Effect.FromOutsideTheGame, so the two roads agree; CR 201.4a's
-- naming restriction; Living Lore's exiled card, which Parun.ReplacementSpec's
-- Tidewalker case proves), every Quantity rewriteQuantity, every nested effect
-- rewriteEffect, a token's card rewriteCard, a granted ability
-- rewriteGrantedAbility, a keyword CR 702.14a's word and an entry option's
-- subtype CR 205.3's, a counter kind Filter.rewriteCounterKind, entry counters
-- rewriteWithCounters and a copy rewriteAsCopy.
--
-- Kept: a CR 109.5 relation, a baked recipient and the cost a turn-up
-- procedure must have paid (engine-baked, compared against the payment) name
-- no printed word, and the history reads are the engine's.
--
-- Classification, not identity: Children.replacementEffectChildren cases on
-- STRUCTURE, by CR 614.1 event class, and asks nothing else.
--
-- NO BOARD OBSERVES the printed redirection destination (Pariah's
-- Filter.IsHostOfSource), the nested damage programs (Kill-Suit Cultist), the
-- counted prevention ceiling (Shield of the Avatar counts a card type), the
-- wish filter (Ring of Ma'rûf's @And []@), or an option's keywords (Molten
-- Sentry's haste and defender carry no word): each is the rule rather than a
-- proven behaviour.
rewriteReplacementEffect :: [(TextWord.TextWord, TextWord.TextWord)] -> ReplacementEffect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ReplacementEffect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteReplacementEffect pairs =
  Identity.runIdentity
    . Children.replacementEffectChildren
      Children.replacementLeaves
        { Children.replacementFilter = Identity.Identity . Filter.rewrite pairs,
          Children.replacementQuantity = Identity.Identity . rewriteQuantity pairs,
          Children.replacementEffect = Identity.Identity . rewriteEffect pairs,
          Children.replacementAbility = Identity.Identity . rewriteGrantedAbility pairs,
          Children.replacementCard = Identity.Identity . rewriteCard pairs,
          Children.replacementKeyword = Identity.Identity . Filter.rewriteKeyword pairs,
          Children.replacementSubtype = Identity.Identity . TextWord.Engine.subtypeAfter pairs,
          Children.replacementCounterKind = Identity.Identity . Filter.rewriteCounterKind pairs,
          Children.replacementDamage = Identity.Identity . rewriteDamagePattern pairs,
          Children.replacementCounters = Identity.Identity . rewriteWithCounters pairs,
          Children.replacementCopy = Identity.Identity . rewriteAsCopy pairs
        }

-- CR 612.1 through CR 707.9's "except ..." clause. Exhaustive rather than a
-- wildcard, so a later arm carrying a word fails to compile here.
--
-- NO BOARD OBSERVES IT, an entry option's keywords' position: what changes
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

-- The modal payload both abilities carry, its shared clauses too: Children's
-- modalChildren at Identity. Both halves of a mode: its clauses' conditions,
-- costs and effects, and its TARGET SLOTS, whose Filter is the candidate set CR
-- 601.2c (imported by CR 602.2b) reads. The Pool is not an omission: it names a
-- rules category (CR 115) rather than a word printed on the card. The mode
-- selection, a clause's asker and the slot names stay leaves, as before.
rewriteModal :: [(TextWord.TextWord, TextWord.TextWord)] -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteModal pairs =
  Identity.runIdentity
    . Children.modalChildren
      Children.modalLeaves
        { Children.modalCondition = Identity.Identity . rewriteCondition pairs,
          Children.modalPayGate = Identity.Identity . rewritePayGate pairs,
          Children.modalEffect = Identity.Identity . rewriteEffect pairs,
          Children.modalTargetSlot = Identity.Identity . rewriteTargetSlot pairs
        }

-- CR 612.1 through the cost a clause offers as it resolves (CR 118.12).
-- Lithophage's "sacrifice this creature unless you sacrifice a Mountain" is the
-- producer: Magical Hack swaps the land type word, and CR 612.2 licenses it
-- because the word is used as a land type.
--
-- Written out field by field rather than as a record update, Children.costChildren's
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
-- CR 612.1 through a trigger's own condition (CR 603.1): its Filters, its
-- conditions and a printed colour of mana. A counter kind, a slot name and a
-- turn scope hold no word Parun.Types.ChangeText's pairs swap, and a
-- transform's face is a card name (CR 612.2), so the leaves keep them.
rewriteTriggerCondition :: [(TextWord.TextWord, TextWord.TextWord)] -> TriggerCondition.TriggerCondition -> TriggerCondition.TriggerCondition
rewriteTriggerCondition pairs =
  Identity.runIdentity
    . Children.triggerConditionChildren
      Children.triggerLeaves
        { Children.triggerFilter = Identity.Identity . Filter.rewrite pairs,
          Children.triggerNested = Identity.Identity . rewriteTriggerCondition pairs,
          Children.triggerState = Identity.Identity . rewriteCondition pairs,
          Children.triggerMana = Identity.Identity . rewriteManaSpecification pairs
        }

-- CR 612.1 through a casting permission's one condition.
rewriteCastingPermission :: [(TextWord.TextWord, TextWord.TextWord)] -> CastingPermission.CastingPermission -> CastingPermission.CastingPermission
rewriteCastingPermission pairs permission = case permission of
  CastingPermission.CastAsThoughItHadFlash condition additional -> CastingPermission.CastAsThoughItHadFlash (fmap (rewriteCondition pairs) condition) additional
  CastingPermission.CastFromGraveyard family additional -> CastingPermission.CastFromGraveyard family (fmap (Filter.rewriteComponent pairs) additional)
  CastingPermission.PlayFromGraveyard -> permission
  CastingPermission.CastFromLibraryWhileSearching -> permission

-- CR 612.1 through Condition's predicate vocabulary: its Quantities and nested
-- conditions. A phase window's turn scope names a seat structurally and holds
-- no word, so the leaves keep it. A Condition reached through a CR 611.2b
-- duration comes here by way of rewriteDuration below.
rewriteCondition :: [(TextWord.TextWord, TextWord.TextWord)] -> Condition.Type.Condition -> Condition.Type.Condition
rewriteCondition pairs =
  Identity.runIdentity
    . Children.conditionChildren
      Children.conditionLeaves
        { Children.conditionQuantity = Identity.Identity . rewriteQuantity pairs,
          Children.conditionNested = Identity.Identity . rewriteCondition pairs
        }

-- CR 612.1 through ONE clause of an activated ability's CR 602.5 "activate only
-- ..." rider, which is printed text like the rest of the ability.
--
-- Exhaustive rather than a wildcard, so a new arm carrying a Condition or a
-- Filter must break this build. Only the board condition and SpendOnly's
-- printed colour hold a word -- every other arm names a phase, a turn, the
-- combat record, or a count of activations, and CR 612.1's swap finds nothing
-- in one to act on.
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
-- other, so the words inside its Condition are reachable, as is the Filter of
-- the spells whose casting ends one. A PlayerRef names its seat structurally
-- and a slot or a baked card holds no word. CR 116.2c's price is a Cost, left
-- alone as an activated ability's own cost is: no Cost arm carries a subtype
-- word outside a Filter, and the Filters a cost's non-mana components hold are
-- matched where the cost is paid.
rewriteDuration :: [(TextWord.TextWord, TextWord.TextWord)] -> Duration.Duration -> Duration.Duration
rewriteDuration pairs =
  Identity.runIdentity
    . Children.durationChildren
      Children.durationLeaves
        { Children.durationCondition = Identity.Identity . rewriteCondition pairs,
          Children.durationFilter = Identity.Identity . Filter.rewrite pairs
        }

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
