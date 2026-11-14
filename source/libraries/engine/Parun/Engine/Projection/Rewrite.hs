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
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.AmassKind as AmassKind
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.Condition as Condition.Type
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Modal as Modal.Type
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.RemovalCount as RemovalCount
import Parun.Types.ReplacementEffect (ReplacementEffect)
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.Subtype as Subtype.Type
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.TriggerCondition as TriggerCondition
import Parun.Types.TriggeredAbility (TriggeredAbility)
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.WordFamily as WordFamily

-- CR 613.1f's grant carries a whole quoted ability, and a card's abilities are
-- written against a whole Card (CR 707.8a).
type Modification = Modification.Modification (GrantedAbility.GrantedAbility Card.Type.Card)

-- Apply text-changes to a modification (CR 612.1/612.2). Parun.Engine.Children's
-- traversal at Identity, looking each word up once in the composed table.
--
-- CR 612.2 gates the two subtype visitors: the position's family is fixed by
-- its constructor, and the PAIR's family is read off the word being replaced,
-- which is why no family tag rides on the stored ChangeWord. Subtype and
-- RemoveSubtype carry no family to check the word against, so the traversal
-- leaves them whole; Parun.EffectLintSpec keeps land-type and creature-type
-- words out of them.
--
-- Every other word is text printed on the GRANTER (CR 612.3): a hacked Lord of
-- Atlantis grants swampwalk, and the same descent reaches a granted enchant's
-- Filter, a flash permission's condition, the Filters naming names or exiled
-- cards, a lent card's restrictions, both boxes of a power and toughness
-- change (a hacked Aspect of Wolf counts the new type), and the whole quoted
-- ability of a CR 613.1f grant or a CR 612.6 full text -- through
-- rewriteGrantedAbility, mutually recursive with this, since that ability's
-- own clauses hold effects that can grant again. A colour word is rewritten
-- where used as one, "is black" printed on the setter.
--
-- Left whole, each for want of a word: a backup grant's keywords are its
-- source's copiable values as the ability was put on the stack (CR 702.165d,
-- Parun.Engine.Resolve.Effect's expandGrant); a lost ability's name names an
-- ability, no subtype; a full text's graveyard is a player. The traversal
-- leaves the rest whole for the same want: card types and supertypes are other
-- lists than subtypes (CR 205.2a, 205.4a), a keyword family names CR 702.14a's
-- generic term, and a stored ChangeWord's words are its own resolution's choice
-- (CR 608.2d), a text changer's printed clause being rewriteEffect's.
rewriteModification :: [(TextWord.TextWord, TextWord.TextWord)] -> Modification -> Modification
-- No pairs, no rebuild: Parun.Engine.Projection's staticParts asks this of
-- every static ability on every projection.
rewriteModification [] = id
rewriteModification pairs =
  let -- `inFamily` is CR 612.2's gate.
      swap inFamily s = case lookup (TextWord.SubtypeWord s) pairs of
        Just (TextWord.SubtypeWord t) | inFamily s -> t
        _ -> s
   in Identity.runIdentity
        . Children.modificationChildren
          Children.MkModificationChildren
            { Children.modificationKeyword = Identity.Identity . Filter.rewriteKeyword pairs,
              Children.modificationStampedKeyword = pure,
              Children.modificationTargetSlot = Identity.Identity . rewriteTargetSlot pairs,
              Children.modificationCastingPermission = Identity.Identity . rewriteCastingPermission pairs,
              Children.modificationAbility = Identity.Identity . rewriteGrantedAbility pairs,
              Children.modificationFilter = Identity.Identity . Filter.rewrite pairs,
              Children.modificationRestriction = Identity.Identity . rewriteRestriction pairs,
              Children.modificationAbilityName = pure,
              Children.modificationQuantity = Identity.Identity . rewriteQuantity pairs,
              Children.modificationPlayer = pure,
              Children.modificationLandSubtype = Identity.Identity . swap Subtype.isLandType,
              -- CR 612.2's other named example: a Turn to Frog's Frog on the stack.
              Children.modificationCreatureSubtype = Identity.Identity . swap Subtype.isCreatureType,
              Children.modificationColors = Identity.Identity . Set.map (TextWord.Engine.colorAfter pairs)
            }

-- rewriteModification's sibling for the other half of a static ability. Under CR
-- 612.1 an ability's affected clause is rules text like any other, so a hacked
-- Kormus Bell animates Islands. Parun.Engine.Children's traversal at Identity:
-- a frozen id set names no word (CR 611.2c), and a permission's duration is
-- left alone.
rewriteAffected :: [(TextWord.TextWord, TextWord.TextWord)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs =
  Identity.runIdentity
    . Children.affectedChildren
      Children.MkAffectedChildren
        { Children.affectedObjects = pure,
          Children.affectedFilter = Identity.Identity . Filter.rewrite pairs,
          Children.affectedDuration = pure
        }

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
          Children.playerDamage = Identity.Identity . rewriteDamagePattern pairs,
          Children.playerKeyword = Identity.Identity . Filter.rewriteKeyword pairs
        }

-- CR 612.1 through a mana payload: Parun.Engine.Children's traversal at
-- Identity, every Filter and Quantity rewritten -- an addition's count, CR
-- 106.7's group, a restriction's spells (CR 106.6), a rider's objects -- and a
-- spend-as-though clause's two halves as written words. A player, a slot and a
-- keyword designator name no word CR 612.2 reaches.
--
-- Proved, each by the scenario named: an addition's count by
-- cr-612-1-a-text-change-naming-the-count-s-word-moves-which-permanents-it, the
-- mana a spend-as-though clause speaks about by
-- cr-609-4b-a-sleighted-celestial-dawn-still-spends-blue-mana-as-any-color. The
-- other children are regression fences: no test observes their rewrite.
rewriteManaAddition :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaAddition.ManaAddition -> ManaAddition.ManaAddition
rewriteManaAddition pairs = Identity.runIdentity . Children.manaAdditionChildren (manaRewrites pairs)

rewriteManaRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> ManaRestriction.ManaRestriction -> ManaRestriction.ManaRestriction
rewriteManaRestriction pairs = Identity.runIdentity . Children.manaRestrictionChildren (manaRewrites pairs)

rewriteSpendManaAsThough :: [(TextWord.TextWord, TextWord.TextWord)] -> SpendManaAsThough.SpendManaAsThough -> SpendManaAsThough.SpendManaAsThough
rewriteSpendManaAsThough pairs = Identity.runIdentity . Children.spendManaAsThoughChildren (manaRewrites pairs)

manaRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.ManaChildren Identity.Identity
manaRewrites pairs =
  Children.manaLeaves
    { Children.manaCriterion = Identity.Identity . Filter.rewrite pairs,
      Children.manaQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.manaSpent = Identity.Identity . rewriteManaFilter pairs,
      Children.manaAsThough = Identity.Identity . rewriteAsThough pairs,
      Children.manaKeyword = Identity.Identity . Filter.rewriteKeyword pairs,
      Children.manaDuration = Identity.Identity . rewriteDuration pairs
    }

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
-- evolved Clavileño; a mana addition's count by rewriteManaAddition's
-- scenario; an entry rider's keys by Parun.CounterspellSpec's Synthetic Warded
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
              Children.onChosenPermanents = Identity.Identity . rewriteChosenPermanents pairs,
              -- CR 612.2: a listed type is a subtype word used as one; no card in data/cards/ lists any in an Effect.ChooseQuality.
              Children.onSubtype = Identity.Identity . TextWord.Engine.subtypeAfter pairs
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

-- The other half of CR 609.4b's clause is written as words too: Sunglasses of
-- Urza's "white mana as though it were red mana".
rewriteAsThough :: [(TextWord.TextWord, TextWord.TextWord)] -> Set.Set ManaType.ManaType -> Set.Set ManaType.ManaType
rewriteAsThough pairs asThough =
  -- CR 609.4b's "mana of any color" (Celestial Dawn) names no colour word,
  -- and a set holding all five can only have been written that way.
  if all (\c -> Set.member (ManaType.Colored c) asThough) [minBound ..]
    then asThough
    else Set.map (rewriteWrittenManaType pairs) asThough

-- CR 612.1 through a damage pattern's two criteria: Parun.Engine.Children's
-- traversal at Identity, every Filter rewritten. Parun.CounterspellSpec's
-- evolved Moonmist proves the source's.
rewriteDamagePattern :: [(TextWord.TextWord, TextWord.TextWord)] -> DamagePattern.DamagePattern -> DamagePattern.DamagePattern
rewriteDamagePattern pairs =
  Identity.runIdentity
    . Children.damagePatternChildren
      Children.MkDamagePatternChildren
        { Children.damageFilter = Identity.Identity . Filter.rewrite pairs,
          Children.damageRecipient = pure,
          Children.damageObject = pure,
          Children.damageSlot = pure
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

-- CR 612.1 through an ObjectRef: Parun.Engine.Children's traversal at
-- Identity, every Filter and Quantity rewritten -- the Filter that says what
-- ends a walk of a library included, and the chosen and random picks' counts.
-- Slots, seats, choosers and zone scopes hold no subtype word.
--
-- Proved by the scenario
-- cr-612-2-a-sleighted-disorder-burns-each-player-who-controls-a-green-creature
-- (Players' Filter). EachCardInYourLibrary's, RandomCard's and
-- RandomCardInGraveyard's Filters are regression fences: no printing in the
-- pool changes the text of an ability that writes one with such a word.
rewriteObjectRef :: [(TextWord.TextWord, TextWord.TextWord)] -> ObjectRef.ObjectRef -> ObjectRef.ObjectRef
rewriteObjectRef pairs = Identity.runIdentity . Children.objectRefChildren (objectRefRewrites pairs)

-- CR 612.1 through a battlefield choice: its Filter and its count are card
-- text, and its chooser names a seat, never a subtype.
rewriteChosenPermanents :: [(TextWord.TextWord, TextWord.TextWord)] -> ChosenPermanents.ChosenPermanents -> ChosenPermanents.ChosenPermanents
rewriteChosenPermanents pairs = Identity.runIdentity . Children.chosenPermanentsChildren (objectRefRewrites pairs)

objectRefRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.ObjectRefChildren Identity.Identity
objectRefRewrites pairs =
  Children.objectRefLeaves
    { Children.refFilter = Identity.Identity . Filter.rewrite pairs,
      Children.refQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.refNested = Identity.Identity . rewriteObjectRef pairs
    }

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

-- CR 613.11's sixteen families under CR 612.1, each at the positions its
-- gatherer module rewrites (Parun.Engine.CombatRestriction and its siblings), so
-- the layer fold and the gatherers agree on where a word is.
rewriteRuleAbilities :: [(TextWord.TextWord, TextWord.TextWord)] -> RuleAbilities.RuleAbilities -> RuleAbilities.RuleAbilities
rewriteRuleAbilities pairs = Identity.runIdentity . Children.ruleAbilitiesChildren (ruleRewrites pairs)

-- CR 612.1 through a combat rule: Parun.Engine.Children's traversal at
-- Identity, every Affected, Filter, Condition, Quantity and Cost rewritten.
-- Parun.Engine.AttackCost.costsOn and Parun.Engine.BlockCost.costsOn apply the
-- costs to the source's own; Parun.Engine.CombatRestriction the restrictions.
--
-- Proved by the scenario
-- cr-612-1-a-hacked-leviathan-can-t-attack-by-sacrificing-islands (a fixed
-- share's Cost). The other children are regression fences: no test observes
-- their rewrite.
rewriteAttackCost :: [(TextWord.TextWord, TextWord.TextWord)] -> AttackCost.AttackCost -> AttackCost.AttackCost
rewriteAttackCost pairs = Identity.runIdentity . Children.attackCostChildren (ruleRewrites pairs)

rewriteBlockCost :: [(TextWord.TextWord, TextWord.TextWord)] -> BlockCost.BlockCost -> BlockCost.BlockCost
rewriteBlockCost pairs = Identity.runIdentity . Children.blockCostChildren (ruleRewrites pairs)

rewriteCombatRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> CombatRestriction.CombatRestriction -> CombatRestriction.CombatRestriction
rewriteCombatRestriction pairs = Identity.runIdentity . Children.combatRestrictionChildren (ruleRewrites pairs)

ruleRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.RuleChildren Identity.Identity
ruleRewrites pairs =
  Children.MkRuleChildren
    { Children.ruleAffected = Identity.Identity . rewriteAffected pairs,
      Children.ruleFilter = Identity.Identity . Filter.rewrite pairs,
      Children.ruleCondition = Identity.Identity . rewriteCondition pairs,
      Children.ruleQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.ruleCost = Identity.Identity . Filter.rewriteCost pairs,
      -- A counter restriction's kind is left as it is.
      Children.ruleCounterKind = pure
    }

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
-- rewriteGrantedAbility, a keyword CR 702.14a's word, an entry option's
-- subtype and each type a choice of a subtype lists CR 205.3's, a counter kind
-- Filter.rewriteCounterKind, entry counters rewriteWithCounters and a copy
-- rewriteAsCopy. The listed types are proved by
-- data/scenarios/text-change's two evolved Dawn-Blessed Pennant scenarios.
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

-- CR 612.1 through a copy or entry payload: Parun.Engine.Children's traversal
-- at Identity, shared by a copy rewrite (CR 614.1c, 614.1e), its CR 707.9
-- exceptions, an entry's riders and a face-down listing (CR 708.2).
--
-- Kept: a slot and a player name no word, and a CR 708.2 listing's power,
-- toughness and keywords are left as written, as before this walk was shared.
-- So are, by Children, an exception's power and toughness (two literals), its
-- card types and supertypes (no text changer's family holds one,
-- Parun.Types.WordFamily), and its name, which CR 612.2's last sentence puts
-- out of reach outright: the effects that change a name are CR 612.7-612.9's,
-- in layer 3 (Modification's AddNamesMatching, SetName and InsertNameWords).
--
-- Proved, each by the scenario named: an entry rider's counter keys by
-- cr-612-1-hacked-zombie-goblin-the-entry-rider-s-counter-is-a-hexproof-from-goblins-counter,
-- the collision merge by cr-122-1-hacked-zombie-skeleton-the-two-colliding-riders-merge
-- and cr-612-2-122-1-hacked-island-swamp-the-two-rows-merge-into-one, a
-- listing's subtypes by cr-612-1-a-hacked-yedora-returns-a-face-down-island-that-taps-for-blue.
--
-- NO BOARD OBSERVES the rest, each a regression fence: CR 707.9a's keywords
-- and quoted ability, CR 707.9b's subtype clauses (Wall of Stolen Identity's
-- "a Wall" would need an Artificial Evolution aimed at it; no test changes
-- Dino DNA's text) and colour clause, CR 707.9e's counters (Altered Ego's
-- amount is CR 601.2b's X and its kind +1/+1, neither holding a word), and a
-- copy's "which permanents", "when you do" and duration.
copyRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.CopyChildren (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) Identity.Identity
copyRewrites pairs =
  Children.MkCopyChildren
    { Children.copyFilter = Identity.Identity . Filter.rewrite pairs,
      Children.copyQuantity = Identity.Identity . rewriteQuantity pairs,
      -- CR 122.1b's keyword counter is the one counter kind holding a word.
      Children.copyCounterKind = Identity.Identity . Filter.rewriteCounterKind pairs,
      Children.copyKeyword = Identity.Identity . Filter.rewriteKeyword pairs,
      -- "A Wall in addition to its other types" is one of the "all instances
      -- of one creature type" Artificial Evolution changes, exactly as a
      -- Filter's HasSubtype atom is (Filter.rewrite).
      Children.copySubtype = Identity.Identity . TextWord.Engine.subtypeAfter pairs,
      -- CR 612.2's colour word used as a colour word ("except it's white").
      Children.copyColor = Identity.Identity . TextWord.Engine.colorAfter pairs,
      Children.copyAbility = Identity.Identity . rewriteGrantedAbility pairs,
      Children.copyEffect = Identity.Identity . rewriteEffect pairs,
      Children.copyCondition = Identity.Identity . rewriteCondition pairs,
      Children.copyDuration = Identity.Identity . rewriteDuration pairs,
      Children.copySlot = pure,
      Children.copyPlayer = pure,
      -- CR 611.2e's effects, ModifyTarget's modification rewrite.
      Children.copyModification = Identity.Identity . rewriteModification pairs,
      Children.copyTypeLine = \typeLine -> Identity.Identity (List.foldl' (\line pair -> maybe line (\(_, _, swapped) -> swapped) (swapSubtype pair line)) typeLine pairs),
      Children.copyPower = pure,
      Children.copyToughness = pure,
      Children.copyListedKeyword = pure
    }

rewriteCopyException :: [(TextWord.TextWord, TextWord.TextWord)] -> CopyException.CopyException (GrantedAbility.GrantedAbility Card.Type.Card) -> CopyException.CopyException (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteCopyException pairs = Identity.runIdentity . Children.copyExceptionChildren (copyRewrites pairs)

rewriteAsCopy :: [(TextWord.TextWord, TextWord.TextWord)] -> AsCopy.AsCopy (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> AsCopy.AsCopy (GrantedAbility.GrantedAbility Card.Type.Card) (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteAsCopy pairs = Identity.runIdentity . Children.asCopyChildren (copyRewrites pairs)

-- The collision merge Children.traverseCounters makes is what CR 122.1 asks
-- for: a hexproof from Islands counter and a hexproof from Swamps counter,
-- hacked Island -> Swamp, are one tally of that one kind.
rewriteWithCounters :: [(TextWord.TextWord, TextWord.TextWord)] -> WithCounters.WithCounters -> WithCounters.WithCounters
rewriteWithCounters pairs = Identity.runIdentity . Children.withCountersChildren (copyRewrites pairs)

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
-- `perEach` takes rewriteQuantity, rewriteEffect's own descent through a counted
-- amount: a gate scaled by "for each Elf you control" counts what a Magical Hack
-- made an Elf.
--
-- `basis` is kept: CR 118.6's described cost is a SLOT and an amount of mana
-- (Parun.Types.CostBasis), and rule 612.1 swaps neither -- the mana cost it
-- describes is read off the projection of the named object as the gate is
-- offered, so a word swap there lands through that object's own rewrite.
rewritePayGate :: [(TextWord.TextWord, TextWord.TextWord)] -> PayGate.PayGate -> PayGate.PayGate
rewritePayGate pairs = Identity.runIdentity . Children.payGateChildren (countRewrites pairs)

-- Parun.Engine.Children's pay gate, counter move, counter removal and library
-- placement traversal at Identity. A payer, an arrival's chooser and a
-- described cost hold no word Parun.Types.ChangeText's pairs swap, so they stay
-- as they are.
countRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.CountChildren Identity.Identity
countRewrites pairs =
  Children.MkCountChildren
    { Children.countQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.countPlayer = pure,
      Children.countKind = Identity.Identity . Filter.rewriteCounterKind pairs,
      Children.countCost = Identity.Identity . Filter.rewriteCost pairs,
      Children.countBasis = pure
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
rewriteTargetSlot pairs = Identity.runIdentity . Children.targetSlotChildren (permissionRewrites pairs)

-- Parun.Engine.Children's target slot, casting permission and activation
-- restriction traversal at Identity. A pool's zones, a chooser, a per-player
-- slot, a turn scope and a history read hold no word Parun.Types.ChangeText's
-- pairs swap, so they stay as they are.
permissionRewrites :: [(TextWord.TextWord, TextWord.TextWord)] -> Children.PermissionChildren Identity.Identity
permissionRewrites pairs =
  Children.MkPermissionChildren
    { Children.permissionFilter = Identity.Identity . Filter.rewrite pairs,
      Children.permissionQuantity = Identity.Identity . rewriteQuantity pairs,
      Children.permissionPlayer = pure,
      Children.permissionZones = pure,
      Children.permissionSlot = pure,
      Children.permissionCondition = Identity.Identity . rewriteCondition pairs,
      Children.permissionCost = Identity.Identity . Filter.rewriteComponent pairs,
      Children.permissionTurn = pure,
      Children.permissionMana = Identity.Identity . rewriteManaSpecification pairs,
      Children.permissionLooksBack = const (pure ())
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

-- CR 612.1 through a casting permission's condition and the costs a graveyard
-- cast pays on top (CR 118.8).
rewriteCastingPermission :: [(TextWord.TextWord, TextWord.TextWord)] -> CastingPermission.CastingPermission -> CastingPermission.CastingPermission
rewriteCastingPermission pairs = Identity.runIdentity . Children.castingPermissionChildren (permissionRewrites pairs)

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
-- ..." rider, which is printed text like the rest of the ability. Only the
-- board condition and SpendOnly's printed colour hold a word -- every other arm
-- names a phase, a turn, the combat record, or a count of activations, and CR
-- 612.1's swap finds nothing in one to act on.
rewriteRestriction :: [(TextWord.TextWord, TextWord.TextWord)] -> ActivationRestriction.ActivationRestriction -> ActivationRestriction.ActivationRestriction
rewriteRestriction pairs = Identity.runIdentity . Children.activationRestrictionChildren (permissionRewrites pairs)

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
rewriteMovedKinds pairs = Identity.runIdentity . Children.movedKindsChildren (countRewrites pairs)

-- CR 612.1 through a RemovalCount: the count's Quantity, where it writes one.
rewriteRemovalCount :: [(TextWord.TextWord, TextWord.TextWord)] -> RemovalCount.RemovalCount -> RemovalCount.RemovalCount
rewriteRemovalCount pairs = Identity.runIdentity . Children.removalCountChildren (countRewrites pairs)

-- CR 612.1 through a LibraryPlacement: the depth a CR 401.7 placement counts,
-- where it states one.
rewritePlacement :: [(TextWord.TextWord, TextWord.TextWord)] -> LibraryPlacement.LibraryPlacement -> LibraryPlacement.LibraryPlacement
rewritePlacement pairs = Identity.runIdentity . Children.libraryPlacementChildren (countRewrites pairs)

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

-- CR 612.1 over an entry row: copyRewrites' traversal. Its counter amounts
-- ("enters with a +1/+1 counter for each Goblin you control" is a Count like
-- any other) and keys merge as rewriteWithCounters' do, CR 303.4f's narrowing
-- filter takes EntersAttachedTo's rewrite and CR 611.2e's effects
-- ModifyTarget's.
rewriteEntryRiders :: [(TextWord.TextWord, TextWord.TextWord)] -> EntryRiders.EntryRiders Quantity.Type.Quantity (GrantedAbility.GrantedAbility Card.Type.Card) -> EntryRiders.EntryRiders Quantity.Type.Quantity (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteEntryRiders pairs = Identity.runIdentity . Children.entryRidersChildren (copyRewrites pairs)

-- CR 612.1 / 612.2 over a CR 708.2 listing's subtypes and quoted abilities, both
-- printed on the card whose effect lists them: copyRewrites' traversal.
rewriteListing :: [(TextWord.TextWord, TextWord.TextWord)] -> FaceDownCharacteristics.FaceDownCharacteristics (GrantedAbility.GrantedAbility Card.Type.Card) -> FaceDownCharacteristics.FaceDownCharacteristics (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteListing pairs = Identity.runIdentity . Children.faceDownChildren (copyRewrites pairs)

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
-- already keeps them as a pair. Proved by
-- cr-612-1-a-magical-hack-makes-nightmare-s-cda-count-islands.
rewriteCharacteristicPT :: [(TextWord.TextWord, TextWord.TextWord)] -> CharacteristicPT.CharacteristicPT -> CharacteristicPT.CharacteristicPT
rewriteCharacteristicPT pairs = Identity.runIdentity . Children.characteristicPTChildren (Identity.Identity . rewriteQuantity pairs)
