-- CR 601.2f / 118.7: a cost's mana part with increases and reductions applied,
-- pure and board-free. Split out of Parun.Engine.Cost so the projection can
-- price a keyword granted at a mana cost reduced by an amount
-- (Parun.Engine.Keyword.withCost), Cost being unreachable from there.
module Parun.Engine.CostAdjustments where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Parun.Engine.ManaFilter as ManaFilter
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.AppliedReduction as AppliedReduction
import qualified Parun.Types.CostAdjustments as CostAdjustments
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType

-- CR 118.7: one object's mana cost with an amount taken off it, which is the
-- whole of what "its mana cost reduced by {2}" names. Rule 118.7a-g's spill is
-- `applyAdjustments`', a reduction arriving here being a reduction like any
-- other; an empty amount takes nothing off.
--
-- SHARED by the three provenances that describe a cost this way, so none can
-- drift from the others: CR 702.143d's foretell cost
-- (Parun.Engine.Cost.grantedForetellCost), settled as the cast is proposed; CR
-- 118.6's resolution cost (Parun.Types.CostBasis), settled as
-- Parun.Engine.Resolve offers the gate; and a keyword granted at a mana cost
-- reduced by an amount (Parun.Engine.Keyword.withCost, Aminatou, Veil
-- Piercer's miracle). All stay COSTS rather than becoming CR 601.2f reductions
-- -- grantedForetellCost's haddock is where that is argued.
reducedManaCost :: ManaCost.ManaCost -> ManaCost.ManaCost -> ManaCost.ManaCost
reducedManaCost amount = applyAdjustments (plusReductions [AppliedReduction.MkAppliedReduction amount 0 Nothing] noAdjustments)

-- CR 601.2f with nothing in it: the board's own increases and reductions are the
-- caster's business in Parun.Engine.Cost, and reducedManaCost's amount is the
-- only thing its callers apply.
noAdjustments :: CostAdjustments.CostAdjustments
noAdjustments =
  CostAdjustments.MkCostAdjustments
    { CostAdjustments.increases = [],
      CostAdjustments.reductions = [],
      CostAdjustments.components = []
    }

-- CR 601.2f: the reductions a CANDIDATE COST brings with it
-- (Parun.Types.CandidateCost's @reductions@) folded into the adjustments the board
-- states -- rule 702.119a's, rule 702.180a's and rule 702.48a's, each unfloored
-- and unconfined since none states a restriction, and a paid optional addition's
-- (Defiler of Dreams), confined as its card says.
plusReductions :: [AppliedReduction.AppliedReduction] -> CostAdjustments.CostAdjustments -> CostAdjustments.CostAdjustments
plusReductions extra adjustments =
  adjustments {CostAdjustments.reductions = CostAdjustments.reductions adjustments <> extra}

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the cost, its generic part to the generic
--    component and its typed symbols after the printed ones (CR 601.2f's order,
--    and Thalia's own ruling: increases first, then reductions).
-- 2. A REDUCTION is an amount of mana. Its GENERIC part comes off the generic
--    component only (CR 118.7a), floored at zero; its TYPED part cancels
--    matching typed symbols one for one (Edgewalker). CR 118.7f puts a PHYREXIAN
--    symbol on the typed side, and CR 118.7g sends a SNOW symbol the other way,
--    where CR 107.4h keeps an {S} in a COST out of the generic component -- so
--    each question below is asked by two functions, one per SIDE.
-- 3. An EXCESS typed symbol -- one the cost has no matching symbol for -- comes
--    off the GENERIC component instead, one generic mana per symbol (CR
--    118.7b-d). A reduction whose own text confines it to the coloured mana paid
--    DROPS the excess rather than spilling it, which is card text CR 101.1 lets
--    override the rules: Edgewalker's reminder text makes a {1}{W} Cleric spell
--    cost {1}, not {0}.
-- 4. CR 601.2f's floor at {0} needs no special case: the empty list IS {0}.
-- 5. A REDUCING EFFECT'S OWN FLOOR is applied as that reduction lands, never as
--    a clamp on the pooled result -- Heartstone's sentence says THIS EFFECT (CR
--    101.1), and Heartstone beside Blossoming Tortoise on an animated Mutavault's
--    {1} tells the two readings apart at {0} against {1}. The shortfall is
--    GENERIC mana, and it NEVER RAISES a cost already below the floor.
--
-- Reductions are FOLDED one at a time, in the LIST's own order, which is CR
-- 601.2f's order as the PAYER chose it: Parun.Engine.Cost.announceReductions
-- reorders them to the answer it got before this ever runs, and nothing here
-- sorts. A caller that reaches this without that seam is asking a different
-- question -- the GATE does, through Parun.Engine.Cost.totalManas, and
-- enumerates the orders itself rather than picking one.
--
-- Every step's result is CANONICAL: one leading Generic symbol carrying the whole
-- generic component (omitted at zero), then the SURVIVING printed typed symbols
-- in their original order -- presentation, but it is what the next reduction in
-- the fold reads.
applyAdjustments :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> ManaCost.ManaCost
applyAdjustments adjustments cost =
  let increases = CostAdjustments.increases adjustments
      reductions = CostAdjustments.reductions adjustments
      costGenericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated
        -- type, so it is no part of the generic component.
        ManaSymbol.Hybrid {} -> 0
        -- A monocolored hybrid's {2} half IS generic mana once CR 601.2b's
        -- nonhybrid equivalent names it, and a symbol still spelled {2/R} is one
        -- CR 601.2b has NOT named -- Flame Javelin's own ruling. Every road into
        -- this function now totals a cost CR 601.2b has already completed, so the
        -- arm is a total function's and not a live case.
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 107.4f makes this a COLOURED symbol whose other half is life.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 107.4f's hybrid Phyrexian symbol is coloured twice over and generic
        -- not at all: neither of its three ways is generic mana.
        ManaSymbol.HybridPhyrexian _ -> 0
        -- CR 107.4h: generic reductions don't affect {S} costs, which is why it
        -- is not spelled Generic 1. The one arm where this function and
        -- reducingGenericOf part company; the Adjustments case "CR 107.4h a
        -- generic reduction does not affect an {S} in the cost" proves this side.
        ManaSymbol.Snow -> 0
        -- {X} is no part of the generic component: CR 107.3 makes it a value a
        -- player announces, and CR 601.2b's announcement precedes rule 601.2f, so
        -- a cost being TOTALLED never carries one. LIVE rather than unreachable
        -- all the same -- grantedForetellCost applies rule 702.143d's reduction
        -- to a PRINTED mana cost ahead of any announcement, so an {X} card an
        -- effect foretold (Blaze) carries its symbol through here untouched and
        -- announces it at the cast.
        ManaSymbol.Variable -> 0
      -- The REDUCTION's generic amount, two functions rather than one because CR
      -- 118.7g makes the two sides read an {S} differently. Every other arm
      -- agrees with costGenericOf's, and agreeing is not sharing.
      reducingGenericOf symbol = case symbol of
        -- CR 118.7a's amount of generic mana, which is what this side is.
        ManaSymbol.Generic n -> n
        -- The typed side reads an OfType, and CR 118.7b-d spill what it strands
        -- back here -- point 3 above, counted after the cancellation and not by
        -- this function, which cannot see the cost.
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e's colour/colour hybrid has no generic half at all, and a
        -- symbol still spelled {2/R} is one CR 118.7e's choice has not been made
        -- for -- announceReductions leaves a Generic behind when the {2} half is
        -- taken, and the gate enumerates the same halves.
        ManaSymbol.Hybrid {} -> 0
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 118.7f gives a Phyrexian reduction to the typed side whole.
        ManaSymbol.Phyrexian _ -> 0
        -- Neither of its halves is generic mana (Mana.hybridHalvesOf).
        ManaSymbol.HybridPhyrexian _ -> 0
        -- CR 118.7g: a snow-symbol reduction is that much GENERIC mana. THE arm
        -- this side exists for; CR 107.4h is about the other side.
        ManaSymbol.Snow -> 1
        -- {X} again, costGenericOf's Variable arm and for its reason.
        ManaSymbol.Variable -> 0
      -- "Typed" here means "not generic": everything but Generic survives and
      -- keeps its printed position, which is the only way an unreducible symbol
      -- reaches Mana.spend intact.
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        ManaSymbol.Hybrid {} -> True
        ManaSymbol.MonocoloredHybrid _ -> True
        ManaSymbol.Phyrexian _ -> True
        ManaSymbol.HybridPhyrexian _ -> True
        ManaSymbol.Snow -> True
        ManaSymbol.Variable -> True
      -- The two SIDES of the cancellation, two functions because CR 118.7f makes
      -- them disagree: which one mana type a printed COST symbol offers up, and
      -- which one a REDUCTION's symbol takes away. Nothing can be shared -- {G/P}
      -- names green when a reduction says it and nothing when a cost does, and
      -- Parun.PlayerEffectSpec's SyntheticPhyrexianDiscount group proves both
      -- halves against cards.
      costManaTypeOf symbol = case symbol of
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 107.4e names TWO types, and a symbol still spelled {G/U} here is one
        -- CR 601.2b has not named -- Mana.announce leaves an OfType behind when
        -- it does. Every road into this function now totals a cost CR 601.2b has
        -- already completed, so the arm is a total function's and not a live case.
        ManaSymbol.Hybrid {} -> Nothing
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- EXACT rather than an elision: the symbol is necessarily UNANNOUNCED, CR
        -- 601.2b's announcement leaving behind either an OfType or a payment of
        -- life, so no caller reaching this arm has established that there is a
        -- green mana here to cancel. Edgewalker's ruling says so outright.
        ManaSymbol.Phyrexian _ -> Nothing
        -- Nothing, the arm above's reason twice over: unannounced, and naming
        -- two colours rather than one even once it is.
        ManaSymbol.HybridPhyrexian _ -> Nothing
        -- CR 107.4h: {S} is paid with one mana of ANY type, so it names none, and
        -- a reduction of one white mana cannot single it out.
        ManaSymbol.Snow -> Nothing
        -- {X} again, costGenericOf's Variable arm; {X} names no mana type.
        ManaSymbol.Variable -> Nothing
      reducingManaTypeOf symbol = case symbol of
        -- CR 118.7a's half, which reducingGenericOf above already counted.
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 118.7e: the choice of half belongs to the PLAYER PAYING, so
        -- answering it here would be the engine making it. A symbol still spelled
        -- {W/U} or {2/R} is one nobody has been asked about; announceReductions
        -- leaves the chosen half's symbol behind when they have.
        ManaSymbol.Hybrid {} -> Nothing
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- CR 118.7f: reduced by one mana of that symbol's colour. The one arm
        -- where the two sides part company -- unlike CR 118.7e's hybrid this asks
        -- the player nothing, the symbol naming exactly one colour.
        ManaSymbol.Phyrexian color -> Just (ManaType.Colored color)
        -- The hybrid arms' reason: its two colours are halves the payer chooses
        -- between (Mana.hybridHalvesOf), so one still spelled {G/U/P} is unasked.
        ManaSymbol.HybridPhyrexian _ -> Nothing
        -- CR 118.7g makes an {S} reduction GENERIC mana, so reducingGenericOf's
        -- Snow arm is where it lands.
        ManaSymbol.Snow -> Nothing
        -- {X} again, costGenericOf's Variable arm and for its reason.
        ManaSymbol.Variable -> Nothing
      -- The canonical form point 5's header describes: the generic component as
      -- one leading symbol, then the typed symbols left.
      canonical generic typed = ManaCost.MkManaCost ((if generic == 0 then [] else [ManaSymbol.Generic generic]) <> typed)
      -- Point 1: every increase, before any reduction -- Derelor's {B} as a
      -- typed symbol beside the printed ones, Thalia's {1} onto the generic
      -- component.
      raise (ManaCost.MkManaCost printed) =
        let symbols = printed <> concatMap ManaCost.unwrap increases
         in canonical (sum (fmap costGenericOf symbols)) (filter isTyped symbols)
      -- ONE reduction, with the floor and the coloured-mana confinement its own
      -- effect states.
      reduce (ManaCost.MkManaCost symbols) reduction =
        let reducingSymbols = ManaCost.unwrap (AppliedReduction.amount reduction)
            floor_ = AppliedReduction.atLeast reduction
            generic = sum (fmap costGenericOf symbols)
            typed = filter isTyped symbols
            -- A confinement admits only the typed reduction of mana it names:
            -- Defiler of Dreams with "blue" rewritten to "red" takes {U} off
            -- nothing (CR 612.2).
            --
            -- Unprinted: a generic reduction beside a confinement, which comes
            -- off the generic component as ever.
            admitted manaType = maybe True (`ManaFilter.matchesType` manaType) (AppliedReduction.reducesOnly reduction)
            (survivors, unspent) = cancel (filter admitted (Maybe.mapMaybe reducingManaTypeOf reducingSymbols)) typed
            -- Point 3: the typed reduction the cost had nothing to give to comes
            -- off the GENERIC component instead, one generic mana per stranded
            -- symbol (CR 118.7b-d), unless this effect's own text confines it to
            -- the mana it names (Edgewalker, CR 101.1).
            spilled = if Maybe.isJust (AppliedReduction.reducesOnly reduction) then 0 else Natural.length unspent
            taken = sum (fmap reducingGenericOf reducingSymbols) + spilled
            -- Natural subtraction is PARTIAL, so CR 601.2f's floor is also what
            -- keeps this total.
            lowered = if generic >= taken then generic - taken else 0
            -- Point 5 above. Every typed symbol is at least one mana (CR
            -- 107.4e/107.4f/107.4h), so the mana left in the cost is `lowered`
            -- plus how many survivors there are.
            typedCount = Natural.length survivors
            required = min floor_ (generic + Natural.length typed)
            floored = if lowered + typedCount >= required then lowered else required - typedCount
         in canonical floored survivors
      -- Each reducing symbol cancels ONE matching symbol in the cost, walking the
      -- printed order so the survivors keep it. `unspent` is the bag of reducing
      -- types that have not found a match yet; the survivors come back beside
      -- whatever is left of it when the walk ends, which is point 3's excess.
      cancel unspent remaining = case remaining of
        [] -> ([], unspent)
        symbol : rest -> case costManaTypeOf symbol of
          Just manaType | elem manaType unspent -> cancel (List.delete manaType unspent) rest
          _ -> let (survivors, left) = cancel unspent rest in (symbol : survivors, left)
   in List.foldl' reduce (raise cost) reductions
