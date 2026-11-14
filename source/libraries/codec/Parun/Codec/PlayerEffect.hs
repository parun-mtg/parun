module Parun.Codec.PlayerEffect where

import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.AlternativeActivationCost as AlternativeActivationCost
import qualified Parun.Codec.CantSearchLibraries as CantSearchLibraries
import qualified Parun.Codec.CastFromZone as CastFromZone
import qualified Parun.Codec.CombatDeclaration as CombatDeclaration
import qualified Parun.Codec.CostModifier as CostModifier
import qualified Parun.Codec.CounterProhibition as CounterProhibition
import qualified Parun.Codec.DamagePattern as DamagePattern
import qualified Parun.Codec.DrawReveal as DrawReveal
import qualified Parun.Codec.EntriesTrigger as EntriesTrigger
import qualified Parun.Codec.ExtraVotes as ExtraVotes
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.ForbiddenAct as ForbiddenAct
import qualified Parun.Codec.FreeCast as FreeCast
import qualified Parun.Codec.GameOutcome as GameOutcome
import qualified Parun.Codec.HexproofExemption as HexproofExemption
import qualified Parun.Codec.InZone as InZone
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordDesignator as KeywordDesignator
import qualified Parun.Codec.KeywordRiderGrant as KeywordRiderGrant
import qualified Parun.Codec.LandAllowance as LandAllowance
import qualified Parun.Codec.LoyaltyGrant as LoyaltyGrant
import qualified Parun.Codec.ManaFilter as ManaFilter
import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.Codec.ModifiedRoll as ModifiedRoll
import qualified Parun.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Codec.PlayerScope as PlayerScope
import qualified Parun.Codec.PlotFromZone as PlotFromZone
import qualified Parun.Codec.Shows as Shows
import qualified Parun.Codec.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Codec.StateBasedAction as StateBasedAction
import qualified Parun.Codec.StatedFlip as StatedFlip
import qualified Parun.Codec.TapForMana as TapForMana
import qualified Parun.Codec.TargetRestriction as TargetRestriction
import qualified Parun.Codec.TurnScope as TurnScope
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.PlayerEffect as PlayerEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec PlayerEffect.PlayerEffect
codec =
  let filterCodec = Filter.codec Keyword.codec
      -- The designator absent is every ability; the source Filter absent, the
      -- empty And, every source; the kind absent, both kinds.
      activationBan =
        Fields.object $
          (,,)
            <$> Fields.defaulted "designator" Nothing (Common.maybe (KeywordDesignator.codec Keyword.codec)) (\(x, _, _) -> x)
            <*> Fields.defaulted "sources" (Filter.Type.And []) filterCodec (\(_, x, _) -> x)
            <*> Fields.defaulted "kind" Nothing (Common.maybe AbilityKind.codec) (\(_, _, x) -> x)
      -- The type absent is mana kept as it is (Upwelling).
      unspentMana =
        Fields.object $
          (,)
            <$> Fields.required "mana" ManaFilter.codec fst
            <*> Fields.defaulted "becomes" Nothing (Common.maybe ManaType.codec) snd
      lifeForMana =
        Fields.object $
          (,)
            <$> Fields.required "life" Common.natural fst
            <*> Fields.required "mana" ManaType.codec snd
      -- The scope absent is every matching spell, not only a turn's first.
      flashGrant =
        Fields.object $
          (,)
            <$> Fields.required "matching" filterCodec fst
            <*> Fields.defaulted "onlyFirst" Nothing (Common.maybe TurnScope.codec) snd
      -- The keywords absent are none: the next spell gains nothing (CR 611.2f).
      nextSpell =
        Fields.object $
          (,)
            <$> Fields.required "matching" filterCodec fst
            <*> Fields.defaulted "keywords" [] (Common.list Keyword.codec) snd
      -- The filter absent is every permanent the player controls.
      sbaExemption =
        Fields.object $
          (,)
            <$> Fields.required "action" StateBasedAction.codec fst
            <*> Fields.defaulted "matching" (Filter.Type.And []) filterCodec snd
      -- An empty pattern is all damage (Everlasting Torment).
      asThough =
        Fields.object $
          (,)
            <$> Fields.required "matching" DamagePattern.codec fst
            <*> Fields.required "keyword" Keyword.codec snd
      -- The quality absent is every spell and ability the scope's players control.
      targetingBan =
        Fields.object $
          (,)
            <$> Fields.required "scope" PlayerScope.codec fst
            <*> Fields.defaulted "quality" Nothing (Common.maybe filterCodec) snd
   in Arm.tagged
        tagOf
        [ Arm.nullary "CantCastSpells" PlayerEffect.CantCastSpells,
          Arm.payload "CantActivateAbilities" activationBan (\(y, z, w) -> PlayerEffect.CantActivateAbilities y z w) (\x -> case x of PlayerEffect.CantActivateAbilities y z w -> Just (y, z, w); _ -> Nothing),
          Arm.payload "CantCastMoreThan" Common.natural PlayerEffect.CantCastMoreThan (\x -> case x of PlayerEffect.CantCastMoreThan y -> Just y; _ -> Nothing),
          Arm.payload "ModifyCost" CostModifier.codec PlayerEffect.ModifyCost (\x -> case x of PlayerEffect.ModifyCost y -> Just y; _ -> Nothing),
          Arm.payload "AlternativeActivationCost" AlternativeActivationCost.codec PlayerEffect.AlternativeActivationCost (\x -> case x of PlayerEffect.AlternativeActivationCost y -> Just y; _ -> Nothing),
          Arm.payload "PlayAdditionalLands" LandAllowance.codec PlayerEffect.PlayAdditionalLands (\x -> case x of PlayerEffect.PlayAdditionalLands y -> Just y; _ -> Nothing),
          Arm.nullary "NoMaximumHandSize" PlayerEffect.NoMaximumHandSize,
          Arm.payload "SetMaximumHandSize" Common.natural PlayerEffect.SetMaximumHandSize (\x -> case x of PlayerEffect.SetMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "IncreaseMaximumHandSize" Common.natural PlayerEffect.IncreaseMaximumHandSize (\x -> case x of PlayerEffect.IncreaseMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "ReduceMaximumHandSize" Common.natural PlayerEffect.ReduceMaximumHandSize (\x -> case x of PlayerEffect.ReduceMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "IncreaseDevotion" Common.natural PlayerEffect.IncreaseDevotion (\x -> case x of PlayerEffect.IncreaseDevotion y -> Just y; _ -> Nothing),
          Arm.payload "DontLoseUnspentMana" unspentMana (uncurry PlayerEffect.DontLoseUnspentMana) (\x -> case x of PlayerEffect.DontLoseUnspentMana y z -> Just (y, z); _ -> Nothing),
          Arm.nullary "LoseLifeForUnspentMana" PlayerEffect.LoseLifeForUnspentMana,
          Arm.payload "MayPayLifeForMana" lifeForMana (uncurry PlayerEffect.MayPayLifeForMana) (\x -> case x of PlayerEffect.MayPayLifeForMana life mana -> Just (life, mana); _ -> Nothing),
          Arm.payload "SpendManaAsThough" SpendManaAsThough.codec PlayerEffect.SpendManaAsThough (\x -> case x of PlayerEffect.SpendManaAsThough y -> Just y; _ -> Nothing),
          Arm.payload "CantBeTargetedBy" targetingBan (uncurry PlayerEffect.CantBeTargetedBy) (\x -> case x of PlayerEffect.CantBeTargetedBy y z -> Just (y, z); _ -> Nothing),
          Arm.payload "CastAsThoughItHadFlash" flashGrant (uncurry PlayerEffect.CastAsThoughItHadFlash) (\x -> case x of PlayerEffect.CastAsThoughItHadFlash y z -> Just (y, z); _ -> Nothing),
          Arm.payload "MayPlayAsThoughItHadFlash" filterCodec PlayerEffect.MayPlayAsThoughItHadFlash (\x -> case x of PlayerEffect.MayPlayAsThoughItHadFlash y -> Just y; _ -> Nothing),
          Arm.payload "KeywordRiderGrant" KeywordRiderGrant.codec PlayerEffect.KeywordRiderGrant (\x -> case x of PlayerEffect.KeywordRiderGrant y -> Just y; _ -> Nothing),
          Arm.payload "LoyaltyGrant" LoyaltyGrant.codec PlayerEffect.LoyaltyGrant (\x -> case x of PlayerEffect.LoyaltyGrant y -> Just y; _ -> Nothing),
          Arm.payload "MayTapForMana" TapForMana.codec PlayerEffect.MayTapForMana (\x -> case x of PlayerEffect.MayTapForMana y -> Just y; _ -> Nothing),
          Arm.payload "CantBeCountered" CounterProhibition.codec PlayerEffect.CantBeCountered (\x -> case x of PlayerEffect.CantBeCountered y -> Just y; _ -> Nothing),
          Arm.payload "NextSpell" nextSpell (uncurry PlayerEffect.NextSpell) (\x -> case x of PlayerEffect.NextSpell y z -> Just (y, z); _ -> Nothing),
          Arm.payload "RestrictTargeting" (TargetRestriction.codec filterCodec) PlayerEffect.RestrictTargeting (\x -> case x of PlayerEffect.RestrictTargeting y -> Just y; _ -> Nothing),
          Arm.payload "TargetPastHexproof" HexproofExemption.codec PlayerEffect.TargetPastHexproof (\x -> case x of PlayerEffect.TargetPastHexproof y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBePrevented" DamagePattern.codec PlayerEffect.DamageCantBePrevented (\x -> case x of PlayerEffect.DamageCantBePrevented y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBeRedirected" DamagePattern.codec PlayerEffect.DamageCantBeRedirected (\x -> case x of PlayerEffect.DamageCantBeRedirected y -> Just y; _ -> Nothing),
          Arm.payload "DamageAsThoughSourceHad" asThough (uncurry PlayerEffect.DamageAsThoughSourceHad) (\x -> case x of PlayerEffect.DamageAsThoughSourceHad y z -> Just (y, z); _ -> Nothing),
          Arm.payload "CantSearchLibraries" CantSearchLibraries.codec PlayerEffect.CantSearchLibraries (\x -> case x of PlayerEffect.CantSearchLibraries y -> Just y; _ -> Nothing),
          Arm.payload "SearchesTopOfLibraries" Common.natural PlayerEffect.SearchesTopOfLibraries (\x -> case x of PlayerEffect.SearchesTopOfLibraries y -> Just y; _ -> Nothing),
          Arm.payload "HasProtectionFrom" filterCodec PlayerEffect.HasProtectionFrom (\x -> case x of PlayerEffect.HasProtectionFrom y -> Just y; _ -> Nothing),
          Arm.payload "StateBasedActionDoesntApply" sbaExemption (uncurry PlayerEffect.StateBasedActionDoesntApply) (\x -> case x of PlayerEffect.StateBasedActionDoesntApply y z -> Just (y, z); _ -> Nothing),
          Arm.nullary "CantSetSchemesInMotion" PlayerEffect.CantSetSchemesInMotion,
          Arm.payload "EntriesTrigger" EntriesTrigger.codec PlayerEffect.EntriesTrigger (\x -> case x of PlayerEffect.EntriesTrigger y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalTriggers" filterCodec PlayerEffect.AdditionalTriggers (\x -> case x of PlayerEffect.AdditionalTriggers y -> Just y; _ -> Nothing),
          Arm.nullary "ControlledWhileSearching" PlayerEffect.ControlledWhileSearching,
          Arm.nullary "BlocksFromPiles" PlayerEffect.BlocksFromPiles,
          Arm.payload "YouChoose" CombatDeclaration.codec PlayerEffect.YouChoose (\x -> case x of PlayerEffect.YouChoose y -> Just y; _ -> Nothing),
          Arm.nullary "ExilesFoundWhileSearching" PlayerEffect.ExilesFoundWhileSearching,
          Arm.nullary "CantAttackWithCreatures" PlayerEffect.CantAttackWithCreatures,
          Arm.payload "CantCastMatching" filterCodec PlayerEffect.CantCastMatching (\x -> case x of PlayerEffect.CantCastMatching y -> Just y; _ -> Nothing),
          Arm.nullary "CastOnlyAtSorcerySpeed" PlayerEffect.CastOnlyAtSorcerySpeed,
          Arm.payload "CantPlayLands" filterCodec PlayerEffect.CantPlayLands (\x -> case x of PlayerEffect.CantPlayLands y -> Just y; _ -> Nothing),
          Arm.payload "CastFrom" CastFromZone.codec PlayerEffect.CastFrom (\x -> case x of PlayerEffect.CastFrom y -> Just y; _ -> Nothing),
          Arm.payload "PlayLandsFrom" InZone.codec PlayerEffect.PlayLandsFrom (\x -> case x of PlayerEffect.PlayLandsFrom y -> Just y; _ -> Nothing),
          Arm.payload "PlotFrom" PlotFromZone.codec PlayerEffect.PlotFrom (\x -> case x of PlayerEffect.PlotFrom y -> Just y; _ -> Nothing),
          Arm.payload "CastWithoutPayingManaCost" FreeCast.codec PlayerEffect.CastWithoutPayingManaCost (\x -> case x of PlayerEffect.CastWithoutPayingManaCost y -> Just y; _ -> Nothing),
          Arm.payload "CantGetCounters" (Common.maybe PlayerCounterKind.codec) PlayerEffect.CantGetCounters (\x -> case x of PlayerEffect.CantGetCounters y -> Just y; _ -> Nothing),
          Arm.payload "StateCoinFlip" StatedFlip.codec PlayerEffect.StateCoinFlip (\x -> case x of PlayerEffect.StateCoinFlip y -> Just y; _ -> Nothing),
          Arm.payload "ModifyDieRoll" ModifiedRoll.codec PlayerEffect.ModifyDieRoll (\x -> case x of PlayerEffect.ModifyDieRoll y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalVotes" ExtraVotes.codec PlayerEffect.AdditionalVotes (\x -> case x of PlayerEffect.AdditionalVotes y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalSurveilCards" Common.natural PlayerEffect.AdditionalSurveilCards (\x -> case x of PlayerEffect.AdditionalSurveilCards y -> Just y; _ -> Nothing),
          Arm.payload "PutTappedAsYouCascade" filterCodec PlayerEffect.PutTappedAsYouCascade (\x -> case x of PlayerEffect.PutTappedAsYouCascade y -> Just y; _ -> Nothing),
          Arm.payload "Cant" ForbiddenAct.codec PlayerEffect.Cant (\x -> case x of PlayerEffect.Cant y -> Just y; _ -> Nothing),
          Arm.payload "Precluded" GameOutcome.codec PlayerEffect.Precluded (\x -> case x of PlayerEffect.Precluded y -> Just y; _ -> Nothing),
          Arm.payload "Shows" Shows.codec PlayerEffect.Shows (\x -> case x of PlayerEffect.Shows y -> Just y; _ -> Nothing),
          Arm.payload "RevealsDraw" DrawReveal.codec PlayerEffect.RevealsDraw (\x -> case x of PlayerEffect.RevealsDraw y -> Just y; _ -> Nothing)
        ]

tagOf :: PlayerEffect.PlayerEffect -> String
tagOf x = case x of
  PlayerEffect.CantCastSpells {} -> "CantCastSpells"
  PlayerEffect.CantActivateAbilities {} -> "CantActivateAbilities"
  PlayerEffect.CantCastMoreThan {} -> "CantCastMoreThan"
  PlayerEffect.ModifyCost {} -> "ModifyCost"
  PlayerEffect.AlternativeActivationCost {} -> "AlternativeActivationCost"
  PlayerEffect.PlayAdditionalLands {} -> "PlayAdditionalLands"
  PlayerEffect.NoMaximumHandSize {} -> "NoMaximumHandSize"
  PlayerEffect.SetMaximumHandSize {} -> "SetMaximumHandSize"
  PlayerEffect.IncreaseMaximumHandSize {} -> "IncreaseMaximumHandSize"
  PlayerEffect.ReduceMaximumHandSize {} -> "ReduceMaximumHandSize"
  PlayerEffect.IncreaseDevotion {} -> "IncreaseDevotion"
  PlayerEffect.DontLoseUnspentMana {} -> "DontLoseUnspentMana"
  PlayerEffect.LoseLifeForUnspentMana {} -> "LoseLifeForUnspentMana"
  PlayerEffect.MayPayLifeForMana {} -> "MayPayLifeForMana"
  PlayerEffect.SpendManaAsThough {} -> "SpendManaAsThough"
  PlayerEffect.CantBeTargetedBy {} -> "CantBeTargetedBy"
  PlayerEffect.CastAsThoughItHadFlash {} -> "CastAsThoughItHadFlash"
  PlayerEffect.MayPlayAsThoughItHadFlash {} -> "MayPlayAsThoughItHadFlash"
  PlayerEffect.KeywordRiderGrant {} -> "KeywordRiderGrant"
  PlayerEffect.LoyaltyGrant {} -> "LoyaltyGrant"
  PlayerEffect.MayTapForMana {} -> "MayTapForMana"
  PlayerEffect.CantBeCountered {} -> "CantBeCountered"
  PlayerEffect.NextSpell {} -> "NextSpell"
  PlayerEffect.RestrictTargeting {} -> "RestrictTargeting"
  PlayerEffect.TargetPastHexproof {} -> "TargetPastHexproof"
  PlayerEffect.DamageCantBePrevented {} -> "DamageCantBePrevented"
  PlayerEffect.DamageCantBeRedirected {} -> "DamageCantBeRedirected"
  PlayerEffect.DamageAsThoughSourceHad {} -> "DamageAsThoughSourceHad"
  PlayerEffect.CantSearchLibraries {} -> "CantSearchLibraries"
  PlayerEffect.SearchesTopOfLibraries {} -> "SearchesTopOfLibraries"
  PlayerEffect.HasProtectionFrom {} -> "HasProtectionFrom"
  PlayerEffect.StateBasedActionDoesntApply {} -> "StateBasedActionDoesntApply"
  PlayerEffect.CantSetSchemesInMotion {} -> "CantSetSchemesInMotion"
  PlayerEffect.EntriesTrigger {} -> "EntriesTrigger"
  PlayerEffect.AdditionalTriggers {} -> "AdditionalTriggers"
  PlayerEffect.ControlledWhileSearching {} -> "ControlledWhileSearching"
  PlayerEffect.BlocksFromPiles {} -> "BlocksFromPiles"
  PlayerEffect.YouChoose {} -> "YouChoose"
  PlayerEffect.ExilesFoundWhileSearching {} -> "ExilesFoundWhileSearching"
  PlayerEffect.CantAttackWithCreatures {} -> "CantAttackWithCreatures"
  PlayerEffect.CantCastMatching {} -> "CantCastMatching"
  PlayerEffect.PutTappedAsYouCascade {} -> "PutTappedAsYouCascade"
  PlayerEffect.CastOnlyAtSorcerySpeed {} -> "CastOnlyAtSorcerySpeed"
  PlayerEffect.CantPlayLands {} -> "CantPlayLands"
  PlayerEffect.CastFrom {} -> "CastFrom"
  PlayerEffect.PlayLandsFrom {} -> "PlayLandsFrom"
  PlayerEffect.PlotFrom {} -> "PlotFrom"
  PlayerEffect.CastWithoutPayingManaCost {} -> "CastWithoutPayingManaCost"
  PlayerEffect.CantGetCounters {} -> "CantGetCounters"
  PlayerEffect.StateCoinFlip {} -> "StateCoinFlip"
  PlayerEffect.ModifyDieRoll {} -> "ModifyDieRoll"
  PlayerEffect.AdditionalVotes {} -> "AdditionalVotes"
  PlayerEffect.AdditionalSurveilCards {} -> "AdditionalSurveilCards"
  PlayerEffect.Cant {} -> "Cant"
  PlayerEffect.Precluded {} -> "Precluded"
  PlayerEffect.Shows {} -> "Shows"
  PlayerEffect.RevealsDraw {} -> "RevealsDraw"
