{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ActivationCriteria where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.KeywordDesignator as KeywordDesignator
import qualified Parun.Codec.LoyaltyKind as LoyaltyKind
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActivationCriteria as ActivationCriteria

-- | A bare object keyed by the record's field names, every one DEFAULTED to
-- Nothing -- every activated ability of a matching source, which is what most
-- printings say -- so a card file writes only the criteria it prints.
--
-- The keyword codec is a PARAMETER for Parun.Codec.Filter's reason.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ActivationCriteria.ActivationCriteria keyword)
codec keywordCodec = Fields.object $ do
  grantedBy <- Fields.defaulted "grantedBy" Nothing (Common.maybe (KeywordDesignator.codec keywordCodec)) ActivationCriteria.grantedBy
  whichKind <- Fields.defaulted "whichKind" Nothing (Common.maybe AbilityKind.codec) ActivationCriteria.whichKind
  whichLoyalty <- Fields.defaulted "whichLoyalty" Nothing (Common.maybe LoyaltyKind.codec) ActivationCriteria.whichLoyalty
  whichAbility <- Fields.defaulted "whichAbility" Nothing (Common.maybe AbilityName.codec) ActivationCriteria.whichAbility
  pure ActivationCriteria.MkActivationCriteria {ActivationCriteria.grantedBy = grantedBy, ActivationCriteria.whichKind = whichKind, ActivationCriteria.whichLoyalty = whichLoyalty, ActivationCriteria.whichAbility = whichAbility}
