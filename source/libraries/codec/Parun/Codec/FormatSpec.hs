module Parun.Codec.FormatSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.Format as Format
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DraftBooster as DraftBooster
import qualified Parun.Types.Format as Format
import qualified Parun.Types.RuleFamily as RuleFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Format" $ do
  Spec.it s "a pool" $
    Common.assertCodec s Format.codec (Format.MkFormat RuleFamily.Constructed (Just (Text.pack "vintage")) Set.empty) " {\"rules\":\"Constructed\",\"pool\":\"vintage\"} "
  Spec.it s "no pool" $
    Common.assertCodec s Format.codec (Format.MkFormat RuleFamily.Limited Nothing Set.empty) " {\"rules\":\"Limited\"} "
  Spec.it s "boosters" $
    Common.assertCodec s Format.codec (Format.MkFormat RuleFamily.CommanderDraft Nothing (Set.singleton DraftBooster.CommanderMasters)) " {\"rules\":\"CommanderDraft\",\"boosters\":[\"CommanderMasters\"]} "
