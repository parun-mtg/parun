module Parun.Codec.SpecialActionSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.SpecialAction as SpecialAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.SpecialAction as SpecialAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SpecialAction" $ do
  Spec.it s "DiscardThisAnyTime" $
    Common.assertCodec
      s
      (SpecialAction.codec Common.text)
      SpecialAction.DiscardThisAnyTime
      " {\"type\":\"DiscardThisAnyTime\"} "
  -- Leonin Arbiter's {2}, which is the whole of its ignore cost, under the name
  -- its own face gives the ability the payment ignores.
  Spec.it s "IgnoreThisUntilEndOfTurn" $
    Common.assertCodec
      s
      (SpecialAction.codec Common.text)
      ( SpecialAction.IgnoreThisUntilEndOfTurn
          (AbilityName.MkAbilityName (Text.pack "search ban"))
          Cost.MkCost
            { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
              Cost.components = []
            }
      )
      " {\"type\":\"IgnoreThisUntilEndOfTurn\",\"value\":{\"ability\":\"search ban\",\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}}} "
  -- CR 307.5: Talon Gates' sorcery-timed suspend, its effects stood in for.
  Spec.it s "PerformAsASorcery" $
    Common.assertCodec
      s
      (SpecialAction.codec Common.text)
      (SpecialAction.PerformAsASorcery (Seq.singleton (Text.pack "suspend")))
      " {\"type\":\"PerformAsASorcery\",\"value\":[\"suspend\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s (SpecialAction.codec Common.text)
