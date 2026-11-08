module Parun.Codec.CommanderDeckRule where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule

codec :: Codec.Codec CommanderDeckRule.CommanderDeckRule
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "NoMaximumDeckSize" CommanderDeckRule.NoMaximumDeckSize,
      Arm.payload "AnyColorIdentity" (Filter.codec Keyword.codec) CommanderDeckRule.AnyColorIdentity (\x -> case x of CommanderDeckRule.AnyColorIdentity y -> Just y; _ -> Nothing),
      Arm.payload "ChosenExtraColor" (Filter.codec Keyword.codec) CommanderDeckRule.ChosenExtraColor (\x -> case x of CommanderDeckRule.ChosenExtraColor y -> Just y; _ -> Nothing)
    ]

tagOf :: CommanderDeckRule.CommanderDeckRule -> String
tagOf x = case x of
  CommanderDeckRule.NoMaximumDeckSize -> "NoMaximumDeckSize"
  CommanderDeckRule.AnyColorIdentity {} -> "AnyColorIdentity"
  CommanderDeckRule.ChosenExtraColor {} -> "ChosenExtraColor"
