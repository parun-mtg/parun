{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChooseInstead where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Condition as Condition
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChooseInstead as ChooseInstead

-- | Named fields, since both selections share a type and a card file that
-- swapped them would still decode.
codec :: (Typeable.Typeable selection) => Codec.Codec selection -> Codec.Codec (ChooseInstead.ChooseInstead selection)
codec selection = Fields.object $ do
  condition <- Fields.required "condition" Condition.codec ChooseInstead.condition
  ordinary <- Fields.required "ordinary" selection ChooseInstead.ordinary
  instead <- Fields.required "instead" selection ChooseInstead.instead
  pure
    ChooseInstead.MkChooseInstead
      { ChooseInstead.condition = condition,
        ChooseInstead.ordinary = ordinary,
        ChooseInstead.instead = instead
      }
