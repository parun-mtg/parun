{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PerformerR where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.InstructionRewrite as InstructionRewrite
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerInstruction as PlayerInstruction
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PerformerR as PerformerR

codec :: Codec.Codec PerformerR.PerformerR
codec = Fields.object $ do
  instruction <- Fields.required "instruction" PlayerInstruction.codec PerformerR.instruction
  performer <- Fields.required "performer" (Filter.codec Keyword.codec) PerformerR.performer
  rewrite <- Fields.required "rewrite" InstructionRewrite.codec PerformerR.rewrite
  pure
    PerformerR.MkPerformerR
      { PerformerR.instruction = instruction,
        PerformerR.performer = performer,
        PerformerR.rewrite = rewrite
      }
