module Parun.Codec.CounterHolder where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CounterHolder as CounterHolder
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation.Type

-- | A Permanent watching no player keeps the bare "Permanent" tag every
-- permanent-only card writes; one watching players too is "PermanentOrPlayer".
codec :: Codec.Codec CounterHolder.CounterHolder
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Self" CounterHolder.Self,
      Arm.payload "Permanent" (Filter.codec Keyword.codec) (`CounterHolder.Permanent` Nothing) (\x -> case x of CounterHolder.Permanent y Nothing -> Just y; _ -> Nothing),
      Arm.payload "PermanentOrPlayer" permanentOrPlayer (\(f, r) -> CounterHolder.Permanent f (Just r)) (\x -> case x of CounterHolder.Permanent f (Just r) -> Just (f, r); _ -> Nothing)
    ]

tagOf :: CounterHolder.CounterHolder -> String
tagOf x = case x of
  CounterHolder.Self {} -> "Self"
  CounterHolder.Permanent _ Nothing -> "Permanent"
  CounterHolder.Permanent _ (Just _) -> "PermanentOrPlayer"

-- | The permanents' Filter and the players' relation.
permanentOrPlayer :: Codec.Codec (Filter.Type.Filter Keyword.Type.Keyword, PlayerRelation.Type.PlayerRelation)
permanentOrPlayer =
  Fields.object $
    (,)
      <$> Fields.required "permanent" (Filter.codec Keyword.codec) fst
      <*> Fields.required "player" PlayerRelation.codec snd
