module Parun.Codec.ZoneChangeRSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.ZoneChangeR as ZoneChangeR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterName as CounterName
import qualified Parun.Types.DiscardCause as DiscardCause
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ZoneChangeR" $ do
  -- CR 614.1a: Rest in Peace exiles what would go to a graveyard.
  Spec.it s "MkZoneChangeR" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Exile,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False,
            ZoneChangeR.counters = Nothing,
            ZoneChangeR.whenYouDo = Seq.empty,
            ZoneChangeR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\"},\"destination\":\"Exile\"} "
  -- CR 701.20 / 701.24: Nexus of Fate's shape, where both riders are written
  -- out rather than defaulted away.
  Spec.it s "MkZoneChangeR with both riders" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.IsSource,
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Library,
            ZoneChangeR.revealing = True,
            ZoneChangeR.shuffling = True,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False,
            ZoneChangeR.counters = Nothing,
            ZoneChangeR.whenYouDo = Seq.empty,
            ZoneChangeR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\",\"whatObject\":{\"type\":\"IsSource\"}},\"destination\":\"Library\",\"revealing\":true,\"shuffling\":true} "
  -- CR 614.1a / 401.2: Library of Leng's optional redirect of an effect's
  -- discard to the top of the library, every new field off its default.
  Spec.it s "MkZoneChangeR (Library of Leng)" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.You,
                  ZoneChangePattern.whenDiscarded = Just DiscardCause.ByEffect,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Library,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.Top,
            ZoneChangeR.optional = True,
            ZoneChangeR.counters = Nothing,
            ZoneChangeR.whenYouDo = Seq.empty,
            ZoneChangeR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\",\"whoseObject\":{\"type\":\"You\"},\"whenDiscarded\":\"ByEffect\"},\"destination\":\"Library\",\"position\":\"Top\",\"optional\":true} "
  -- CR 603.11: Head of the Hunt's linked "when you do"; the effect codec is a
  -- parameter, so text stands in for it.
  Spec.it s "MkZoneChangeR with a linked trigger (Head of the Hunt)" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Exile,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False,
            ZoneChangeR.counters = Nothing,
            ZoneChangeR.whenYouDo = Seq.singleton (Text.pack "wolf"),
            ZoneChangeR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\"},\"destination\":\"Exile\",\"whenYouDo\":[\"wolf\"]} "
  -- CR 614.1a / 614.6: Kalitas, Traitor of Ghet's "instead exile that card and
  -- create a 2/2 black Zombie creature token".
  Spec.it s "MkZoneChangeR with a rider (Kalitas, Traitor of Ghet)" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Exile,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False,
            ZoneChangeR.counters = Nothing,
            ZoneChangeR.whenYouDo = Seq.empty,
            ZoneChangeR.riders = Seq.singleton (Text.pack "zombie")
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\"},\"destination\":\"Exile\",\"riders\":[\"zombie\"]} "
  -- CR 122.1: Goliath Daydreamer's "exile that card with a dream counter on it".
  Spec.it s "MkZoneChangeR with arrival counters (Goliath Daydreamer)" $
    Common.assertCodec
      s
      (ZoneChangeR.codec Common.text)
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.causedBy = Nothing,
                  ZoneChangePattern.duringResolution = True
                },
            ZoneChangeR.destination = Zone.Exile,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False,
            ZoneChangeR.counters = Just (WithCounters.one (CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "dream"))) (Quantity.Literal 1)),
            ZoneChangeR.whenYouDo = Seq.empty,
            ZoneChangeR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whenDestination\":\"Graveyard\",\"duringResolution\":true},\"destination\":\"Exile\",\"counters\":[{\"kind\":{\"type\":\"Named\",\"value\":\"dream\"},\"count\":{\"type\":\"Literal\",\"value\":1}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (ZoneChangeR.codec Common.text)
