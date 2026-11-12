module Parun.Codec.ChoosePawprintsSpec where

import qualified Data.Either as Either
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.ChoosePawprints as ChoosePawprints
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChoosePawprints as ChoosePawprints

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChoosePawprints" $ do
  -- CR 700.2i (data/cards/season-of-loss.json).
  Spec.it s "MkChoosePawprints" $
    Common.assertCodec
      s
      ChoosePawprints.codec
      (ChoosePawprints.MkChoosePawprints 5 (Seq.fromList [1, 2, 3]))
      " {\"most\":5,\"pawprints\":[1,2,3]} "
  Spec.it s "rejects a mode worth no pawprints" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"most\":5,\"pawprints\":[1,0]} ") >>= Codec.decode ChoosePawprints.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePawprints.codec
