{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChoosePawprints where

import qualified Data.Text as Text
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChoosePawprints as ChoosePawprints

-- | Rejects a mode worth no pawprints (CR 700.2i), Parun.Types.ChoosePawprints'
-- invariant.
codec :: Codec.Codec ChoosePawprints.ChoosePawprints
codec =
  let check cp =
        if elem 0 (ChoosePawprints.pawprints cp)
          then Left (Text.pack "ChoosePawprints: a mode must be worth at least one pawprint")
          else Right cp
   in Fields.objectWith check $ do
        most <- Fields.required "most" Common.natural ChoosePawprints.most
        pawprints <- Fields.required "pawprints" (Common.seq Common.natural) ChoosePawprints.pawprints
        pure
          ChoosePawprints.MkChoosePawprints
            { ChoosePawprints.most = most,
              ChoosePawprints.pawprints = pawprints
            }
