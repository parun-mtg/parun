-- | What tab completion answers a shell's shim.
module Parun.CommandLine.Completion.Reply where

-- | The answer for the word at the cursor.
data Reply
  = -- | These whole words, each beginning with the word at the cursor.
    MkCandidates [String]
  | -- | A file name the shell completes, after this prefix of the word.
    MkFiles String
  deriving (Eq, Show)

-- | The reply on the wire Parun.CommandLine.Completion describes.
render :: Reply -> String
render reply = case reply of
  MkCandidates candidates -> unlines candidates
  MkFiles prefix -> ":files" <> prefix <> "\n"
