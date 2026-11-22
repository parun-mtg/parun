-- | What an option's argument or a positional argument completes to.
module Parun.CommandLine.Completion.Value where

-- | The values a word completes to.
data Value
  = -- | A file name, completed by the shell.
    MkFiles
  | -- | One of these words.
    MkChoices [String]
  | -- | A shipped format's name (Parun.Legality.shippedFormats).
    MkFormats
  | -- | The value, or one of these seats, `=`, and the value.
    MkSeated [String] Value
  | -- | This prefix and the value; nothing without the prefix.
    MkPrefixed String Value
  | -- | Nothing: a value no list holds.
    MkOpaque
  deriving (Eq, Show)
