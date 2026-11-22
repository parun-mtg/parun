-- | The script each shell runs to complete a `parun` command line: a shim
-- passing the words typed to `parun __complete` and the answer back to the
-- shell, on the wire Parun.CommandLine.Completion describes.
module Parun.CommandLine.Completion.Shim where

import qualified Data.List as List
import qualified Parun.CommandLine.Bare.Flag as Flag
import qualified System.Console.GetOpt as GetOpt

-- | Every shell's name and its shim, in order.
shells :: [(String, String)]
shells = [("bash", bash), ("zsh", zsh), ("fish", fish)]

-- | `parun completion`'s usage, saying how to install each shim.
usage :: String
usage =
  GetOpt.usageInfo
    ( List.intercalate
        "\n"
        [ "usage: parun completion [OPTION...] SHELL",
          "Prints the tab completion script for SHELL, bash, zsh or fish. To install it:",
          "  bash: eval \"$(parun completion bash)\" in ~/.bashrc",
          "  zsh:  eval \"$(parun completion zsh)\" in ~/.zshrc, after compinit",
          "  fish: parun completion fish > ~/.config/fish/completions/parun.fish"
        ]
    )
    Flag.flags

-- | Bash's shim. It splits the line at blanks alone, so a word holds its `=`
-- as GetOpt reads it, and drops from each reply what bash keeps of the word
-- before its last `=` or `:`.
bash :: String
bash =
  unlines
    [ "# parun's bash completion: eval \"$(parun completion bash)\" in ~/.bashrc",
      "_parun() {",
      "  local typed=\"${COMP_LINE:0:COMP_POINT}\" line prefix current broken",
      "  local -a words replies",
      "  read -r -a words <<< \"$typed\"",
      "  [[ \"$typed\" == *[[:space:]] ]] && words+=(\"\")",
      "  current=\"${words[${#words[@]} - 1]}\"",
      "  while IFS= read -r line; do replies+=(\"$line\"); done < <(\"${words[0]}\" __complete \"$((${#words[@]} - 2))\" \"${words[@]:1}\" 2> /dev/null)",
      "  if [[ \"${replies[0]}\" == :files* ]]; then",
      "    prefix=\"${replies[0]#:files}\"",
      "    replies=()",
      "    while IFS= read -r line; do replies+=(\"$prefix$line\"); done < <(compgen -f -- \"${current#\"$prefix\"}\")",
      "    compopt -o filenames 2> /dev/null",
      "  fi",
      "  broken=\"${current%\"${current##*[=:]}\"}\"",
      "  COMPREPLY=()",
      "  for line in \"${replies[@]}\"; do COMPREPLY+=(\"${line#\"$broken\"}\"); done",
      "}",
      "complete -F _parun parun"
    ]

-- | Zsh's shim, for `eval` or as the `_parun` file of a directory on
-- `fpath`, which compinit loads as the function's body and so runs it.
zsh :: String
zsh =
  unlines
    [ "#compdef parun",
      "# parun's zsh completion: eval \"$(parun completion zsh)\" in ~/.zshrc, after compinit",
      "_parun() {",
      "  local -a replies",
      "  local prefix",
      "  replies=(${(f)\"$(\"${words[1]}\" __complete \"$((CURRENT - 2))\" \"${(@)words[2,CURRENT]}\" 2> /dev/null)\"})",
      "  if [[ \"${replies[1]}\" == :files* ]]; then",
      "    prefix=\"${replies[1]#:files}\"",
      "    PREFIX=\"${PREFIX#\"$prefix\"}\"",
      "    IPREFIX=\"$IPREFIX$prefix\"",
      "    _files",
      "  else",
      "    compadd -- \"${replies[@]}\"",
      "  fi",
      "}",
      "compdef _parun parun",
      "if [[ \"${funcstack[1]}\" == _parun ]]; then _parun \"$@\"; fi"
    ]

-- | Fish's shim, for a file under ~/.config/fish/completions.
fish :: String
fish =
  unlines
    [ "# parun's fish completion: parun completion fish > ~/.config/fish/completions/parun.fish",
      "function __parun_complete",
      "    set -l tokens (commandline -opc)",
      "    set -l current (commandline -ct)",
      "    set -l program $tokens[1]",
      "    set -e tokens[1]",
      "    set -l replies ($program __complete (count $tokens) $tokens $current 2> /dev/null)",
      "    if string match -q -- ':files*' \"$replies[1]\"",
      "        set -l prefix (string sub -s 7 -- $replies[1])",
      "        for path in (__fish_complete_path (string sub -s (math (string length -- \"$prefix\") + 1) -- \"$current\"))",
      "            printf '%s\\n' \"$prefix$path\"",
      "        end",
      "    else",
      "        for reply in $replies",
      "            printf '%s\\n' $reply",
      "        end",
      "    end",
      "end",
      "complete -c parun -f -a '(__parun_complete)'"
    ]
