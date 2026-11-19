#!/usr/bin/env bash
# Keep one worktree at origin/main with a finished build, as the donor for
# every fresh worktree's dist-newstyle, which script/mutate.sh uses too, and
# its dist-wasm (script/build-web.sh), where the wasm dev shell builds. Run
# after each merge; the build is incremental, so it costs the merge's diff and
# nothing else. Usage:
#   script/warm-worktree.sh            # refresh the donor
#   script/warm-worktree.sh seed DIR   # clone its build into DIR
# The donor lives at .claude/worktrees/warm under the primary checkout and is
# only ever reset to origin/main; nothing is edited or committed there.
set -euo pipefail
primary=$(git rev-parse --path-format=absolute --git-common-dir)
primary=${primary%/.git}
warm="$primary/.claude/worktrees/warm"
case "${1:-refresh}" in
  seed)
    dest="${2:?usage: warm-worktree.sh seed DIR}"
    [ -d "$warm/dist-newstyle" ] || { echo "no warm build at $warm" >&2; exit 1; }
    [ -e "$dest/dist-newstyle" ] && { echo "$dest/dist-newstyle exists" >&2; exit 1; }
    cp -a "$warm/dist-newstyle" "$dest/dist-newstyle"
    if [ -d "$warm/dist-wasm" ] && [ ! -e "$dest/dist-wasm" ]; then cp -a "$warm/dist-wasm" "$dest/dist-wasm"; fi
    cp "$primary/cabal.project.local" "$dest/cabal.project.local"
    echo "seeded $dest from $(git -C "$warm" rev-parse --short HEAD)"
    ;;
  refresh)
    git -C "$primary" fetch -q origin
    if [ ! -d "$warm" ]; then
      git -C "$primary" worktree add -q --detach "$warm" origin/main
    else
      git -C "$warm" checkout -q --detach origin/main
    fi
    cp "$primary/cabal.project.local" "$warm/cabal.project.local"
    (cd "$warm" && "$primary/script/with-build-lock.sh" cabal build -v0 all)
    # The web client's build, whose path names the version as the native
    # one's does, so a copy from before a version bump rebuilds cold.
    if nix develop "$warm#wasm" --command true 2>/dev/null; then
      out=$(mktemp -d)
      (cd "$warm" && "$primary/script/with-build-lock.sh" nix develop .#wasm --command script/build-web.sh "$out/dist-web" > "$out/log" 2>&1) || { tail -20 "$out/log" >&2; exit 1; }
      rm -rf "$out"
    fi
    # A version bump leaves the old version's build behind, and seed copies it.
    version=$(awk '$1 == "version:" { print $2 }' "$warm/parun.cabal")
    find "$warm/dist-newstyle/build" "$warm/dist-wasm/build" -mindepth 3 -maxdepth 3 -type d -name 'parun-*' ! -name "parun-$version" -prune -exec rm -rf {} + 2>/dev/null || true
    echo "warm at $(git -C "$warm" rev-parse --short HEAD)"
    ;;
  *)
    echo "usage: warm-worktree.sh [refresh | seed DIR]" >&2
    exit 2
    ;;
esac
