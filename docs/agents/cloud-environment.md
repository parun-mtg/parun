# The cloud environment

The setup script of the claude.ai cloud environment "parun"
(`env_01NJhY7oBkWmf9enJzj655dH`), which every cloud session on this
repository starts in, the label routine's (`docs/agents/label-routine.md`)
included. The environment is the source of truth and this file its history:
change both in the same PR, the environment through the `update_environment`
tool (or its settings page on claude.ai), and the text below verbatim.

The script's result is kept as a snapshot for about a week, and only when the
script finishes within about five minutes: a session's toolchain can be that
old, and its checkout is still current. An edit takes effect the next time a
container is built fresh.

```bash
#!/usr/bin/env bash
# Cloud environment setup for parun: install Nix, enter the dev shell,
# configure cabal, fetch card data, build dependencies. The project itself
# changes too often for a prebuilt copy to stay useful, so sessions build it
# themselves.
set -euo pipefail
cd parun
export USER=root HOME=/root PATH="/root/.nix-profile/bin:$PATH"

# 1. Install Nix. The image has no nixbld group; an empty one builds as root.
mkdir -p /etc/nix
printf 'build-users-group =\nexperimental-features = nix-command flakes\n' > /etc/nix/nix.conf
if [ ! -x /root/.nix-profile/bin/nix ]; then
  curl -fsSL https://releases.nixos.org/nix/nix-2.35.2/install | sh -s -- --no-daemon --yes
fi

# 2. Enter the dev shell. The session's GitHub proxy refuses github: inputs,
# so fetch each as an archive tarball, nested inputs by their path. Sessions
# never load the shell, so link its tools into /usr/local/bin, which is on
# their PATH. The wasm toolchain comes from release assets, which sessions
# can fetch but only this script can install.
overrides=()
while read -r name url; do overrides+=(--override-input "$name" "$url"); done < <(jq -r '
  . as $lock
  | def inputs($node; $path): ($lock.nodes[$node].inputs // {}) | to_entries[]
      | select(.value | type == "string") | [$path + [.key], .value] as $edge
      | $edge, inputs($edge[1]; $edge[0]);
  inputs($lock.root; []) | $lock.nodes[.[1]].locked as $l | select($l.type == "github")
  | "\(.[0] | join("/")) https://github.com/\($l.owner)/\($l.repo)/archive/\($l.rev).tar.gz"' flake.lock)
shell() { nix develop --no-write-lock-file "${overrides[@]}" . --command bash -c "$1"; }
shell 'for tool in ghc cabal cabal-gild hooky hlint ormolu ghcid nixfmt \
  wasm32-wasi-ghc wasm32-wasi-cabal wasm-opt wasmtime node; do
  ln -sf "$(dirname "$(command -v "$tool")")"/* /usr/local/bin/; done'

# 3. Configure cabal. cabal.project already turns off shared and dynamic objects.
# GHC gets -j4 directly: the machine has four cores, the package graph is nearly
# a chain, and the shared semaphore is unreliable.
cat > cabal.project.local <<'EOF'
benchmarks: True
flags: +pedantic
ignore-project: False
jobs: 4
multi-repl: True
tests: True

program-options
  ghc-options: -j4 +RTS -A64m -RTS
EOF

# 4. Fetch MTGJSON and mtgish into _scratch/ beside the build, which they
# must not push past the snapshot's time limit. mtgish comes as a tarball,
# like the flake inputs. A failure here must not stop sessions starting.
mkdir -p _scratch
mtgjson() {
  curl -fsSL "https://mtgjson.com/api/v5/$1.json.gz" | gunzip > "_scratch/$1.json.part" &&
    mv "_scratch/$1.json.part" "_scratch/$1.json"
}
(mtgjson AllPrintings) || true &
(mtgjson AtomicCards &&
  jq -f script/ingest/candidates.jq _scratch/AtomicCards.json > _scratch/candidates.json) || true &
(rm -rf _scratch/mtgish && mkdir _scratch/mtgish &&
  curl -fsSL https://github.com/i5jb/mtgish/archive/HEAD.tar.gz |
  tar -xz --strip-components=1 -C _scratch/mtgish) || true &

# 5. Build dependencies. A failure here must not stop sessions starting.
shell 'cabal update && cabal build all --only-dependencies' || true
wait
```
