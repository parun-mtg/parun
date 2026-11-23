#!/usr/bin/env bash
# Builds the web client into a directory ready to serve (default dist-web/):
# the page, its style sheet, manifest and icons, the service worker, the
# engine's Web Worker, the wasm module and its JavaScript FFI glue, the WASI
# shim, and the data directory the engine mounts, in chunks it fetches one at
# a time (decks, four slices of the cards, rules, and scenarios on demand). Every file but the page,
# its manifest and icons and the service worker is named by a hash of its
# contents, so a browser caches each until it changes, and the service
# worker (sw.js) keeps them for later visits. Runs in the wasm dev shell;
# serve the result with script/serve-web.sh:
#   nix develop .#wasm --command script/build-web.sh
#   script/serve-web.sh
set -euo pipefail
out=${1:-dist-web}
cabal=(wasm32-wasi-cabal --project-file=cabal.wasm.project --builddir=dist-wasm --flags=pedantic)
"${cabal[@]}" build -v1 --enable-optimization exe:parun-web
wasm=$("${cabal[@]}" list-bin -v0 --enable-optimization exe:parun-web)
node --check source/web/page.js
node --check source/web/engine.js
node --check source/web/sw.js
rm -rf "$out"
mkdir -p "$out"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$out/ghc_wasm_jsffi.js"
wasm-opt -Oz --strip-debug "$wasm" -o "$out/parun.wasm"
cp source/web/index.html source/web/style.css source/web/page.js source/web/engine.js source/web/sw.js source/web/manifest.webmanifest source/web/icon.svg source/web/icon.png "$out/"
cp -r "$BROWSER_WASI_SHIM/dist" "$out/browser_wasi_shim"
chmod -R u+w "$out"
# The version and commit the settings menu shows.
version=$(awk '$1 == "version:" { print $2 }' parun.cabal)
commit=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
sed -i "s|@VERSION@|$version|; s|@COMMIT@|$commit|" "$out/index.html"
# The data directory the engine mounts, reproducibly packed in chunks, each
# merged into /data as it arrives (engine.js): the decks and formats; the
# cards as four bundles (Parun.Registry.bundlePaths) of equal count, in path
# order; the rules data; and the scenarios with their titles, fetched only
# for a replay.
staging=$(mktemp -d)
trap 'rm -rf "$staging"' EXIT
pack() {
  local name=$1
  shift
  tar --format=ustar --sort=name --owner=0 --group=0 --numeric-owner --mtime=@0 -cf - "$@" | gzip -9n > "$out/$name.tar.gz"
}
find data/cards -name '*.json' | LC_ALL=C sort > "$staging/cards"
slice=$(( ($(wc -l < "$staging/cards") + 3) / 4 ))
split -l "$slice" -d -a 1 "$staging/cards" "$staging/slice-"
for k in 0 1 2 3; do
  touch "$staging/slice-$k"
  while read -r card; do
    printf '%s\0' "${card##*/}"
    cat "$card"
    printf '\0'
  done < "$staging/slice-$k" > "$staging/cards-$k.bundle"
  pack "cards-$k" -C "$staging" "cards-$k.bundle"
done
pack decks -C "$PWD/data" decks formats
pack rules -C "$PWD/data" legalities.json printings.json sticker-sheets
# Each scenario's description by its name (Parun.Scenario.Load.titlesPath),
# so the replay lists every scenario by its title without decoding each.
node -e '
const fs = require("fs");
const root = "data/scenarios";
const names = fs.readdirSync(root, { recursive: true }).filter((f) => f.endsWith(".json")).sort();
const titles = Object.fromEntries(names.map((f) => [f.slice(0, -5), JSON.parse(fs.readFileSync(root + "/" + f, "utf8")).description]));
fs.writeFileSync(process.argv[1], JSON.stringify(titles));
' "$staging/scenario-titles.json"
pack scenarios -C "$PWD/data" --exclude=README.md scenario-manifest.json scenarios -C "$staging" scenario-titles.json

# Content addressing, leaves first: a file's hash covers the names it refers
# to, so a change anywhere renames every file that leads to it.
hash() { sha256sum "$1" | cut -c1-16; }
# Renames name.ext to name-HASH.ext and rewrites "./name.ext" in each
# referring file.
address() {
  local file=$1
  shift
  local stem=${file%.*} ext=${file##*.}
  local named
  named="$stem-$(hash "$out/$file").$ext"
  mv "$out/$file" "$out/$named"
  for referrer in "$@"; do
    sed -i "s|\"\\./$file\"|\"./$named\"|g" "$out/$referrer"
  done
}
# The shim's modules import each other by name, so its directory is renamed
# whole, by a hash of every file in it.
shim="browser_wasi_shim-$(cat "$out"/browser_wasi_shim/* | sha256sum | cut -c1-16)"
mv "$out/browser_wasi_shim" "$out/$shim"
sed -i "s|\"\\./browser_wasi_shim/|\"./$shim/|g" "$out/engine.js"
address parun.wasm engine.js
for chunk in decks cards-0 cards-1 cards-2 cards-3 rules scenarios; do
  address "$chunk.tar.gz" engine.js
done
address ghc_wasm_jsffi.js engine.js
address engine.js page.js
address page.js index.html
address style.css index.html
ls -l "$out"
