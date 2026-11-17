#!/usr/bin/env bash
# Builds the web client into a directory ready to serve (default dist-web/):
# the wasm module, its JavaScript FFI glue, the WASI shim, the page and the
# data directory it mounts. Runs in the wasm dev shell; serve the result with
# any static server, e.g.
#   nix develop .#wasm --command script/build-web.sh
#   python3 -m http.server -d dist-web
set -euo pipefail
out=${1:-dist-web}
cabal=(wasm32-wasi-cabal --project-file=cabal.wasm.project --builddir=dist-wasm --flags=pedantic)
"${cabal[@]}" build -v1 --enable-optimization exe:parun-web
wasm=$("${cabal[@]}" list-bin -v0 --enable-optimization exe:parun-web)
rm -rf "$out"
mkdir -p "$out"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$out/ghc_wasm_jsffi.js"
wasm-opt -Oz --strip-debug "$wasm" -o "$out/parun.wasm"
node --check source/web/parun.js
cp source/web/index.html source/web/parun.js "$out/"
# The data directory the page mounts, reproducibly packed.
tar -C data --format=ustar --sort=name --owner=0 --group=0 --numeric-owner --mtime=@0 \
  -cf - cards decks formats legalities.json printings.json sticker-sheets | gzip -9n > "$out/data.tar.gz"
cp -r "$BROWSER_WASI_SHIM/dist" "$out/browser_wasi_shim"
chmod -R u+w "$out"
ls -l "$out/parun.wasm" "$out/data.tar.gz"
