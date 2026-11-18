#!/usr/bin/env bash
# Serves the built web client (default dist-web/, from script/build-web.sh)
# over https on localhost and this machine's IPv4 addresses, so a phone or
# another machine on the network can load it. Plain http from a network
# address is not a secure context, and Safari runs WebAssembly an order of
# magnitude slower outside one. Runs in the wasm dev shell, with Caddy
# (script/serve-web.caddyfile):
#   nix develop .#wasm --command script/serve-web.sh [DIR] [PORT]
# Caddy signs the certificates with its own local authority. A browser warns
# until it trusts that authority: on this machine, run `caddy trust` once in
# another shell while this runs (it asks for your password); on another
# device, install the root certificate from Caddy's data directory
# (pki/authorities/local/root.crt) and trust it.
set -euo pipefail
dir=${1:-dist-web}
port=${2:-8443}
[ -f "$dir/index.html" ] || { echo "no $dir/index.html; run script/build-web.sh first" >&2; exit 1; }
sites="https://localhost:$port"
for address in $(ifconfig 2>/dev/null | awk '/inet / { print $2 }' | sed 's/^addr://'); do
  sites="$sites, https://$address:$port"
done
echo "$sites"
ROOT=$(cd "$dir" && pwd) SITES=$sites exec caddy run --adapter caddyfile --config "$(dirname "$0")/serve-web.caddyfile"
