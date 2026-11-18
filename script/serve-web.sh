#!/usr/bin/env bash
# Serves the built web client (default dist-web/, from script/build-web.sh)
# over https on every interface, so a phone or another machine on the network
# can load it. Plain http from a network address is not a secure context, and
# Safari runs WebAssembly an order of magnitude slower outside one; localhost
# is one even over http. The certificate is self-signed for localhost and this
# machine's IPv4 addresses and made afresh each run, so a browser warns once.
#   script/serve-web.sh [DIR] [PORT]
set -euo pipefail
dir=${1:-dist-web}
port=${2:-8443}
[ -f "$dir/index.html" ] || { echo "no $dir/index.html; run script/build-web.sh first" >&2; exit 1; }
certs=$(mktemp -d)
trap 'rm -r "$certs"' EXIT
addresses=$(ifconfig 2>/dev/null | awk '/inet / { print $2 }' | sed 's/^addr://')
names=DNS:localhost
for address in $addresses; do names="$names,IP:$address"; done
openssl req -x509 -newkey rsa:2048 -nodes -days 7 -subj /CN=parun-web \
  -addext "subjectAltName=$names" -keyout "$certs/key.pem" -out "$certs/cert.pem" 2>/dev/null
for address in $addresses; do echo "https://$address:$port/"; done
python3 -I -c '
import functools, http.server, ssl, sys
directory, port, certs = sys.argv[1], int(sys.argv[2]), sys.argv[3]
handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=directory)
server = http.server.ThreadingHTTPServer(("0.0.0.0", port), handler)
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certs + "/cert.pem", certs + "/key.pem")
server.socket = context.wrap_socket(server.socket, server_side=True)
server.serve_forever()
' "$dir" "$port" "$certs"
