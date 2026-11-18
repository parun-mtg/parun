#!/usr/bin/env bash
# Turns a release's build artifacts (.github/workflows/build.yml), downloaded
# into ARTIFACTS one directory each, into its assets in OUT: an archive per
# platform holding the binary and the data directory it reads through
# parun_datadir, the sdist, the Wasm build with the web client, the schemas,
# and SHA256SUMS.
#   script/package-release.sh VERSION ARTIFACTS OUT
set -euo pipefail
version=$1
artifacts=$2
out=$3
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$out"
out=$(cd "$out" && pwd)
for dir in "$artifacts/parun-$version"-*/; do
  name=$(basename "$dir")
  mkdir "$work/$name"
  tar --extract --file "$dir/artifact.tar" --directory "$work/$name"
done
# The static Linux job builds the platform-independent assets.
linux=$work/parun-$version-Linux-X64/artifact
cp "$linux/parun-$version.tar.gz" "$linux/schema.json" "$linux/scenario-schema.json" "$out/"
mkdir "$work/sdist"
tar --extract --gzip --file "$linux/parun-$version.tar.gz" --directory "$work/sdist"
data=$work/sdist/parun-$version/data
mkdir "$work/stage"
for dir in "$work/parun-$version"-*/; do
  platform=$(basename "$dir")
  platform=${platform#"parun-$version-"}
  if test "$platform" = wasm; then
    asset=parun-$version-wasm
    mkdir "$work/stage/$asset"
    cp -R "$dir/artifact/web" "$dir/artifact/parun.wasm" "$work/stage/$asset/"
    tar --create --gzip --file "$out/$asset.tar.gz" --directory "$work/stage" "$asset"
    continue
  fi
  os=${platform%-*}
  case ${platform#*-} in
    X64) arch=x86_64 ;;
    ARM64) arch=arm64 ;;
    *) echo "unknown architecture: $platform" >&2; exit 1 ;;
  esac
  asset=parun-$version-$(echo "$os" | tr "[:upper:]" "[:lower:]")-$arch
  mkdir "$work/stage/$asset"
  for binary in parun parun.exe; do
    if test -e "$dir/artifact/$binary"; then cp "$dir/artifact/$binary" "$work/stage/$asset/"; fi
  done
  cp -R "$data" "$work/stage/$asset/data"
  if test "$os" = Windows; then
    (cd "$work/stage" && zip --quiet --recurse-paths "$out/$asset.zip" "$asset")
  else
    tar --create --gzip --file "$out/$asset.tar.gz" --directory "$work/stage" "$asset"
  fi
done
(cd "$out" && sha256sum -- * > SHA256SUMS)
ls -l "$out"
