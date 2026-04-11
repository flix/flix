#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [[ -n "${FLIX:-}" ]]; then
  FLIX_BIN="$FLIX"
elif command -v flix >/dev/null 2>&1; then
  FLIX_BIN="flix"
elif [[ -x "$ROOT/../../../build/install/flix/bin/flix" ]]; then
  FLIX_BIN="$ROOT/../../../build/install/flix/bin/flix"
else
  echo "error: could not find 'flix' on PATH or at $ROOT/../../../build/install/flix/bin/flix" >&2
  echo "set FLIX=/path/to/flix or build the repo launcher with ./gradlew installDist" >&2
  exit 1
fi

"$FLIX_BIN" bind wasm-effects --wit wit --world demo --out generated
cp generated/flix/Wit.flix src/Wit.flix

printf 'Generated bindings in %s\n' "$ROOT/generated"
printf 'Vendored Flix source to %s\n' "$ROOT/src/Wit.flix"
