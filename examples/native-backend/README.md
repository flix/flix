# Native Backend Examples

These examples exercise the LLVM native/wasm proof of concept rather than the package manager itself.

They cover:

- typed native/wasm exports,
- async wasm effect bindings,
- and cross-target host integration / FFI workflows.

Current examples:

- `export-smoke`
- `wasm-effects-smoke`
- `wasm-effects-recursive-smoke`
- `sqlite-cross-target`

## Common Setup

From repo root:

```bash
./gradlew installDist
export FLIX="$PWD/build/install/flix/bin/flix"
```

You may use `flix` on `PATH` instead, but the examples and scripts are written to work with the
repo-local launcher too.

## Fastest Paths

### Typed exports

```bash
cd examples/native-backend/export-smoke
$FLIX build --target native
zig cc \
  -I build/native/sdk/include \
  host/native/main.c \
  build/native/sdk/lib/libexport-smoke.a \
  -o build/native/export-smoke-host
./build/native/export-smoke-host

$FLIX build --target wasm
node host/wasm-js/main.mjs
cargo +stable run --manifest-path host/wasm-rust/Cargo.toml
```

See [export-smoke/README.md](./export-smoke/README.md).

### Async wasm imports

```bash
cd examples/native-backend/wasm-effects-smoke
$FLIX build --target wasm
node host/wasm-js/main.mjs
cargo +stable run --manifest-path host/wasm-rust/Cargo.toml
```

For recursive portable values, use:

```bash
cd examples/native-backend/wasm-effects-recursive-smoke
$FLIX build --target wasm
node host/wasm-js/main.mjs
cargo +stable run --manifest-path host/wasm-rust/Cargo.toml
```

See:

- [wasm-effects-smoke/README.md](./wasm-effects-smoke/README.md)
- [wasm-effects-recursive-smoke/README.md](./wasm-effects-recursive-smoke/README.md)

### Native FFI / bridge package workflow

This is the best “small real project with FFI” example in the tree.

```bash
cd examples/native-backend/sqlite-cross-target/native
$FLIX check
$FLIX build --target native
./build/native/llvm/sqlite-native
```

Expected output:

```text
1:hello:4
```

For the full cross-target pressure test instead of just the native half:

```bash
./gradlew test --tests ca.uwaterloo.flix.SqliteCrossTargetSuite
```

See [sqlite-cross-target/README.md](./sqlite-cross-target/README.md).
