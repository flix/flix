# Export Smoke

Minimal `@Export` example for native and wasm embedding.

The exported Flix surface is in [src/Api.flix](./src/Api.flix). It demonstrates:

- typed scalar arguments and results
- `String`
- `Bytes` (`Array[Int8, Static]`)
- `List[Int32]`
- `Array[Int32, Static]`
- `Array[String, Static]`
- `List[(Int32, String)]`
- `Array[Option[Int32], Static]`
- `List[{ name = String, score = Int32 }]`
- `Array[{ name = String, score = Int32 }, Static]`
- `Option[Int32]`
- tuples
- `Result[String, Int32]`
- closed records
- a suspending export (`suspendEcho`)

## Build

Native:

```bash
flix build --target native
```

Wasm:

```bash
flix build --target wasm
```

## Native C Host

After `flix build --target native`:

```bash
zig cc \
  -I build/native/sdk/include \
  host/native/main.c \
  build/native/sdk/lib/libexport-smoke.a \
  -o build/native/export-smoke-host

./build/native/export-smoke-host
```

## Wasm JS Host

After `flix build --target wasm`:

```bash
node host/wasm-js/main.mjs
```

This host uses the generated JS bindings:

- `build/wasm/sdk/js/export-smoke.bindings.mjs`
- `build/wasm/sdk/js/export-smoke.exports.component.js`

## Wasm Rust / Wasmtime Host

After `flix build --target wasm`:

```bash
cargo +stable run --manifest-path host/wasm-rust/Cargo.toml
```

The Rust host binds against:

- `build/wasm/sdk/wit`
- `build/wasm/sdk/component/export-smoke.exports.component.wasm`

## Notes

- The wasm export surface is typed and public. It does not expose the internal `flix:runtime/runtime`
  substrate directly.
- The public SDK bundle is under `build/native/sdk/` and `build/wasm/sdk/`.
- The current portable export ABI includes:
  - scalars
  - `String`
  - `Bytes`
  - `List[T]`
  - `Array[T, Static]`
  - tuples
  - `Option`
  - `Result`
  - closed records
- Sequence element types may themselves be aggregates, e.g. `List[Tuple]`, `Array[Option[T], Static]`, `List[Record]`, and `Array[Record, Static]`.
- Native C aggregate parameters use `const *` pointer parameters in the generated header.
- Native C sequence params/results use `{ len, ptr }`.
- Native C sequence result buffers are released with `flix_free`.
- Nested string handles inside native sequence results are still released with `flix_handle_release`.
- The JS bindings expose ergonomic host shapes:
  - tuples as arrays
  - `List[T]` and `Array[T, Static]` as arrays
  - `Option[T]` as `null | T`
  - `Result[Err, Ok]` as `{ tag: "ok" | "err", val: ... }`
  - records as JS objects
- Suspensions are explicit. Hosts resume them with exported `resume-*` functions.
- When the compiler can prove one portable suspension-op signature for an export, it also emits a
  typed `request-*` helper for reading suspension arguments without dropping to generic `value`
  resources.
- In this example the native C host and the wasm hosts all use the same typed suspension surface.
- `resumeSuspendEcho` is typed as `String`, not generic `value`, because the
  compiler can prove that every escaping suspension from `suspendEcho` resumes with a `String`.
- In this example `requestSuspendEcho` is also typed, because `suspendEcho` only suspends on one
  portable op signature: `HostEcho.echo(String): String`.
