# Flix Native / Wasm Exports

For the overall native/wasm proof-of-concept status and scope, see [README.md](README.md).

This document describes the current public `@Export` contract for the native and wasm backends.

The backend is still experimental, but the export surface is now intentionally shaped as a real
embedding API rather than raw backend internals.

## Source Model

Exported functions are ordinary Flix defs annotated with `@Export`:

```flix
mod Api {
    @Export
    pub def add(x: Int32, y: Int32): Int32 = x + y
}
```

Portable export defs must currently satisfy:

- public
- monomorphic
- use only the portable export ABI types
- no effectful return contract except via explicit suspension

## Export ABI v1

Supported portable ABI types:

- `Unit`
- `Bool`
- `Int8`
- `Int16`
- `Int32`
- `Int64`
- `Float32`
- `Float64`
- `String`
- `Array[Int8, Static]` as `Bytes`
- `List[T]` where `T` is exportable
- `Array[T, Static]` where `T` is exportable
- tuples
- `Option[T]`
- `Result[Err, Ok]`
- closed records with distinct host-safe labels

Not currently supported:

- arbitrary ADTs
- generic values
- `Char`
- open records
- records with duplicate or non host-safe labels

Those should be added deliberately as cross-target ABI features, not ad hoc per backend.

The recursive cases are intentional. `T` can itself be a supported aggregate, so these are valid:

- `List[(Int32, String)]`
- `List[{name = String, score = Int32}]`
- `Array[Option[Int32], Static]`
- `Array[(String, Int32), Static]`

### Aggregate Mapping

The portable export ABI now includes a small aggregate set with deliberately explicit host shapes:

- tuples map to ordered aggregate fields
- `List[T]` maps to a host sequence
- `Array[T, Static]` maps to a host sequence
- `Option[T]` maps to `{ is_some, val }`
- `Result[Err, Ok]` maps to `{ is_ok, ok, err }`
- records map to named host fields with a canonical label order

Two details matter:

- Flix source order for `Result` is still `Result[Err, Ok]`
- the host-facing aggregate shape uses named `ok` and `err` fields
- records are structural in Flix, but the ABI is positional underneath, so field order is
  canonicalized by label for stability across targets

That means `Result[String, Int32]` exports as a host aggregate with:

- `is_ok: bool`
- `ok: Int32`
- `err: String`

## Execution Model

Exports use a shared execution model across native and wasm:

- `ok`
- `thrown`
- `suspended`

That means an exported call may:

- return normally,
- throw an exception value,
- suspend and require host resumption.

This is the right model for Flix because effects and suspension are part of the language semantics.

## Native

Native exports generate:

- typed C symbols
- a generated C header
- static/shared libraries

Normal results are written through out-parameters. Exceptions and suspensions cross the boundary as
stable handles.

Aggregate native parameters are passed by `const *` pointer. This is intentional. Passing small
struct aggregates by value would require target-specific ABI coercion logic, which is the wrong
tradeoff for a stable public C surface.

For native sequence types:

- `List[T]` maps to `struct { int64_t len; T* ptr; }`
- `Array[T, Static]` maps to `struct { int64_t len; T* ptr; }`

If `T` is itself an aggregate, `ptr` points to an array of the generated aggregate struct type.

For result ownership:

- scalar fields are copied
- sequence result buffers are host-owned buffers allocated by the runtime and released with `flix_free`
- nested `String` / `Bytes` / suspension handles inside those buffers are still normal Flix handles
  and must be released with `flix_handle_release`

When the compiler can prove a stable portable suspension contract, native exports get the same
typed suspension ergonomics as wasm:

- typed `resume-*` entry points
- typed `request-*` helpers for suspension arguments

If the compiler cannot prove that contract, native hosts stay on the generic suspension-handle API.

See:

- [LlvmExportWriter.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmExportWriter.scala)
- [examples/native-backend/export-smoke/host/native/main.c](../../../examples/native-backend/export-smoke/host/native/main.c)

## Wasm

Wasm exports generate:

- a typed component: `*.exports.component.wasm`
- typed WIT: `*.exports.wit/`
- transpiled component JS: `*.exports.component.js`
- convenience JS/TS bindings: `*.bindings.mjs`, `*.bindings.d.ts`

The public wasm surface is the typed `flix:exports/api@0.1.0` interface. The internal
`flix:runtime/runtime` substrate is not part of the public JS/TS or Wasmtime-facing contract.

Wasm aggregate types are emitted as named WIT records. The convenience JS/TS bindings then map
those records to more idiomatic host values:

- tuples -> JS arrays
- `List[T]` -> JS arrays
- `Array[T, Static]` -> JS arrays
- `Option[T]` -> `null | T`
- `Result[Err, Ok]` -> `{ tag: "ok" | "err", val: ... }`
- records -> JS objects with named fields

This composes recursively. For example, `List[{name = String, score = Int32}]` becomes
`ReadonlyArray<{ name: string; score: number }>` in the generated TypeScript bindings and
`Vec<RecordNameStringScoreInt32>` in Wasmtime Rust bindings. Likewise, `List[(Int32, String)]`
becomes `ReadonlyArray<readonly [number, string]>` in the generated TypeScript bindings and
`Vec<Tuple2Int32String>` in Wasmtime Rust bindings, and `Array[Option[Int32], Static]` becomes
`ReadonlyArray<number | null>` in TypeScript and `Vec<OptionInt32>` in Wasmtime Rust bindings.

Suspension resumption is also typed when the compiler can prove a single portable resume value type
for every escaping suspension in an exported def. For example, an export that only suspends on an
effect op returning `String` will expose a typed `resume-*` entry point. On wasm that becomes
`resume-...(..., resume: string)`. If the compiler cannot prove one uniform portable type, the
public wasm API falls back to the generic `value` resource for that resume parameter, and native
falls back to the generic suspension-handle path.

Suspension request inspection follows a similar rule, but it is stricter. The compiler only emits a
typed `request-...` helper when it can prove that an exported def suspends on exactly one portable
effect-op signature. In that case the helper returns a typed record of the request arguments. If the
export may suspend on multiple ops, or on a non-portable op signature, hosts stay on the generic
inspection path: `suspension-arg-*` on native and `value` / `suspension-arg-*` on wasm.

See:

- [LlvmWasmTypedExportsWriter.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmWasmTypedExportsWriter.scala)
- [LlvmWasmBindingWriter.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmWasmBindingWriter.scala)
- [examples/native-backend/export-smoke/host/wasm-js/main.mjs](../../../examples/native-backend/export-smoke/host/wasm-js/main.mjs)
- [examples/native-backend/export-smoke/host/wasm-rust/src/main.rs](../../../examples/native-backend/export-smoke/host/wasm-rust/src/main.rs)

## Artifact Summary

For a project named `export-smoke`:

Native:

- `build/native/sdk/manifest.json`
- `build/native/sdk/include/export-smoke.h`
- `build/native/sdk/lib/libexport-smoke.a`
- `build/native/sdk/lib/libexport-smoke.so` / `.dylib` / `.dll`

Wasm:

- `build/wasm/sdk/manifest.json`
- `build/wasm/sdk/component/export-smoke.exports.component.wasm`
- `build/wasm/sdk/wit/`
- `build/wasm/sdk/js/export-smoke.exports.component.js`
- `build/wasm/sdk/js/export-smoke.bindings.mjs`
- `build/wasm/sdk/js/export-smoke.bindings.d.ts`

The `build/*/llvm/...` trees are backend build internals. The `build/*/sdk/...` trees are the
public embedding surface.

## Example

The canonical in-tree example is:

- [examples/native-backend/export-smoke/README.md](../../../examples/native-backend/export-smoke/README.md)

It includes one Flix export library and three host embeddings:

- native C
- wasm JS
- wasm Rust / Wasmtime

## Current Limitations

- The public export ABI is still intentionally small.
- Suspension inspection is still mixed-mode. Typed `resume-*` and `request-*` helpers are available
  only when the compiler can prove a stable portable suspension contract; otherwise hosts still use
  the generic `value` / `suspension-arg-*` fallback.
- JVM `@Export` is not yet on the same contract as native/wasm.

Those are the next areas to improve. The current implementation is good enough to embed real typed
Flix exports from C, JS/TS, and Wasmtime hosts without exposing backend-only machinery.
