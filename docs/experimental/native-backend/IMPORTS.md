# Imports and FFI

For the overall native/wasm proof-of-concept status and scope, see [README.md](README.md).

This document describes the current import-side interop story for the LLVM backends.

## Direct Sync Imports

Direct imports are the target-specific escape hatch for synchronous foreign calls.

### Native

```flix
extern native(symbol = "abs")
def cAbs(x: Int32): Int32
```

Current `extern native` scope:

- target: `llvm-native`
- security context: `unrestricted`
- semantics:
  - scalar-only signatures use the raw direct C ABI
  - `String` / `Array[Int8, Static]` (`Bytes`) use a native bridge ABI
- type surface:
  - `Unit`
  - `Bool`
  - `Int8 / Int16 / Int32 / Int64`
  - `Float32 / Float64`
  - `String`
  - `Array[Int8, Static]` as `Bytes`

For bridge-ABI imports, the foreign symbol is expected to use the same native handle/helper surface
as exports:

- a hidden leading `flix_ctx_t*` parameter
- `flix_string_t` for `String`
- `flix_bytes_t` / `flix_i8_array_t` for `Array[Int8, Static]`
- the existing runtime helpers such as `flix_string_from_utf8`, `flix_string_to_utf8`,
  `flix_i8_array_from_bytes`, and `flix_i8_array_to_bytes`

That keeps the public native ABI aligned with exports, but it also means direct `String` / `Bytes`
imports still target Flix-aware C shims, not arbitrary raw `const char*` library functions yet.

Generate native bindings from a curated C header with:

```bash
flix bind native --header <header.h> --out <out-dir>
```

For ordinary projects, the intended workflow is now manifest-driven instead of script-driven:

```toml
[[bindings.native]]
header = "bridge/native_api.h"
module = "Native"
spec = "bridge/native_api.bind.toml"
include = ["bridge/include"]

[target.native]
compile-sources = ["bridge/native_api.c"]
compile-include = ["bridge/include"]
pkg-config = ["sqlite3"]
```

Typical sidecar spec shape:

```toml
[[binding]]
symbol = "widget_open"
result = "status-owned-handle"
type = "Widget"
out = "out_widget"
ok = "0"
effect = "IO"
```

Then `flix check` / `flix build --target native` will:

- generate bindings under `build/native/generated/bindings/native/...`
- add the generated Flix module to the project automatically
- pass project-level `pkg-config` C flags through native header translation and generated shim compilation
- compile any generated shim plus the declared bridge C/C++ sources
- and link the result with the declared native libraries/frameworks

`flix package` / `flix build-pkg` now also preserve the native interop bundle needed to reuse that
bridge from another project:

- the generated Flix binding module
- any generated native shim / copied shim header
- the manifest-referenced curated bridge assets:
  - native headers
  - sidecar binding specs
  - declared bridge C/C++ sources
  - declared include trees

When a consuming project depends on such a package in project mode, the native build now aggregates
the dependency's declared `[target.native]` compile/link config and compiles its packaged bridge
sources automatically. This is the intended v0 bridge-package workflow.

There are now two distinct reuse modes:

- workspace/local development:
  - a project may depend on a sibling bridge package with a local path dependency such as
    `"sqlite-bridge" = { path = "../native-bridge", security = "unrestricted" }`
  - the consuming build loads that dependency's Flix sources directly from the local project,
    refreshes its generated bindings for the active target, and aggregates its declared native
    compile/link config
- publishable package reuse:
  - the producer package must run `flix package` / `flix build-pkg`
  - consumers then depend on the produced `.fpkg` / released package instead of a local path

This distinction is intentional. `build-pkg` rejects local path dependencies. A package that points
at `../some-local-dir` is a workspace convenience, not something we should publish and pretend is
portable.

Useful options:

- `--native-module <Name>`
  Root Flix module for the generated bindings. Default: `Native`.
- `--spec <file>`
  Sidecar TOML spec for ownership/effect/callback metadata.
- `--include <dir>`
  Additional C include path. Repeat as needed.
- `--define NAME[=VALUE]`
  Additional preprocessor definition. Repeat as needed.
- `--cflag <flag>`
  Additional C compiler flag passed through to `zig translate-c`. Repeat as needed.

`bind native` currently generates:

- `flix/<Module>.flix`
  A Flix module containing:
  - private raw `extern native` declarations, and
  - public wrapper defs that call those raw imports.
- `native/<Module>_shim.c` when shim-backed bindings are needed.
- `native/include/<header>.h` as a copied curated header for compiling that shim.

Current `bind native` v0 scope:

- uses `zig translate-c` as the header parsing / normalization substrate
- uses an explicit sidecar TOML spec for ownership/effect/callback metadata
- expects a curated header surface, not an arbitrary umbrella header
- expects semantics to live in the sidecar spec, not in C comments
- emits direct bindings for declarations whose translated signatures fit the scalar/unit slice:
  - `Unit`
  - `Bool`
  - `Int8 / Int16 / Int32 / Int64`
  - `Float32 / Float64`
- emits shim-backed bindings for the first ordinary borrowed-input C patterns:
  - `const char*` input becomes `String`
  - `const uint8_t*` / `const int8_t*` / `const unsigned char*` plus a following supported length parameter becomes `Array[Int8, Static]`
- emits shim-backed bindings for the first honest synchronous callback pattern:
  - sidecar fields:
    - `callback = "<param>"`
    - `callback-export = "<Module.def>"`
  - on a function whose callback parameter is a translated C callback typedef
  - the callback target must be a Flix `@Export`ed def
  - the generated public Flix wrapper does not expose a raw function-pointer value; it binds that callback parameter to the named exported def
  - current callback scope is intentionally narrow:
    - synchronous callbacks only
    - the foreign function must not retain the callback pointer beyond the call
    - callback parameter/result types must stay in the scalar/unit slice
    - if the exported callback throws or suspends, the generated shim reports it and aborts because a plain C callback ABI has nowhere to carry `thrown` / `suspended`
- emits shim-backed bindings for explicit curated string-result ownership annotations:
  - sidecar `result = "borrowed-string"` on `const char* f(void)` or `char* f(void)` becomes `(): String`
  - sidecar `result = "owned-string"` plus `free = "<symbol>"` on `char* f(void)` becomes `(): String`, and the generated shim calls the named free function after bridging
- emits shim-backed bindings for explicit curated bytes-result ownership annotations:
  - sidecar `result = "borrowed-bytes"` plus `len = "<out-param>"` on `const uint8_t* f(size_t* out_len)` or the equivalent `int8_t*` / `unsigned char*` shape becomes `(): Array[Int8, Static]`
  - sidecar `result = "owned-bytes"` plus `len = "<out-param>"` and `free = "<symbol>"` on a mutable byte-pointer return plus a supported length out-param becomes `(): Array[Int8, Static]`
- emits shim-backed bindings for explicit owned/borrowed opaque handle outputs:
  - sidecar `result = "owned-handle"` plus `type = "<Handle>"` on an opaque pointer factory such as `widget_t* f(...)` becomes `(...): Option[Handle]`
  - sidecar `result = "borrowed-handle"` plus `type = "<HandleRef>"` on a `const widget_t*` factory such as `const widget_t* f(...)` becomes `(...): Option[HandleRef]`
  - sidecar `result = "status-owned-handle"` plus `type = "<Handle>"`, `out = "<out-param>"`, and optional `ok = "<literal>"` on a status-returning creator such as `int32_t widget_open(args..., widget_t** out_widget)` becomes `(...): Result[Int32, Handle]`
  - the status type stays whatever scalar C status type the declaration already uses; the generator does not collapse it to `Bool`
  - `<out-param>` must lower to a known opaque out-handle shape, and `ok` defaults to `0` when omitted
  - later functions over the same opaque pointer type automatically use:
    - the borrowed handle type for `const widget_t*` parameters when one exists
    - the owned handle type for mutable `widget_t*` parameters
  - when both owned and borrowed handle types exist for the same opaque C type, the generator emits an explicit owned-to-borrowed helper such as `borrowWidget(handle: Widget): WidgetRef`
  - handle-only shims stay on the raw scalar ABI; they do not force the native bridge `ctx` path unless `String` / `Bytes` helpers are also needed
- emits canonical close helpers for explicit one-arg owned-handle destroy functions:
  - sidecar `effect = "IO"` plus `destroy = "<param>"`
    on a function whose public wrapper is exactly `(<Handle>): Unit \ IO`
  - this generates an alias such as `closeWidget(handle: Widget): Unit \ IO`
  - this is an ergonomic naming/lifetime convention only; it does not add linearity, finalizers, or automatic cleanup
- emits canonical retain helpers for explicit one-arg owned-handle retain/clone functions:
  - sidecar `result = "owned-handle"`, `type = "<Handle>"`, `retain = "<param>"`, and `effect = "IO"`
    on a function whose public wrapper is exactly `(<BorrowedHandle or OwnedHandle>): Option[Handle] \ IO`
  - if the underlying C API takes `const widget_t*`, the generated raw/public wrapper still uses the borrowed handle type when one exists
  - the generator additionally emits an owned-handle alias such as `retainWidget(handle: Widget): Option[Widget] \ IO`
- treats owner-tied borrowed results as first-class generator metadata:
  - sidecar `result = "borrowed-string"` plus `borrowed-from = "<param>"`
  - sidecar `result = "borrowed-bytes"` plus `len = "<out-param>"` and `borrowed-from = "<param>"`
  - sidecar `result = "borrowed-handle"` plus `type = "<HandleRef>"` and `borrowed-from = "<param>"`
  - the annotation requires `<param>` to lower to a known owned/borrowed handle family
  - when the public wrapper would otherwise take the borrowed handle type, the generator emits an additional owned-handle convenience wrapper such as `widgetNameOwned(handle: Widget): String`
  - this improves ergonomics for read-only resource methods without pretending Flix now enforces lifetimes for borrowed views
- supports explicit effect marking where the foreign call is semantically impure:
  - sidecar `effect = "IO"`
  - this can be combined with result annotations, e.g. `result = "owned-handle"` plus `type = "Widget"` and `effect = "IO"`
  - this is based on library semantics, not just the C signature:
    - a `const widget_t*` getter can still require `IO` if it observes mutable resource state
    - borrowed results tied to statement/database handles are often `IO` for exactly that reason
- keeps the remaining pointer/resource surface conservative:
  - raw pointer results without an explicit ownership annotation are skipped
  - raw opaque pointer results without an explicit handle ownership annotation are skipped
- records skipped declarations and the reason in the generated file comments

That means unsigned integers, variadic imports, retained/asynchronous callbacks, richer opaque handle/resource conventions,
and pointer-return ownership policies are still out of scope even though the compiler-side native
import ABI now supports `String` / `Bytes` and recursive portable values through the bridge convention above.

Typical native flow:

1. Declare `[[bindings.native]]` in `flix.toml`.
   Include `spec = "bridge/native_api.bind.toml"` when the bridge needs ownership/effect/callback metadata.
2. Declare any bridge sources under `[target.native]`:
   - `compile-sources`
   - `compile-include`
   - `compile-cflags`
3. Declare link settings under `[target.native]`:
   - `pkg-config`
   - `link-libs`
   - `link-search`
   - `link-flags`
   - `frameworks`
   - `framework-search`
4. Run `flix check` or `flix build --target native`.

`pkg-config` is the preferred path for system libraries when a `.pc` file exists. It feeds both
bridge compilation and final linking. `link-libs` / `link-search` / `link-flags` remain the manual
escape hatch for libraries that do not participate in `pkg-config`.

The standalone `bind native` command remains useful for inspection/debugging and for generating a
bundle outside a project build, but it is no longer the intended day-to-day project workflow.

For reusable bridge packages, the intended workflow is:

1. For workspace development, depend on the bridge package by local path.
2. For publishable reuse, declare `[[bindings.native]]` and `[target.native]` in the bridge package
   manifest and run `flix package`.
3. Publish/distribute the resulting `.fpkg` and manifest.
4. Depend on that package from a consuming native project.

Current package-reuse boundary:

- local path dependencies are supported for workspace/development use
- local path dependencies are resolved relative to the manifest that declares them
- local path dependencies are **not** publishable; `build-pkg` rejects them
- native bridge packages are now first-class
- consuming builds inherit dependency native compile/link config
- dependency bindings are **not** regenerated transitively; the package is expected to carry its
  generated Flix/shim outputs already
- wasm host implementations are still host/application assets, not something the guest build
  compiles transitively from dependencies

Current generator coverage:

- ordinary borrowed `String` / `Bytes` inputs are now covered by generated shims
- annotated string and byte outputs are now covered too
- owned and borrowed opaque handle generation are now covered too
- canonical close-helper generation for simple owned-handle destructors is now covered too
- status-plus-out-handle creators are now covered too, so a common `status + T** out` C creation API can become `Result[Status, Handle]` instead of a lossy `Option[Handle]`
- canonical retain-helper generation and owner-tied borrowed-result helpers are now covered too
Remaining gaps:

- richer multi-step resource conventions beyond simple create/borrow/retain/close helpers
- callback surfaces beyond the current synchronous export-backed trampoline slice
- broader WIT coverage where real host APIs need it

We should not add more guessed pointer patterns without a concrete library surface proving the need.

## FFI Cookbook: Curated Bridge Pattern

For real C libraries, the recommended pattern is a **curated bridge package**, not direct binding to
the library's public umbrella header.

The bridge package should own:

- a small C header that exposes the Flix-facing ABI you actually want
- a sidecar `.bind.toml` file with ownership, effect, callback, and resource metadata
- any small handwritten C shim needed to adapt the library's native API to that ABI
- `[[bindings.native]]` and `[target.native]` manifest entries

The application should then depend on the bridge package rather than duplicating its headers,
binding specs, generated Flix files, or C sources.

Example workspace shape:

```text
my-app/
  flix.toml
  src/
my-lib-bridge/
  flix.toml
  bridge/
    my_lib_bridge.c
  contracts/native/
    my_lib_bridge.h
    my_lib_bridge.bind.toml
```

The app uses a local path dependency while developing both packages together:

```toml
[dependencies]
"my-lib-bridge" = { path = "../my-lib-bridge", security = "unrestricted" }
```

That local dependency is intentionally not publishable. If the app itself should be packaged,
the bridge must be packaged or released separately and the app should depend on that package identity
instead of a local path.

The SQLite cross-target example follows this pattern:

- [sqlite-cross-target/native-bridge](../../../examples/native-backend/sqlite-cross-target/native-bridge)
  owns the native SQLite C bridge, sidecar binding spec, and manifest-driven binding contract.
- [sqlite-cross-target/native](../../../examples/native-backend/sqlite-cross-target/native)
  is a thin native app that depends on the bridge package by local path.
- [sqlite-cross-target/wasm](../../../examples/native-backend/sqlite-cross-target/wasm)
  uses a WIT resource world and generated async effect bindings for browser/Wasmtime hosts.

This split is deliberate. Native FFI is a synchronous C ABI boundary. Browser and Wasmtime host
integration should use WIT resources/effects instead of pretending the raw C ABI is portable.

## Quick Try: Small Native FFI Project

If you want the shortest concrete path for trying native FFI in this branch, use the SQLite example
as the template instead of starting from an empty project.

Prerequisites:

- `./gradlew installDist` from repo root
- `export FLIX="$PWD/build/install/flix/bin/flix"`
- `sqlite3` available through `pkg-config`
- a working `zig` on `PATH`

Then run:

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

To adapt this into your own small project:

1. Copy the structure from
   [sqlite-cross-target/native-bridge](../../../examples/native-backend/sqlite-cross-target/native-bridge).
2. Replace the curated header, sidecar `.bind.toml`, and handwritten bridge C source with your own
   library-specific bridge surface.
3. Keep `[[bindings.native]]` and `[target.native]` in the bridge package manifest.
4. Consume that bridge package from your app through a local path dependency while developing.

That is the intended v0 workflow. We should improve the ergonomics further, but we should not hide
the real bridge boundary behind scripts or ad hoc vendored generated sources.

Practical rules:

- Keep third-party headers out of the public Flix binding surface when the library API is large,
  macro-heavy, callback-heavy, or ownership-heavy.
- Use `pkg-config` for system libraries when available.
- Mark semantically stateful accessors as `effect = "IO"` even if the C signature is `const`.
- Require explicit sidecar metadata for pointer results. Do not infer ownership from C syntax.
- Prefer status-plus-out-handle annotations for APIs shaped like `int open(..., T** out)`.
- Keep C++ behind an `extern "C"` bridge. Do not expose raw C++ ABI details to Flix.

### Wasm

```flix
extern wasm(interface = "host:math/basic@0.1.0", func = "cos")
def cos(x: Float64): Float64
```

Current `extern wasm` sync scope:

- target: `llvm-wasm`
- semantics: synchronous typed WIT/component imports
- type surface: recursive portable ABI parity with the current export surface

Direct imports are not the async story for wasm/browser/WASI-style host interaction.

## Async Wasm Effect Bindings

Async imported WIT worlds use generated Flix effects instead of direct `extern wasm` declarations.

Generate bindings with:

```bash
flix bind wasm-effects --wit <wit-dir> --world <world> --out <out-dir>
```

For ordinary projects, the intended workflow is now manifest-driven:

```toml
[[bindings.wasm]]
wit = "wit"
world = "demo"
module = "Wit"
```

Then `flix check` / `flix build --target wasm` will generate the bindings bundle automatically under
`build/wasm/generated/bindings/wasm/...` and add the generated Flix effect module to the project.

The generated output is a structured SDK bundle:

- `flix/`
  Generated Flix effect source, typically `flix/Wit.flix`.
- `manifest/`
  Binding metadata for unknown-effect dispatch.
- `js/`
  Typed JS/TS host bindings:
  - `js/index.mjs`
  - `js/index.d.ts`
  - `js/package.json`
- `rust/`
  A self-contained Rust/Wasmtime host crate:
  - `rust/Cargo.toml`
  - `rust/src/lib.rs`
  - `rust/src/wit_effect_bindings.rs`
- `wit/`
  A copy of the source WIT world used to generate the bundle.
- `README.md`
  A generated integration guide for the bundle.

Typical flow:

1. Declare `[[bindings.wasm]]` in `flix.toml`.
2. Run `flix check` or `flix build --target wasm`.
3. On the host side:
   - use the generated `js/` bundle for Node/JS integration, or
   - use the generated `rust/` crate for Wasmtime/Rust integration.
4. Use `manifest/wit-effect-bindings.json` together with the wasm build's effect manifest when wiring unknown-effect handlers.

The standalone `bind wasm-effects` command remains available for inspection/debugging and for
generating a bundle outside a project build.

Reference example:

- [wasm-effects-smoke](../../../examples/native-backend/wasm-effects-smoke/README.md)
  End-to-end async wasm effect bindings with:
  - generated Flix bindings
  - a wasm component built from Flix
  - a JS host
  - a Rust/Wasmtime host
- [wasm-effects-recursive-smoke](../../../examples/native-backend/wasm-effects-recursive-smoke/README.md)
  End-to-end async wasm effect bindings over recursive portable values:
  - `list<tuple<string, s32>>`
  - `option<tuple<string, s32>>`
  - `result<tuple<string, s32>, s32>`

## Current Async Wasm Generator Scope

`bind wasm-effects` currently supports:

- imported WIT interfaces
- world-level freestanding imports
- top-level WIT resources
- world-level resource items
- nested owned/borrowed resource handles
- recursive portable ABI types

It does not yet aim to cover every WIT feature. The current focus is a clean async effect path for the portable ABI, not arbitrary WIT surface area.
