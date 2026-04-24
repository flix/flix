# Native Backend Proof of Concept

This document summarizes what the LLVM native/wasm proof of concept currently demonstrates and what it does not.

The goal is not to claim JVM parity or production readiness. The goal is to show that Flix can plausibly support a real native/wasm backend family with coherent project workflows, runtime semantics, embedding, and interop.

## AI Warning / Disclaimer

I did this by guiding AI (specifically GPT-5.4-xhigh). I am not a compiler expert, an llvm expert, a c expert, wasm expert, a Scala expert, or a Flix expert, although I have tinkered in all of those areas. This was done because I think Flix has a lot of great ideas but I also think it is constrained by being tied to the JVM. I wanted to see if it was possible and learn some things along the way, which I did.

I think it's largely solid and well tested to the best of my knowledge and available time, but if there's no interest in evaluating code written largely via LLM I completely understand.

## What This Proof Of Concept Demonstrates

- Flix can target both `llvm-native` and `llvm-wasm` from real project roots.
- The same backend family can support:
  - native executables and libraries,
  - wasm components for browser/JS hosts,
  - wasm components for Wasmtime/Rust hosts.
- `@Export` can act as a real cross-target embedding surface instead of raw backend internals.
- Native FFI can be made reasonably ergonomic through manifest-driven binding generation and curated bridge packages.
- Async wasm imports can use generated Flix effects and host adapters instead of pretending browser/WASI host calls are ordinary synchronous imports.
- The runtime can already cover enough substrate to make the backend credible:
  - timers,
  - HTTP,
  - TCP,
  - file operations,
  - process operations,
  - cancellation,
  - region-based concurrency,
  - and an initial portable `Sync/*` surface.

## Language Feature Support

Because the native/wasm backend reuses the existing Flix frontend and lowerer,
most language-level features carry over. The scope below reflects what is
actually exercised by in-tree tests and examples on this branch.

**Supported end-to-end:**

- algebraic data types and pattern matching
- first-class functions, closures, higher-order functions
- parametric polymorphism (erasure, same model as the JVM backend)
- **effect handlers** — user-defined algebraic effects with `run ... with handler`
  and `resume` work on both `llvm-native` and `llvm-wasm`. See
  [counter_effect](../../../main/test/flix/native/apps/counter_effect/Main.flix)
  for a handler with resumption and local state, and `WasmAsyncEffectsLlvmWasmSuite`
  plus [wasm-effects-smoke](../../../examples/native-backend/wasm-effects-smoke/README.md)
  for effects crossing the async wasm host boundary. The Zig runtime provides
  the continuation/suspension machinery
  ([continuation_roots_v0.zig](../../../runtime/src/continuation_roots_v0.zig))
- regions and region-scoped mutable state (`Ref`)
- records, tuples, structs
- arrays and iteration
- channels, `spawn`, and region-based concurrency
- exceptions (`throw` / `try..catch`)
- full tail-call elimination (exercised by
  [tce_mutual](../../../main/test/flix/native/apps/tce_mutual/Main.flix)
  with mutual recursion over hundreds of thousands of iterations)
- async I/O — timers, HTTP, TCP, file and process operations, cancellation —
  via the Zig runtime
- async wasm imports via generated Flix effect bindings for WIT-style hosts
- typed `@Export` values crossing into native and wasm hosts

**Partial — behind the portable profile / ABI:**

- standard library — only the `portable` split
  ([main/src/library/portable](../../../main/src/library/portable)) is available
  on non-JVM targets. A meaningful slice of ordinary Flix stdlib code compiles
  unchanged; the rest still needs to move behind the portable split.
- export/import ABI — scalars, strings, records, tuples, arrays, and a curated
  set of ADTs cross the `@Export`/import boundary today. Arbitrary generic
  values, richer ADTs, and general callback/resource conventions are not yet in
  scope. See [EXPORTS.md](EXPORTS.md) and [IMPORTS.md](IMPORTS.md).
- async wasm imports cover the current portable/WIT slice, not arbitrary WIT
  surface area.

**Not supported (by design, on non-JVM targets):**

- JVM interop: `new` on JVM classes, `unsafe import java ...`, `java.*` types,
  and the JVM-only `Sync/*` and `Concurrent/*` stdlib modules under
  [main/src/library/jvm](../../../main/src/library/jvm). The portable profile
  rejects these.
- wasm multi-threading — the wasm runtime is currently single-threaded. Native
  threads and spawn-based concurrency do work.

Anything not listed above (e.g. `Fixpoint`/Datalog, lazy evaluation) has not
been specifically validated on this backend yet and should be treated as
untested rather than supported.

## Current Usability

Today this looks usable for:

- proof-of-concept applications,
- embeddable libraries,
- CLI tools and services,
- host-integration experiments,
- and FFI-heavy examples that stay within the documented surface.

Representative in-tree examples:

- [export-smoke](../../../examples/native-backend/export-smoke/README.md)
- [wasm-effects-smoke](../../../examples/native-backend/wasm-effects-smoke/README.md)
- [wasm-effects-recursive-smoke](../../../examples/native-backend/wasm-effects-recursive-smoke/README.md)
- [sqlite-cross-target](../../../examples/native-backend/sqlite-cross-target/README.md)

## How To Try It

The current docs are split by concern:

- this file explains scope and what the proof of concept demonstrates
- [../../../examples/native-backend/README.md](../../../examples/native-backend/README.md) is the runnable entry point
- [IMPORTS.md](IMPORTS.md) explains how to build native/wasm interop surfaces
- [EXPORTS.md](EXPORTS.md) explains the host-facing `@Export` surface

Recommended prerequisites:

- a working JDK/Gradle setup
- a working `zig` on `PATH`
- Node.js for the JS/browser wasm hosts
- Rust + Cargo for the Wasmtime hosts
- `pkg-config` and `sqlite3` for the SQLite native example

Build the repo-local `flix` launcher once from repo root:

```bash
./gradlew installDist
export FLIX="$PWD/build/install/flix/bin/flix"
```

Recommended targeted validation from repo root:

```bash
./gradlew test \
  --tests ca.uwaterloo.flix.LlvmNativeExportSuite \
  --tests ca.uwaterloo.flix.LlvmWasmExportSuite \
  --tests ca.uwaterloo.flix.NativeBindingsToolSuite \
  --tests ca.uwaterloo.flix.WasmEffectBindingsToolSuite \
  --tests ca.uwaterloo.flix.WasmAsyncEffectsLlvmWasmSuite \
  --tests ca.uwaterloo.flix.SqliteCrossTargetSuite
```

Then pick one of these hands-on paths:

- typed exports:
  - [../../../examples/native-backend/export-smoke/README.md](../../../examples/native-backend/export-smoke/README.md)
- async wasm imports:
  - [../../../examples/native-backend/wasm-effects-smoke/README.md](../../../examples/native-backend/wasm-effects-smoke/README.md)
  - [../../../examples/native-backend/wasm-effects-recursive-smoke/README.md](../../../examples/native-backend/wasm-effects-recursive-smoke/README.md)
- native FFI / bridge package workflow:
  - [../../../examples/native-backend/sqlite-cross-target/README.md](../../../examples/native-backend/sqlite-cross-target/README.md)

If the goal is specifically “build a small native project with FFI,” start with the SQLite example's
`native-bridge/` and `native/` pair, then adapt the curated header, sidecar spec, and bridge source
to your target C library.

## Reviewer Guide

This branch is large. The most efficient review order is:

1. Read this file for scope, non-goals, and trial instructions.
2. Read [EXPORTS.md](EXPORTS.md) for the proposed host-facing embedding surface.
3. Read [IMPORTS.md](IMPORTS.md) for the proposed native FFI and async wasm import story.
4. Skim the runnable examples under [../../../examples/native-backend/README.md](../../../examples/native-backend/README.md).
5. Review the commits in order:
   - backend/runtime substrate
   - portable stdlib/concurrency
   - binding generation / interop tooling
   - examples and tests
   - proposal docs

If the question is whether this is worth pursuing at all, I would focus first on architecture and
surface design, not on whether every subsystem is already production-grade.

## Implementation Map

If you want to navigate the code rather than the docs first, these are the main areas:

- compiler / CLI integration:
  - [Main.scala](../../../main/src/ca/uwaterloo/flix/Main.scala)
  - [Flix.scala](../../../main/src/ca/uwaterloo/flix/api/Flix.scala)
  - [Bootstrap.scala](../../../main/src/ca/uwaterloo/flix/api/Bootstrap.scala)
  - [Options.scala](../../../main/src/ca/uwaterloo/flix/util/Options.scala)
- LLVM lowering and codegen:
  - [Lowerer.scala](../../../main/src/ca/uwaterloo/flix/language/phase/Lowerer.scala)
  - [LlvmBackend.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmBackend.scala)
  - [LlvmNativeDriver.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmNativeDriver.scala)
  - [LlvmWasmDriver.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmWasmDriver.scala)
- export/import ABI shaping:
  - [ExportAbi.scala](../../../main/src/ca/uwaterloo/flix/language/phase/ExportAbi.scala)
  - [NativeImportAbi.scala](../../../main/src/ca/uwaterloo/flix/language/phase/NativeImportAbi.scala)
  - [WasmImportAbi.scala](../../../main/src/ca/uwaterloo/flix/language/phase/WasmImportAbi.scala)
  - [LlvmExportWriter.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmExportWriter.scala)
  - [LlvmWasmTypedExportsWriter.scala](../../../main/src/ca/uwaterloo/flix/language/phase/llvm/LlvmWasmTypedExportsWriter.scala)
- runtime substrate:
  - [flix_rt_llvm.zig](../../../runtime/src/flix_rt_llvm.zig)
  - [continuation_roots_v0.zig](../../../runtime/src/continuation_roots_v0.zig)
  - [handshake_v0.zig](../../../runtime/src/handshake_v0.zig)
  - [rt_xev.zig](../../../runtime/src/rt_xev.zig)
- interop and binding generation:
  - [NativeBindingsTool.scala](../../../main/src/ca/uwaterloo/flix/tools/NativeBindingsTool.scala)
  - [WasmEffectBindingsTool.scala](../../../main/src/ca/uwaterloo/flix/tools/WasmEffectBindingsTool.scala)
  - [BindingConfig.scala](../../../main/src/ca/uwaterloo/flix/util/BindingConfig.scala)
  - [PkgConfig.scala](../../../main/src/ca/uwaterloo/flix/util/PkgConfig.scala)
- portable stdlib split:
  - [portable](../../../main/src/library/portable)
  - [Sync.flix](../../../main/src/library/Sync.flix)
  - [Sync](../../../main/src/library/Sync)
  - [jvm](../../../main/src/library/jvm)
- examples and end-to-end tests:
  - [examples/native-backend](../../../examples/native-backend)
  - [experimental/nativebackend](../../../main/test/ca/uwaterloo/flix/experimental/nativebackend)
  - `Portable*` suites under [main/test/ca/uwaterloo/flix](../../../main/test/ca/uwaterloo/flix)

## What It Does Not Yet Claim

This proof of concept does **not** yet claim:

- drop-in support for arbitrary existing JVM-oriented Flix projects,
- full stdlib parity with the JVM backend,
- production-grade GC/runtime maturity,
- a complete export/import ABI for every Flix type,
- complete packaging/distribution conventions for every cross-target dependency scenario.

## What Remains For A Complete Backend

To treat this as a complete backend for the language rather than a proof of concept, the main
remaining work is:

- portable-profile completion:
  - non-JVM targets currently rely on the portable stdlib profile
  - JVM interop and JVM-specific types are intentionally rejected there
  - more stdlib surface still needs to move behind the portable split so a much larger fraction of
    ordinary Flix code compiles unchanged
- runtime maturity:
  - the current runtime is still a non-moving mark/sweep design triggered at pollchecks
  - it needs more hardening, performance work, and operational confidence before it should be
    treated as production-ready
  - the wasm runtime is still single-threaded; there is no wasm-threads story yet
- ABI and language-surface completion:
  - exports/imports still intentionally exclude parts of the type surface such as arbitrary ADTs,
    generic values, and richer callback/resource conventions
  - the async wasm import generator covers the current portable/WIT slice, not arbitrary WIT
    surface area
  - JVM `@Export` is not yet on the same public contract as native/wasm
- tooling and packaging:
  - the project workflow is coherent, but dependency distribution and host/toolchain ergonomics are
    not yet finished enough to call this a complete product story

## Relevant Docs

- [IMPORTS.md](IMPORTS.md): native FFI and wasm effect-binding story.
- [EXPORTS.md](EXPORTS.md): native/wasm embedding and `@Export` contract.

## Why I hope this is worth pursuing

This is more than just "hello world" on llvm, it demonstrates a (hopefully) viable llvm/wasm backend with:

- project workflows,
- runtime behavior,
- embedding,
- native FFI,
- async wasm imports,
- and portable synchronization.
