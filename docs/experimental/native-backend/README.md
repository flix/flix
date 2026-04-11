# Native Backend Proof of Concept

This document summarizes what the LLVM native/wasm proof of concept currently demonstrates and what it does not.

The goal is not to claim JVM parity or production readiness. The goal is to show that Flix can plausibly support a real native/wasm backend family with coherent project workflows, runtime semantics, embedding, and interop.

## Warning / Disclaimer

I did this by guiding AI (specifically GPT-5.4-xhigh). I am not a compiler expert, an llvm expert, a c expert, wasm expert, a Scala expert, or a Flix expert, although I have tinkered in all of those areas. This was done because I think Flix has a lot of great ideas but I also think it is very limited by being tied to the JVM. I wanted to see if it was possible and learn some things along the way, which I did.

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

## What It Does Not Yet Claim

This proof of concept does **not** yet claim:

- drop-in support for arbitrary existing JVM-oriented Flix projects,
- full stdlib parity with the JVM backend,
- production-grade GC/runtime maturity,
- a complete export/import ABI for every Flix type,
- complete packaging/distribution conventions for every cross-target dependency scenario.

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

