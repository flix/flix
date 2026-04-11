# Flix Native Runtime (Zig)

This directory contains the committed Zig runtime modules that back the experimental LLVM native/wasm backend.

The runtime surface here includes:

- continuation rooting and remembered-set mechanics,
- pollcheck / handshake coordination,
- async wait and `libxev` integration,
- file, process, TCP, and HTTP substrate slices,
- and the shared `flix_rt_llvm.zig` runtime entrypoints.

## Run Focused Zig Tests

From repo root:

- `zig test runtime/src/continuation_roots_v0.zig`
- `zig test runtime/src/handshake_v0.zig`
- `zig test runtime/src/rt_xev.zig`

The Scala regression suite in `NativeRuntimeZigSuite` stages those same modules into an isolated cache-backed test run.

Exploratory spike harnesses are kept out of the committed runtime surface.
