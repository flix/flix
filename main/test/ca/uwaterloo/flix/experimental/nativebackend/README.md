# Experimental Native Backend Tests

This subtree groups the suites that primarily exercise the LLVM native/wasm proof of concept:

- backend-driven application examples,
- export embedding,
- native and wasm binding generation,
- Zig runtime substrate tests,
- wasm import/effect integration,
- and cross-target example applications and program suites.

Subdirectories:

- `apps`: end-to-end example applications such as weather and langcensus.
- `programs`: backend-targeted program suites.
- `examples`: cross-target example-package suites.
- `exports`, `ffi`, `runtime`, `wasm`: focused backend surface tests.

The Scala package remains `ca.uwaterloo.flix` to avoid unnecessary package churn while still keeping the filesystem layout readable.
