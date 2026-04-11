# WIT Packages

Status: source of truth for Flix wasm/component interfaces
Last updated: 2026-03-10

This directory contains the authored WIT packages for the Flix LLVM backend’s wasm/component story.

Goals:

- Make interop surfaces **real** (actual signatures) so we can iterate early.
- Keep the initial shape compatible with:
  - WASI 0.3 / P3 hosts (Wasmtime),
  - browser hosts (via component→JS tooling),
  - and our existing native C ABI planning.

Current packages:

- `flix:sys@0.1.0` (`runtime/wit/flix-sys/sys.wit`)
  - Minimal host capability surface (log/time/random + capabilities).
- `flix:runtime@0.1.0` (`runtime/wit/flix-runtime/runtime.wit`)
  - Handle-based runtime API:
    - `ctx` / `value` / `suspension` resources
    - `invoke` (direct call; may suspend with a `task-id`)
    - task scheduler surface (`start-task` / `sched-step` / `poll-task`)
    - typed per-op suspension payloads (`suspension-request`) + typed resumers
      - `resume-timer-sleep`
      - `resume-http-ok` / `resume-http-err`
      - Filesystem (portable `%%FILE_*%%` primops): typed requests + `resume-file-*` resumers (see `op-catalog.md`)
      - Process: typed requests + `resume-process-*` resumers (see `op-catalog.md`)
      - TCP:
        - `resume-tcp-socket-connect-ok` / `resume-tcp-socket-connect-err`
        - `resume-tcp-socket-read-ok` / `resume-tcp-socket-read-err`
        - `resume-tcp-socket-write-ok` / `resume-tcp-socket-write-err`
        - `resume-tcp-socket-close-ok` / `resume-tcp-socket-close-err`
        - `resume-tcp-server-bind-ok` / `resume-tcp-server-bind-err`
        - `resume-tcp-server-accept-ok` / `resume-tcp-server-accept-err`
        - `resume-tcp-server-local-port-ok` / `resume-tcp-server-local-port-err`
        - `resume-tcp-server-close-ok` / `resume-tcp-server-close-err`
    - `resume-ok` / `resume-throw`
    - primitive boxing/unboxing
- `flix:bindings@0.1.0` (`runtime/wit/flix-bindings/bindings.wit`)
  - Aggregate world that **imports** `flix:sys` and **exports** `flix:runtime`.
  - `runtime/wit/flix-bindings/deps/` currently vendors the `flix:sys` and `flix:runtime` package definitions for local validation.

Notes:

- The `flix:runtime` interface is intentionally the untyped substrate (handle-based).
- Typed per-`@Export` bindings are generated on top of that substrate; they are not authored here by hand.
- The canonical “component boundary world” is currently `flix:bindings/world flix`.
- `runtime/wit/op-catalog.md` records the mapping from portable primops → typed WIT ops.

Validation (requires `wasm-tools`):

```bash
wasm-tools component wit runtime/wit/flix-sys
wasm-tools component wit runtime/wit/flix-runtime
wasm-tools component wit runtime/wit/flix-bindings
```

Regeneration of the checked-in C glue (`runtime/src/wit/flix.c`, `runtime/src/wit/flix.h`) uses a
pinned `wit-bindgen-cli` version:

```bash
./runtime/tools/regenerate_wit_glue.sh
./runtime/tools/regenerate_wit_glue.sh --check
```

Notes:

- The script installs `wit-bindgen-cli 0.53.1` with `cargo +stable` into `build/tools/` if needed.
- We intentionally regenerate only `flix.c` and `flix.h`.
  The temporary `flix_component_type.o` emitted by `wit-bindgen` is not checked in; the runtime
  provides the link anchor directly in `runtime/src/flix_rt_llvm.zig`.
