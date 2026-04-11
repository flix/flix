# Native SQLite Example

This project is the native half of the cross-target SQLite example.

It consumes the reusable sibling bridge package at:

- [../native-bridge](../native-bridge)

That package owns the native interop contract and bridge implementation:

- curated bridge header
- sidecar binding spec
- handwritten `sqlite3` bridge C source
- generated `Sqlite` Flix bindings

## Build Prerequisites

This project is intentionally thin:

- `flix.toml` declares a local path dependency on `../native-bridge`
- local path dependencies are a workspace/development feature, not a publishable package feature yet
- the only external system dependency remains `sqlite3` with a working `pkg-config` entry, but that requirement lives in the bridge package now

Current flow:

```bash
flix check
flix build --target native
./build/native/llvm/sqlite-native
```

Expected output:

```text
1:hello:4
```
