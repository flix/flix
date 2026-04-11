# SQLite Native Bridge Package

This package contains the reusable native SQLite bridge for the cross-target
SQLite example.

It owns the native interop surface:

- curated C header
- sidecar binding spec
- thin handwritten `sqlite3` bridge source
- generated `Sqlite` Flix bindings under `build/native/generated/...`

Build or package it directly with:

```bash
flix check --target native
flix build-pkg
```

The intended consumer workflow is a local path dependency from another Flix
native project during workspace development:

```toml
[dependencies]
"sqlite-bridge" = { path = "../native-bridge", security = "unrestricted" }
```

For publishable reuse, package this bridge with `flix build-pkg` and consume the
resulting `.fpkg` / released package instead. Local path dependencies are a
workspace convenience, not something `build-pkg` will carry through another
package graph.
