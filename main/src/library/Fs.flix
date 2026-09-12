/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

/// ## Effect Hierarchy
///
/// `[D]` = has `@DefaultHandler`.
///
/// ```
/// FileSystem [D]                      (29 ops — unified root)
/// ├── FileStat [D]                    (11 ops)
/// │   ├── FileTest [D]                (4 ops)
/// │   │   ├── FileExists                    — exists
/// │   │   ├── IsDirectory                   — isDirectory
/// │   │   ├── IsRegularFile                 — isRegularFile
/// │   │   └── IsSymbolicLink                — isSymbolicLink
/// │   ├── FilePermission [D]          (3 ops)
/// │   │   ├── IsReadable                    — isReadable
/// │   │   ├── IsWritable                    — isWritable
/// │   │   └── IsExecutable                  — isExecutable
/// │   ├── FileTime [D]                (3 ops)
/// │   │   ├── AccessTime                    — accessTime
/// │   │   ├── CreationTime                  — creationTime
/// │   │   └── ModificationTime              — modificationTime
/// │   └── FileSize                          — size
/// ├── FileRead [D]                    (3 ops)
/// │   ├── ReadFile                          — read
/// │   ├── ReadLines                         — readLines
/// │   └── ReadBytes                         — readBytes
/// ├── DirList [D]                           — list
/// ├── Glob [D]                              — glob
/// └── FileWrite [D]                   (13 ops)
///     ├── WriteFile                         — write
///     ├── WriteLines                        — writeLines
///     ├── WriteBytes                        — writeBytes
///     ├── AppendFile                        — append
///     ├── AppendLines                       — appendLines
///     ├── AppendBytes                       — appendBytes
///     ├── Truncate                          — truncate
///     ├── CopyFile                          — copyWith
///     ├── MoveFile                          — moveWith
///     ├── DeleteFile                        — delete
///     ├── MkDir                             — mkDir
///     ├── MkDirs                            — mkDirs
///     └── MkTempDir                         — mkTempDir
/// ```
///
/// ## Middleware Availability
///
/// |                   | FileTest | FilePermission | FileTime | FileStat | FileRead | DirList | Glob | FileWrite | FileSystem |
/// |-------------------|:--------:|:--------------:|:--------:|:--------:|:--------:|:-------:|:----:|:---------:|:----------:|
/// | withLogging       | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withBaseDir       | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withChroot        | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withAllowList     | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withDenyList      | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withAllowGlob     | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withDenyGlob      | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withFollowLinks   | x        | x              | x        | x        | x        | x       | x    | x         | x          |
/// | withTransferLimit |          |                |          |          | x        |         |      | x         | x          |
/// | withChecksum      |          |                |          |          | x        |         |      | x         | x          |
/// | withDryRun        |          |                |          |          |          |         |      | x         | x          |
/// | withReadOnly      |          |                |          |          |          |         |      | x         | x          |
/// | withAtomicWrite   |          |                |          |          |          |         |      | x         | x          |
/// | withBackup        |          |                |          |          |          |         |      | x         | x          |
/// | withConflictCheck |          |                |          |          |          |         |      | x         | x          |
/// | withMkParentDirs  |          |                |          |          |          |         |      | x         | x          |
/// | withSizeRotation  |          |                |          |          |          |         |      | x         | x          |
/// | withMemoryOverlay |          |                |          |          |          |         |      |           | x          |
/// | withInMemoryFS    |          |                |          |          |          |         |      |           | x          |
pub mod Fs {
    /* empty */
}
