/*
 * Copyright 2026 Flix Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// Effect Hierarchy ([D] = has @DefaultHandler):
///
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
///
/// Middleware Availability:
///
///                    FileTest  FilePermission  FileTime  FileStat  FileRead  DirList  Glob  FileWrite  FileSystem
/// withLogging        x         x               x         x         x         x        x     x          x
/// withBaseDir        x         x               x         x         x         x        x     x          x
/// withChroot         x         x               x         x         x         x        x     x          x
/// withAllowList      x         x               x         x         x         x        x     x          x
/// withDenyList       x         x               x         x         x         x        x     x          x
/// withAllowGlob      x         x               x         x         x         x        x     x          x
/// withDenyGlob       x         x               x         x         x         x        x     x          x
/// withFollowLinks    x         x               x         x         x         x        x     x          x
/// withTransferLimit                                                x                        x          x
/// withChecksum                                                     x                        x          x
/// withDryRun                                                                                x          x
/// withReadOnly                                                                              x          x
/// withAtomicWrite                                                                           x          x
/// withBackup                                                                                x          x
/// withConflictCheck                                                                         x          x
/// withMkParentDirs                                                                          x          x
/// withSizeRotation                                                                          x          x
/// withMemoryOverlay                                                                                    x
/// withInMemoryFS                                                                                       x
//
pub mod Fs {

    /// In-memory overlay state, parameterized by region.
    ///
    /// - `files`:     map from normalized absolute paths to in-memory content (text or bytes).
    /// - `dirs`:      set of normalized absolute paths registered as directories.
    /// - `whiteouts`: set of normalized absolute paths deleted in the overlay (hides lower-layer entries).
    /// - `counter`:   monotonically increasing counter for generating unique temp directory names.
    pub struct MemoryOverlay[r: Region] {
        mut files: Map[String, MemoryOverlay.OverlayEntry],
        mut dirs: Set[String],
        mut whiteouts: Set[String],
        mut counter: Int32
    }

    /// Fully in-memory filesystem state, parameterized by region.
    ///
    /// - `files`:   map from normalized absolute paths to in-memory content (text or bytes).
    /// - `dirs`:    set of normalized absolute paths registered as directories.
    /// - `times`:   map from normalized absolute paths to `{atime, ctime, mtime}` records (ms since epoch).
    /// - `counter`: monotonically increasing counter for generating unique temp directory names.
    pub struct InMemoryFs[r: Region] {
        mut files: Map[String, InMemoryFs.Entry],
        mut dirs: Set[String],
        mut times: Map[String, {atime = Int64, ctime = Int64, mtime = Int64}],
        mut counter: Int32
    }

}

