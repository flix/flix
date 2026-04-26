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

    ///
    /// An effect used to check if a file exists.
    ///
    /// Use the `FileExists.runWithFileTest` handler.
    ///
    pub eff FileExists {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a path is a directory.
    ///
    /// Use the `IsDirectory.runWithFileTest` handler.
    ///
    pub eff IsDirectory {

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a path is a regular file.
    ///
    /// Use the `IsRegularFile.runWithFileTest` handler.
    ///
    pub eff IsRegularFile {

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a path is a symbolic link.
    ///
    /// Use the `IsSymbolicLink.runWithFileTest` handler.
    ///
    pub eff IsSymbolicLink {

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a file is readable.
    ///
    /// Use the `IsReadable.runWithFilePermission` handler.
    ///
    pub eff IsReadable {

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a file is writable.
    ///
    /// Use the `IsWritable.runWithFilePermission` handler.
    ///
    pub eff IsWritable {

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a file is executable.
    ///
    /// Use the `IsExecutable.runWithFilePermission` handler.
    ///
    pub eff IsExecutable {

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to read the last access time of a file.
    ///
    /// Use the `AccessTime.runWithFileTime` handler.
    ///
    pub eff AccessTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the creation time of a file.
    ///
    /// Use the `CreationTime.runWithFileTime` handler.
    ///
    pub eff CreationTime {

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the last modification time of a file.
    ///
    /// Use the `ModificationTime.runWithFileTime` handler.
    ///
    pub eff ModificationTime {

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the size of a file.
    ///
    /// Use the `FileSize.runWithFileStat` handler.
    ///
    pub eff FileSize {

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

    }

    ///
    /// An effect used to read the contents of a file as a string.
    ///
    /// Use the `ReadFile.runWithFileRead` handler.
    ///
    pub eff ReadFile {

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

    }

    ///
    /// An effect used to read the lines of a file.
    ///
    /// Use the `ReadLines.runWithFileRead` handler.
    ///
    pub eff ReadLines {

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

    }

    ///
    /// An effect used to read the bytes of a file.
    ///
    /// Use the `ReadBytes.runWithFileRead` handler.
    ///
    pub eff ReadBytes {

        ///
        /// Returns a vector of all the bytes in the given file `f`.
        ///
        def readBytes(f: String): Result[IoError, Vector[Int8]]

    }

    ///
    /// An effect used to list the contents of a directory.
    ///
    /// Use the `DirList.runWithFileSystem` handler.
    ///
    pub eff DirList {

        ///
        /// Returns a list with the names of all files and directories in the given directory `f`.
        ///
        def list(f: String): Result[IoError, List[String]]

    }

    ///
    /// An effect used to write the contents of a file as a string.
    ///
    /// Use the `WriteFile.runWithFileWrite` handler.
    ///
    pub eff WriteFile {

        ///
        /// Writes `str` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def write(data: {str = String}, f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to write lines to a file.
    ///
    /// Use the `WriteLines.runWithFileWrite` handler.
    ///
    pub eff WriteLines {

        ///
        /// Writes `lines` to the given file `f`.
        ///
        /// Creates `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to write bytes to a file.
    ///
    /// Use the `WriteBytes.runWithFileWrite` handler.
    ///
    pub eff WriteBytes {

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to append a string to a file.
    ///
    /// Use the `AppendFile.runWithFileWrite` handler.
    ///
    pub eff AppendFile {

        ///
        /// Appends `str` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def append(data: {str = String}, f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to append lines to a file.
    ///
    /// Use the `AppendLines.runWithFileWrite` handler.
    ///
    pub eff AppendLines {

        ///
        /// Appends `lines` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to append bytes to a file.
    ///
    /// Use the `AppendBytes.runWithFileWrite` handler.
    ///
    pub eff AppendBytes {

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to truncate a file.
    ///
    /// Use the `Truncate.runWithFileWrite` handler.
    ///
    pub eff Truncate {

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to copy a file.
    ///
    /// Use the `CopyFile.runWithFileWrite` handler.
    ///
    pub eff CopyFile {

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

    }

    ///
    /// An effect used to move (rename) a file or directory.
    ///
    /// Use the `MoveFile.runWithFileWrite` handler.
    ///
    pub eff MoveFile {

        ///
        /// Moves (renames) the file or directory `src` to `dst` with the given `options`.
        ///
        def moveWith(src: {src = String}, dst: String, opts: Set[MoveOption]): Result[IoError, Unit]

    }

    ///
    /// An effect used to delete a file.
    ///
    /// Use the `DeleteFile.runWithFileWrite` handler.
    ///
    pub eff DeleteFile {

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to create a directory.
    ///
    /// Use the `MkDir.runWithFileWrite` handler.
    ///
    pub eff MkDir {

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to create a directory and all its parent directories.
    ///
    /// Use the `MkDirs.runWithFileWrite` handler.
    ///
    pub eff MkDirs {

        ///
        /// Creates the directory `d` and all its parent directories.
        ///
        def mkDirs(d: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to create a temporary directory.
    ///
    /// Use the `MkTempDir.runWithFileWrite` handler.
    ///
    pub eff MkTempDir {

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

    ///
    /// An effect used to test file properties (exists, directory, regular file, symbolic link).
    ///
    pub eff FileTest {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check file permissions (readable, writable, executable).
    ///
    pub eff FilePermission {

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to read file timestamps (access, creation, modification).
    ///
    pub eff FileTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read file metadata (test, permissions, timestamps, size).
    ///
    pub eff FileStat {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

    }

    ///
    /// An effect used to read file contents (string, lines, bytes).
    ///
    pub eff FileRead {

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

        ///
        /// Returns a vector of all the bytes in the given file `f`.
        ///
        def readBytes(f: String): Result[IoError, Vector[Int8]]

    }

    ///
    /// An effect used to write files, append to files, truncate files, and create directories.
    ///
    pub eff FileWrite {

        ///
        /// Writes `str` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def write(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Writes `lines` to the given file `f`.
        ///
        /// Creates `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Appends `str` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def append(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Appends `lines` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

        ///
        /// Moves (renames) the file or directory `src` to `dst` with the given `options`.
        ///
        def moveWith(src: {src = String}, dst: String, opts: Set[MoveOption]): Result[IoError, Unit]

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d` and all its parent directories.
        ///
        def mkDirs(d: String): Result[IoError, Unit]

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

    ///
    /// An effect used to perform all filesystem operations.
    ///
    pub eff FileSystem {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

        ///
        /// Returns a vector of all the bytes in the given file `f`.
        ///
        def readBytes(f: String): Result[IoError, Vector[Int8]]

        ///
        /// Returns a list with the names of all files and directories in the given directory `f`.
        ///
        def list(f: String): Result[IoError, List[String]]

        ///
        /// Returns a list of paths under `base` that match the given glob `pattern`.
        ///
        def glob(base: String, pattern: String): Result[IoError, List[String]]

        ///
        /// Writes `str` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def write(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Writes `lines` to the given file `f`.
        ///
        /// Creates `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Appends `str` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def append(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Appends `lines` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

        ///
        /// Moves (renames) the file or directory `src` to `dst` with the given `options`.
        ///
        def moveWith(src: {src = String}, dst: String, opts: Set[MoveOption]): Result[IoError, Unit]

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d` and all its parent directories.
        ///
        def mkDirs(d: String): Result[IoError, Unit]

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

}

