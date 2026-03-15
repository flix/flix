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

pub mod Fs {

    // Effect Hierarchy:
    //
    // FileSystem                          (27 ops — unified root)
    // ├── FileStat                        (11 ops)
    // │   ├── FileTest                    (4 ops)
    // │   │   ├── FileExists                    — exists
    // │   │   ├── IsDirectory                   — isDirectory
    // │   │   ├── IsRegularFile                 — isRegularFile
    // │   │   └── IsSymbolicLink                — isSymbolicLink
    // │   ├── FilePermission              (3 ops)
    // │   │   ├── IsReadable                    — isReadable
    // │   │   ├── IsWritable                    — isWritable
    // │   │   └── IsExecutable                  — isExecutable
    // │   ├── FileTime                    (3 ops)
    // │   │   ├── AccessTime                    — accessTime
    // │   │   ├── CreationTime                  — creationTime
    // │   │   └── ModificationTime              — modificationTime
    // │   └── FileSize                          — size
    // ├── FileRead                        (3 ops)
    // │   ├── ReadFile                          — read
    // │   ├── ReadLines                         — readLines
    // │   └── ReadBytes                         — readBytes
    // ├── DirList                               — list
    // └── FileWrite                       (12 ops)
    //     ├── WriteFile                         — write
    //     ├── WriteLines                        — writeLines
    //     ├── WriteBytes                        — writeBytes
    //     ├── AppendFile                        — append
    //     ├── AppendLines                       — appendLines
    //     ├── AppendBytes                       — appendBytes
    //     ├── Truncate                          — truncate
    //     ├── MoveFile                          — move(src, dst)
    //     ├── DeleteFile                        — delete
    //     ├── MkDir                             — mkDir
    //     ├── MkDirs                            — mkDirs
    //     └── MkTempDir                         — mkTempDir

    ///
    /// An effect used to check if a file exists.
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
    pub eff IsDirectory {

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a path is a regular file.
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
    pub eff IsSymbolicLink {

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a file is readable.
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
    pub eff IsWritable {

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

    }

    ///
    /// An effect used to check if a file is executable.
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
    pub eff AccessTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the creation time of a file.
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
    pub eff ModificationTime {

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the size of a file.
    ///
    pub eff FileSize {

        ///
        /// Returns the size of the given file `f` in bytes.
        ///
        def size(f: String): Result[IoError, Int64]

    }

    ///
    /// An effect used to read the contents of a file as a string.
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
    pub eff ReadLines {

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

    }

    ///
    /// An effect used to read the bytes of a file.
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
    pub eff DirList {

        ///
        /// Returns a list with the names of all files and directories in the given directory `f`.
        ///
        def list(f: String): Result[IoError, List[String]]

    }

    ///
    /// An effect used to write the contents of a file as a string.
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
    pub eff Truncate {

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to move (rename) a file or directory.
    ///
    pub eff MoveFile {

        ///
        /// Moves (renames) the file or directory `src` to `dst`.
        ///
        /// Fails if `dst` already exists.
        ///
        def move(src: {src = String}, dst: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to delete a file.
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
    pub eff MkDir {

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

    }

    ///
    /// An effect used to create a directory and all its parent directories.
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
        /// Returns the size of the given file `f` in bytes.
        ///
        def size(f: String): Result[IoError, Int64]

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
        /// Moves (renames) the file or directory `src` to `dst`.
        ///
        /// Fails if `dst` already exists.
        ///
        def move(src: {src = String}, dst: String): Result[IoError, Unit]

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
        /// Returns the size of the given file `f` in bytes.
        ///
        def size(f: String): Result[IoError, Int64]

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
        /// Moves (renames) the file or directory `src` to `dst`.
        ///
        /// Fails if `dst` already exists.
        ///
        def move(src: {src = String}, dst: String): Result[IoError, Unit]

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
