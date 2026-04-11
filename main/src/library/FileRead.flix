/*
 *  Copyright 2024 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// An effect used to read from the file system.
///
/// All operations on this effect are infallible.
/// If an operation fails the handler must deal with it.
///
pub eff FileRead {

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    def exists(f: String): Bool

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    def isDirectory(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    def isRegularFile(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    def isReadable(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    def isSymbolicLink(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    def isWritable(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    def isExecutable(f: String): Bool

    ///
    /// Returns the last access time of the given file `f` in milliseconds since the epoch.
    ///
    def accessTime(f: String): Int64

    ///
    /// Returns the creation time of the given file `f` in milliseconds since the epoch.
    ///
    def creationTime(f: String): Int64

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
    ///
    def modificationTime(f: String): Int64

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    def size(f: String): Int64

    ///
    /// Returns a string of all lines in the given file `f`.
    ///
    def read(f: String): String

    ///
    /// Returns a list of all lines in the given file `f`.
    ///
    def readLines(f: String): List[String]

    ///
    /// Returns a vector of all the bytes in the given file `f`.
    ///
    def readBytes(f: String): Vector[Int8]

    ///
    /// Returns a list with the names of all files and directories in the given directory `d`.
    ///
    def list(f: String): List[String]

}

// JVM-only: the default handler implementation lives in `jvm/FileRead.flix`.
