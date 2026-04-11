/*
 *  Copyright 2024 Holger Dal Mogensen
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
/// An effect used to write to the file system.
///
pub eff FileWriteWithResult {

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

// JVM-only: the default handler implementation lives in `jvm/FileWriteWithResult.flix`.
