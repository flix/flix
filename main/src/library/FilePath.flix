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
/// An effect used to interact with the file system.
///
/// All operations on this effect are infalliable.
/// If an operation fails the handler must deal with it.
///
pub eff FilePath {

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: String): Bool

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    pub def isDirectory(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    pub def isRegularFile(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: String): Bool

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: String): Bool

    ///
    /// Returns the last access time of the given file `f` in milliseconds since the epoch.
    ///
    pub def accessTime(f: String): Int64

    ///
    /// Returns the creation time of the given file `f` in milliseconds since the epoch.
    ///
    pub def creationTime(f: String): Int64

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
    ///
    pub def modificationTime(f: String): Int64

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    pub def size(f: String): Int64

    ///
    /// Returns a string of all lines in the given file `f`.
    ///
    pub def read(f: String): String

    ///
    /// Returns a list of all lines in the given file `f`.
    ///
    pub def readLines(f: String): List[String]

    ///
    /// Returns a vector of all the bytes in the given file `f`.
    ///
    pub def readBytes(f: String): Vector[Int8]

    ///
    /// Writes `str` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def write(data: {str = String}, f: String): Unit

    ///
    /// Writes `lines` to the given file `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def writeLines(data: {lines = List[String]}, f: String): Unit

    ///
    /// Writes `data` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def writeBytes(data: Vector[Int8], f: String): Unit

    ///
    /// Appends `str` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def append(data: {str = String}, f: String): Unit

    ///
    /// Appends `lines` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def appendLines(data: {lines = List[String]}, f: String): Unit

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def appendBytes(data: Vector[Int8], f: String): Unit

    ///
    /// Truncates the given file `f`.
    ///
    pub def truncate(f: String): Unit

    ///
    /// Creates the directory `d`.
    ///
    pub def mkDir(d: String): Unit

    ///
    /// Creates the directory `d` and all its parent directories.
    ///
    pub def mkdDirs(d: String): Unit

    ///
    /// Returns a list with the names of all files and directories in the given directory `d`.
    ///
    pub def list(f: String): List[String]

}

mod FilePath {

    import java.io.File
    import java.lang.Class
    import java.lang.Exception
    import java.lang.{String => JString}
    import java.nio.charset.StandardCharsets
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.Files
    import java.nio.file.LinkOption
    import java.nio.file.Path
    import java.nio.file.Paths

    ///
    /// Handles the `FilePath` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePath` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IOError, b] \ (ef - FilePath) + IO = x ->
        try {
            Ok(f(x))
        } with FilePath {
            def exists(f, k) = {
                let existsRes = try {
                    let restPath: Vector[String] = Vector.empty();
                    let path = Paths.get(f, restPath);
                    let linkOpts: Vector[LinkOption] = Vector.empty();
                    let exists = Files.exists(path, linkOpts);
                    Ok(exists)
                } catch {
                    case ex: Exception =>
                        Err(IOError.Generic(ex.getMessage()))
                };
                match existsRes {
                    case Ok(exists) => k(exists)
                    case Err(e) => Err(e)
                }
            }
        }

//        def fileAttributes(path: Path): BasicFileAttributes \ IO = {
//            let varargs: Vector[LinkOption] = Vector.empty();
//            Files.readAttributes(path, Class.forName("java.nio.file.attribute.BasicFileAttributes"), varargs)
//        }

}
