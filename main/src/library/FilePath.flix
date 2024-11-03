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
    pub def mkDirs(d: String): Unit

    ///
    /// Returns a list with the names of all files and directories in the given directory `d`.
    ///
    pub def list(f: String): List[String]

}

mod FilePath {

    import java.io.File
    import java.io.IOException
    import java.lang.Class
    import java.lang.{String => JString}
    import java.lang.UnsupportedOperationException
    import java.nio.charset.StandardCharsets
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.attribute.FileAttribute
    import java.nio.file.FileAlreadyExistsException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.NotDirectoryException
    import java.nio.file.OpenOption
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.StandardOpenOption
    import java.util.{List => JList}
    import java.util.stream.Collectors

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
                try {
                    k(Files.exists(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isDirectory(f, k) = {
                try {
                    k(Files.isDirectory(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isRegularFile(f, k) = {
                try {
                    k(Files.isRegularFile(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isReadable(f, k) = {
                try {
                    k(Files.isReadable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isSymbolicLink(f, k) = {
                try {
                    k(Files.isSymbolicLink(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isWritable(f, k) = {
                try {
                    k(Files.isWritable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def isExecutable(f, k) = {
                try {
                    k(Files.isExecutable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def accessTime(f, k) = {
                try {
                    k(fileAttributes(Paths.get(f)).lastAccessTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def creationTime(f, k) = {
                try {
                    k(fileAttributes(Paths.get(f)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def modificationTime(f, k) = {
                try {
                    k(fileAttributes(Paths.get(f)).lastModifiedTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def size(f, k) = {
                try {
                    k(Files.size(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def read(f, k) = {
                try {
                    k(new JString(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def readLines(f, k) = {
                try {
                    k(FromJava.fromJava(Files.readAllLines(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def readBytes(f, k) = {
                try {
                    k(Array.toVector(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def write(data, f, k) = {
                try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def writeLines(data, f, k) = {
                try {
                    let bf = Files.newBufferedWriter(Paths.get(f), (...{}: Vector[OpenOption]));
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def writeBytes(data, f, k) = {
                try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
                        k(())
                    }
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def append(data, f, k) = {
                try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    Files.write(Paths.get(f), bytes, ...{opt});
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def appendLines(data, f, k) = {
                try {
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    let bf = Files.newBufferedWriter(Paths.get(f), ...{opt});
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def appendBytes(data, f, k) = {
                try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                        Files.write(Paths.get(f), bytes, ...{opt});
                        k(())
                    }
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def truncate(f, k) = {
                try {
                    region rc {
                        let opt: OpenOption = checked_cast(StandardOpenOption.TRUNCATE_EXISTING);
                        let arr: Array[Int8, rc] = Array#{} @ rc;
                        Files.write(Paths.get(f), arr, ...{opt});
                        k(())
                    }
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def mkDir(d, k) = {
                try {
                    Files.createDirectory(Paths.get(d), (...{}: Vector[FileAttribute]));
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: FileAlreadyExistsException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def mkDirs(d, k) = {
                try {
                    Files.createDirectories(Paths.get(d), (...{}: Vector[FileAttribute]));
                    k(())
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IOError.Generic(ex.getMessage()))
                    case ex: FileAlreadyExistsException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }

            def list(f, k) = {
                try {
                    // TODO: Use java.nio.file.Files.list, but it returns a generic Stream<Path> which Flix's type system currently doesn't support.
                    let file = new File(f);
                    k(Array.toList(file.list()))
                } catch {
                    case ex: InvalidPathException => Err(IOError.Generic(ex.getMessage()))
                    case ex: NotDirectoryException => Err(IOError.Generic(ex.getMessage()))
                    case ex: IOException => Err(IOError.Generic(ex.getMessage()))
                }
            }
        }

        ///
        /// Returns the attributes of the given file `f`.
        /// May throw `IOException` and `SecurityException`.
        ///
        def fileAttributes(path: Path): BasicFileAttributes \ IO =
            Files.readAttributes(
                path,
                // "[...] invoking this method with a type parameter of BasicFileAttributes.class will not throw UnsupportedOperationException."
                Class.forName("java.nio.file.attribute.BasicFileAttributes"),
                (...{}: Vector[LinkOption])
            )

}
