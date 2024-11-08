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
/// An effect used to interact with the file system.
///
pub eff FilePathWithError {

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    pub def isDirectory(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    pub def isRegularFile(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: String): Result[IoError, Bool]

    ///
    /// Returns the last access time of the given file `f` in milliseconds since the epoch.
    ///
    pub def accessTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the creation time of the given file `f` in milliseconds since the epoch.
    ///
    pub def creationTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
    ///
    pub def modificationTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    pub def size(f: String): Result[IoError, Int64]

    ///
    /// Returns a string of all lines in the given file `f`.
    ///
    pub def read(f: String): Result[IoError, String]

    ///
    /// Returns a list of all lines in the given file `f`.
    ///
    pub def readLines(f: String): Result[IoError, List[String]]

    ///
    /// Returns a vector of all the bytes in the given file `f`.
    ///
    pub def readBytes(f: String): Result[IoError, Vector[Int8]]

    ///
    /// Writes `str` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def write(data: {str = String}, f: String): Result[IoError, Unit]

    ///
    /// Writes `lines` to the given file `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    ///
    /// Writes `data` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    pub def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    ///
    /// Appends `str` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def append(data: {str = String}, f: String): Result[IoError, Unit]

    ///
    /// Appends `lines` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Creates the file `f` if it does not exist.
    ///
    pub def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    ///
    /// Truncates the given file `f`.
    ///
    pub def truncate(f: String): Result[IoError, Unit]

    ///
    /// Creates the directory `d`.
    ///
    pub def mkDir(d: String): Result[IoError, Unit]

    ///
    /// Creates the directory `d` and all its parent directories.
    ///
    pub def mkDirs(d: String): Result[IoError, Unit]

    ///
    /// Returns a list with the names of all files and directories in the given directory `d`.
    ///
    pub def list(f: String): Result[IoError, List[String]]

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

    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FilePath` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePath` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FilePath) + IO = x ->
        try {
            Ok(f(x))
        } with FilePath {
            def exists(f, k) = {
                let res = try {
                    Ok(Files.exists(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isDirectory(f, k) = {
                let res = try {
                    Ok(Files.isDirectory(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isRegularFile(f, k) = {
                let res = try {
                    Ok(Files.isRegularFile(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isReadable(f, k) = {
                let res = try {
                    Ok(Files.isReadable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isSymbolicLink(f, k) = {
                let res = try {
                    Ok(Files.isSymbolicLink(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isWritable(f, k) = {
                let res = try {
                    Ok(Files.isWritable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isExecutable(f, k) = {
                let res = try {
                    Ok(Files.isExecutable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def accessTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).lastAccessTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def creationTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def modificationTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).lastModifiedTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def size(f, k) = {
                let res = try {
                    Ok(Files.size(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def read(f, k) = {
                let res = try {
                    Ok(new JString(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readLines(f, k) = {
                let res = try {
                    Ok(FromJava.fromJava(Files.readAllLines(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readBytes(f, k) = {
                let res = try {
                    Ok(Array.toVector(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def write(data, f, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def writeLines(data, f, k) = {
                let res = try {
                    let bf = Files.newBufferedWriter(Paths.get(f), (...{}: Vector[OpenOption]));
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def writeBytes(data, f, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def append(data, f, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    Files.write(Paths.get(f), bytes, ...{opt});
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def appendLines(data, f, k) = {
                let res = try {
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    let bf = Files.newBufferedWriter(Paths.get(f), ...{opt});
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def appendBytes(data, f, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                        Files.write(Paths.get(f), bytes, ...{opt});
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def truncate(f, k) = {
                let res = try {
                    region rc {
                        let opt: OpenOption = checked_cast(StandardOpenOption.TRUNCATE_EXISTING);
                        let arr: Array[Int8, rc] = Array#{} @ rc;
                        Files.write(Paths.get(f), arr, ...{opt});
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def mkDir(d, k) = {
                let res = try {
                    Files.createDirectory(Paths.get(d), (...{}: Vector[FileAttribute]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def mkDirs(d, k) = {
                let res = try {
                    Files.createDirectories(Paths.get(d), (...{}: Vector[FileAttribute]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def list(f, k) = {
                let res = try {
                    // TODO: Use java.nio.file.Files.list when we have support for Java generics.
                    let file = new File(f);
                    Ok(Array.toList(file.list()))
                } catch {
                    case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: NotDirectoryException => Err(IoError(ErrorKind.NotADirectory, ex.getMessage()))
                    case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
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
