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
/// An effect used to write to the file system.
///
/// All operations on this effect are infalliable.
/// If an operation fails the handler must deal with it.
///
pub eff FileWrite {

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

}

mod FileWrite {

    import java.io.IOException
    import java.lang.UnsupportedOperationException
    import java.nio.charset.StandardCharsets
    import java.nio.file.attribute.FileAttribute
    import java.nio.file.FileAlreadyExistsException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.OpenOption
    import java.nio.file.Paths
    import java.nio.file.StandardOpenOption

    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - FileWrite) + IO = x ->
        try {
            Ok(f(x))
        } with FileWrite {
            def write(data, f, k) = {
                try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
                    k(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }

            def append(data, f, k) = {
                try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    Files.write(Paths.get(f), bytes, ...{opt});
                    k(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }

            def mkDir(d, k) = {
                try {
                    Files.createDirectory(Paths.get(d), (...{}: Vector[FileAttribute]));
                    k(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }

            def mkDirs(d, k) = {
                try {
                    Files.createDirectories(Paths.get(d), (...{}: Vector[FileAttribute]));
                    k(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }
        }

}
