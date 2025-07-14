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

mod FileWriteWithResult {

    import java.io.IOException
    import java.lang.IllegalArgumentException
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
    /// Handles the `FileWriteWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWriteWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileWriteWithResult) + IO = x ->
        run {
            f(x)
        } with handler FileWriteWithResult {
            def write(data, file, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    Files.write(Paths.get(file), bytes, (...{}: Vector[OpenOption]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def writeLines(data, file, k) = {
                let res = try {
                    let bf = Files.newBufferedWriter(Paths.get(file), (...{}: Vector[OpenOption]));
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

            def writeBytes(data, file, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        Files.write(Paths.get(file), bytes, (...{}: Vector[OpenOption]));
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def append(data, file, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    Files.write(Paths.get(file), bytes, ...{opt});
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def appendLines(data, file, k) = {
                let res = try {
                    let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                    let bf = Files.newBufferedWriter(Paths.get(file), ...{opt});
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

            def appendBytes(data, file, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        let opt: OpenOption = checked_cast(StandardOpenOption.APPEND);
                        Files.write(Paths.get(file), bytes, ...{opt});
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def truncate(file, k) = {
                let res = try {
                    region rc {
                        let opt: OpenOption = checked_cast(StandardOpenOption.TRUNCATE_EXISTING);
                        let arr: Array[Int8, rc] = Array#{} @ rc;
                        Files.write(Paths.get(file), arr, ...{opt});
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

            def mkTempDir(prefix, k) = {
                let res = try {
                    let path = Files.createTempDirectory(prefix, (...{}: Vector[FileAttribute]));
                    Ok(path.toString())
                } catch {
                    case ex: IllegalArgumentException      => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `FileWriteWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWriteWithResult` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileWriteWithResult) + {IO} = handle(f)()

}
