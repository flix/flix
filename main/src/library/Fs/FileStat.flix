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

pub mod Fs.FileStat {

    import java.io.IOException
    import java.lang.Class
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.attribute.BasicFileAttributes

    use Fs.FileSystem
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileStat` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileStat` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileStat) + IO = x ->
        run {
            f(x)
        } with handler FileStat {
            def exists(filename, k) = {
                let res = try {
                    Ok(Files.exists(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isDirectory(filename, k) = {
                let res = try {
                    Ok(Files.isDirectory(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isRegularFile(filename, k) = {
                let res = try {
                    Ok(Files.isRegularFile(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isSymbolicLink(filename, k) = {
                let res = try {
                    Ok(Files.isSymbolicLink(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isReadable(filename, k) = {
                let res = try {
                    Ok(Files.isReadable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isWritable(filename, k) = {
                let res = try {
                    Ok(Files.isWritable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isExecutable(filename, k) = {
                let res = try {
                    Ok(Files.isExecutable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def accessTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).lastAccessTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def creationTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def modificationTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).lastModifiedTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def size(filename, k) = {
                let res = try {
                    Ok(Files.size(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `FileStat` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileStat` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileStat) + IO = handle(f)()

    ///
    /// Handles the `FileStat` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - FileStat) + FileSystem = x ->
        run { f(x) } with handler FileStat {
            def exists(filename, k)           = k(Fs.FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(Fs.FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(Fs.FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(Fs.FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(Fs.FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(Fs.FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(Fs.FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(Fs.FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(Fs.FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(Fs.FileSystem.modificationTime(filename))
            def size(filename, k)             = k(Fs.FileSystem.size(filename))
        }

    ///
    /// Returns the attributes of the given file `f`.
    /// May throw `IOException` and `SecurityException`.
    ///
    def fileAttributes(path: Path): BasicFileAttributes \ IO =
        Files.readAttributes(
            path,
            Class.forName("java.nio.file.attribute.BasicFileAttributes"),
            (...{}: Vector[LinkOption])
        )

}
