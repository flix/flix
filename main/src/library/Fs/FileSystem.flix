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

pub mod Fs.FileSystem {

    import java.io.File
    import java.io.IOException
    import java.lang.Class
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.NotDirectoryException
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.attribute.BasicFileAttributes

    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileSystem) + IO = x ->
        run {
            f(x)
        } with handler FileSystem {
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

            def read(filename, k) = {
                let res = try {
                    Ok(String.fromBytes(Array.toVector(Files.readAllBytes(Paths.get(filename)))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readLines(filename, k) = {
                let res = try {
                    Ok(ToFlix.toFlix(Files.readAllLines(Paths.get(filename))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readBytes(filename, k) = {
                let res = try {
                    Ok(Array.toVector(Files.readAllBytes(Paths.get(filename))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def list(filename, k) = {
                let res = try {
                    let file = new File(filename);
                    Ok(Array.toList(file.list()))
                } catch {
                    case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: NotDirectoryException => Err(IoError(ErrorKind.NotDirectory, ex.getMessage()))
                    case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileSystem) + IO = handle(f)()

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
