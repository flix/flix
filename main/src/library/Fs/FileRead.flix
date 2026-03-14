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

pub mod Fs.FileRead {

    import java.io.IOException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.Paths

    use Fs.FileSystem
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileRead) + IO = x ->
        run {
            f(x)
        } with handler FileRead {
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
        }

    ///
    /// Runs the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileRead) + IO = handle(f)()

    ///
    /// Handles the `FileRead` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - FileRead) + FileSystem = x ->
        run { f(x) } with handler FileRead {
            def read(filename, k)      = k(Fs.FileSystem.read(filename))
            def readLines(filename, k) = k(Fs.FileSystem.readLines(filename))
            def readBytes(filename, k) = k(Fs.FileSystem.readBytes(filename))
        }

}
