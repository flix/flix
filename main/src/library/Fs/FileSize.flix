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

pub mod Fs.FileSize {

    import java.io.IOException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.Paths

    use Fs.FileStat
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileSize` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSize` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileSize) + IO = x ->
        run {
            f(x)
        } with handler FileSize {
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
    /// Runs the `FileSize` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSize` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileSize) + IO = handle(f)()

    ///
    /// Handles the `FileSize` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def handleWithFileStat(f: a -> b \ ef): a -> b \ (ef - FileSize) + FileStat = x ->
        run { f(x) } with handler FileSize {
            def size(filename, k) = k(Fs.FileStat.size(filename))
        }

}
