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

pub mod Fs.FilePermission {

    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.Paths

    use Fs.FileStat
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FilePermission` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePermission` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FilePermission) + IO = x ->
        run {
            f(x)
        } with handler FilePermission {
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
        }

    ///
    /// Runs the `FilePermission` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePermission` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FilePermission) + IO = handle(f)()

    ///
    /// Handles the `FilePermission` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def handleWithFileStat(f: a -> b \ ef): a -> b \ (ef - FilePermission) + FileStat = x ->
        run { f(x) } with handler FilePermission {
            def isReadable(filename, k)  = k(Fs.FileStat.isReadable(filename))
            def isWritable(filename, k)  = k(Fs.FileStat.isWritable(filename))
            def isExecutable(filename, k) = k(Fs.FileStat.isExecutable(filename))
        }

}
