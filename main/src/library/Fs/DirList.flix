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

pub mod Fs.DirList {

    import java.io.File
    import java.io.IOException
    import java.nio.file.InvalidPathException
    import java.nio.file.NotDirectoryException

    use Fs.DirList
    use Fs.FileSystem
    use Fs.FsClient.{logPre, logPost}
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `DirList` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DirList` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - DirList) + IO = x ->
        run {
            f(x)
        } with handler DirList {
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
    /// Runs the `DirList` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DirList` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - DirList) + IO = handle(f)()

    ///
    /// Handles the `DirList` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - DirList) + FileSystem = x ->
        run { f(x) } with handler DirList {
            def list(filename, k) = k(Fs.FileSystem.list(filename))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - DirList) + {DirList, Logger} =
        run { f() } with handler DirList {
            def list(filename, k) = {
                logPre("list", filename);
                let result = DirList.list(filename);
                logPost("list", filename, ls -> "${List.length(ls)} entries", result);
                k(result)
            }
        }

}
