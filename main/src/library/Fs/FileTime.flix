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

pub mod Fs.FileTime {

    use Fs.FileStat
    use Fs.FileTime
    use Fs.FsLayer.{logPre, logPost}

    ///
    /// Handles the `FileTime` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTime` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileTime) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                y -> checked_ecast(
                    Fs.FileStat.handleWithFileSystem(
                        handleWithFileStat(f)
                    )(y)
                )
            )(x)
        )

    ///
    /// Runs the `FileTime` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTime` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileTime) + IO = handle(f)()

    ///
    /// Handles the `FileTime` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def handleWithFileStat(f: a -> b \ ef): a -> b \ (ef - FileTime) + FileStat = x ->
        run { f(x) } with handler FileTime {
            def accessTime(filename, k)       = k(Fs.FileStat.accessTime(filename))
            def creationTime(filename, k)     = k(Fs.FileStat.creationTime(filename))
            def modificationTime(filename, k) = k(Fs.FileStat.modificationTime(filename))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileTime) + {FileTime, Logger} =
        run { f() } with handler FileTime {
            def accessTime(filename, k) = {
                logPre("accessTime", filename);
                let result = FileTime.accessTime(filename);
                logPost("accessTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def creationTime(filename, k) = {
                logPre("creationTime", filename);
                let result = FileTime.creationTime(filename);
                logPost("creationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def modificationTime(filename, k) = {
                logPre("modificationTime", filename);
                let result = FileTime.modificationTime(filename);
                logPost("modificationTime", filename, t -> "${t}ms", result);
                k(result)
            }
        }

}
