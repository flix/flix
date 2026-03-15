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

    use Fs.FilePermission
    use Fs.FileStat
    use Fs.FsClient.{logPre, logPost}

    ///
    /// Handles the `FilePermission` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePermission` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FilePermission) + IO = x ->
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

    ///
    /// Middleware that intercepts the `FilePermission` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FilePermission) + {FilePermission, Logger} =
        run { f() } with handler FilePermission {
            def isReadable(filename, k) = {
                logPre("isReadable", filename);
                let result = FilePermission.isReadable(filename);
                logPost("isReadable", filename, b -> "${b}", result);
                k(result)
            }
            def isWritable(filename, k) = {
                logPre("isWritable", filename);
                let result = FilePermission.isWritable(filename);
                logPost("isWritable", filename, b -> "${b}", result);
                k(result)
            }
            def isExecutable(filename, k) = {
                logPre("isExecutable", filename);
                let result = FilePermission.isExecutable(filename);
                logPost("isExecutable", filename, b -> "${b}", result);
                k(result)
            }
        }

}
