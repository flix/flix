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

    use Fs.AccessTime
    use Fs.CreationTime
    use Fs.FileExists
    use Fs.FilePermission
    use Fs.FileSize
    use Fs.FileStat
    use Fs.FileSystem
    use Fs.FileTest
    use Fs.FileTime
    use Fs.FsClient.{logPre, logPost}
    use Fs.IsDirectory
    use Fs.IsExecutable
    use Fs.IsReadable
    use Fs.IsRegularFile
    use Fs.IsSymbolicLink
    use Fs.IsWritable
    use Fs.ModificationTime

    ///
    /// Handles the `FileStat` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileStat` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileStat) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                handleWithFileSystem(f)
            )(x)
        )

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
    /// Middleware that intercepts the `FileStat` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef - {FileExists, IsDirectory, IsRegularFile, IsSymbolicLink, IsReadable, IsWritable, IsExecutable, AccessTime, CreationTime, ModificationTime, FileSize, FileTest, FilePermission, FileTime}): a \ (ef - FileStat) + {FileStat, Logger} =
        run { f() } with handler FileStat {
            def exists(filename, k) = {
                logPre("exists", filename);
                let result = FileStat.exists(filename);
                logPost("exists", filename, b -> "${b}", result);
                k(result)
            }
            def isDirectory(filename, k) = {
                logPre("isDirectory", filename);
                let result = FileStat.isDirectory(filename);
                logPost("isDirectory", filename, b -> "${b}", result);
                k(result)
            }
            def isRegularFile(filename, k) = {
                logPre("isRegularFile", filename);
                let result = FileStat.isRegularFile(filename);
                logPost("isRegularFile", filename, b -> "${b}", result);
                k(result)
            }
            def isSymbolicLink(filename, k) = {
                logPre("isSymbolicLink", filename);
                let result = FileStat.isSymbolicLink(filename);
                logPost("isSymbolicLink", filename, b -> "${b}", result);
                k(result)
            }
            def isReadable(filename, k) = {
                logPre("isReadable", filename);
                let result = FileStat.isReadable(filename);
                logPost("isReadable", filename, b -> "${b}", result);
                k(result)
            }
            def isWritable(filename, k) = {
                logPre("isWritable", filename);
                let result = FileStat.isWritable(filename);
                logPost("isWritable", filename, b -> "${b}", result);
                k(result)
            }
            def isExecutable(filename, k) = {
                logPre("isExecutable", filename);
                let result = FileStat.isExecutable(filename);
                logPost("isExecutable", filename, b -> "${b}", result);
                k(result)
            }
            def accessTime(filename, k) = {
                logPre("accessTime", filename);
                let result = FileStat.accessTime(filename);
                logPost("accessTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def creationTime(filename, k) = {
                logPre("creationTime", filename);
                let result = FileStat.creationTime(filename);
                logPost("creationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def modificationTime(filename, k) = {
                logPre("modificationTime", filename);
                let result = FileStat.modificationTime(filename);
                logPost("modificationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def size(filename, k) = {
                logPre("size", filename);
                let result = FileStat.size(filename);
                logPost("size", filename, s -> "${s} bytes", result);
                k(result)
            }
        }

}
