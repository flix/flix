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

    use Fs.FileStat
    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPost}

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
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileStat) + {FileStat, Logger} =
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

    ///
    /// Middleware that intercepts the `FileStat` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileStat) + FileStat =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileStat {
            def exists(filename, k)           = k(FileStat.exists(r(filename)))
            def isDirectory(filename, k)      = k(FileStat.isDirectory(r(filename)))
            def isRegularFile(filename, k)    = k(FileStat.isRegularFile(r(filename)))
            def isSymbolicLink(filename, k)   = k(FileStat.isSymbolicLink(r(filename)))
            def isReadable(filename, k)       = k(FileStat.isReadable(r(filename)))
            def isWritable(filename, k)       = k(FileStat.isWritable(r(filename)))
            def isExecutable(filename, k)     = k(FileStat.isExecutable(r(filename)))
            def accessTime(filename, k)       = k(FileStat.accessTime(r(filename)))
            def creationTime(filename, k)     = k(FileStat.creationTime(r(filename)))
            def modificationTime(filename, k) = k(FileStat.modificationTime(r(filename)))
            def size(filename, k)             = k(FileStat.size(r(filename)))
        }

    ///
    /// Middleware that intercepts the `FileStat` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileStat) + FileStat =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileStat {
            def exists(filename, k)           = k(Result.flatMap(p -> FileStat.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileStat.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileStat.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileStat.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileStat.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileStat.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileStat.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileStat.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileStat.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileStat.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileStat.size(p), c(filename)))
        }

}
