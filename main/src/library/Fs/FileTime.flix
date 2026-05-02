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
    /// An effect used to read file timestamps (access, creation, modification).
    ///
    pub eff FileTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// Handles the `FileTime` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTime` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileTime) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                y -> checked_ecast(
                    Fs.FileStat.runWithFileSystem(
                        () -> runWithFileStat(() -> f(y))
                    )
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
    pub def runWithFileStat(f: Unit -> a \ ef): a \ (ef - FileTime) + FileStat =
        run { f() } with handler FileTime {
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

    ///
    /// Middleware that intercepts the `FileTime` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(FileTime.accessTime(r(filename)))
            def creationTime(filename, k)     = k(FileTime.creationTime(r(filename)))
            def modificationTime(filename, k) = k(FileTime.modificationTime(r(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTime` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks.
    ///
    /// Value ops (`accessTime`, `creationTime`, `modificationTime`): resolve path; propagate all errors.
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - FileTime) + FileTime =
        run { f() } with handler FileTime {
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileTime.accessTime(p), Fs.FsLayer.resolveRealPath(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileTime.creationTime(p), Fs.FsLayer.resolveRealPath(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileTime.modificationTime(p), Fs.FsLayer.resolveRealPath(filename)))
        }

}
