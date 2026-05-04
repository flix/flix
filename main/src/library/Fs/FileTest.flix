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

pub mod Fs.FileTest {

    use Fs.FileStat
    use Fs.FileTest
    use Fs.FsLayer.{logPre, logPost}
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// An effect used to test file properties (exists, directory, regular file, symbolic link).
    ///
    pub eff FileTest {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `FileTest` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTest` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileTest) + IO = x ->
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
    /// Runs the `FileTest` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTest` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileTest) + IO = handle(f)()

    ///
    /// Handles the `FileTest` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def runWithFileStat(f: Unit -> a \ ef): a \ (ef - FileTest) + FileStat =
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Fs.FileStat.exists(filename))
            def isDirectory(filename, k)    = k(Fs.FileStat.isDirectory(filename))
            def isRegularFile(filename, k)  = k(Fs.FileStat.isRegularFile(filename))
            def isSymbolicLink(filename, k) = k(Fs.FileStat.isSymbolicLink(filename))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileTest) + {FileTest, Logger} =
        run { f() } with handler FileTest {
            def exists(filename, k) = {
                logPre("exists", filename);
                let result = FileTest.exists(filename);
                logPost("exists", filename, b -> "${b}", result);
                k(result)
            }
            def isDirectory(filename, k) = {
                logPre("isDirectory", filename);
                let result = FileTest.isDirectory(filename);
                logPost("isDirectory", filename, b -> "${b}", result);
                k(result)
            }
            def isRegularFile(filename, k) = {
                logPre("isRegularFile", filename);
                let result = FileTest.isRegularFile(filename);
                logPost("isRegularFile", filename, b -> "${b}", result);
                k(result)
            }
            def isSymbolicLink(filename, k) = {
                logPre("isSymbolicLink", filename);
                let result = FileTest.isSymbolicLink(filename);
                logPost("isSymbolicLink", filename, b -> "${b}", result);
                k(result)
            }
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(FileTest.exists(r(filename)))
            def isDirectory(filename, k)    = k(FileTest.isDirectory(r(filename)))
            def isRegularFile(filename, k)  = k(FileTest.isRegularFile(r(filename)))
            def isSymbolicLink(filename, k) = k(FileTest.isSymbolicLink(r(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Result.flatMap(p -> FileTest.exists(p), c(filename)))
            def isDirectory(filename, k)    = k(Result.flatMap(p -> FileTest.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)  = k(Result.flatMap(p -> FileTest.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k) = k(Result.flatMap(p -> FileTest.isSymbolicLink(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Result.flatMap(p -> FileTest.exists(p), c(filename)))
            def isDirectory(filename, k)    = k(Result.flatMap(p -> FileTest.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)  = k(Result.flatMap(p -> FileTest.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k) = k(Result.flatMap(p -> FileTest.isSymbolicLink(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Result.flatMap(p -> FileTest.exists(p), c(filename)))
            def isDirectory(filename, k)    = k(Result.flatMap(p -> FileTest.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)  = k(Result.flatMap(p -> FileTest.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k) = k(Result.flatMap(p -> FileTest.isSymbolicLink(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Result.flatMap(p -> FileTest.exists(p), c(filename)))
            def isDirectory(filename, k)    = k(Result.flatMap(p -> FileTest.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)  = k(Result.flatMap(p -> FileTest.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k) = k(Result.flatMap(p -> FileTest.isSymbolicLink(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(Result.flatMap(p -> FileTest.exists(p), c(filename)))
            def isDirectory(filename, k)    = k(Result.flatMap(p -> FileTest.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)  = k(Result.flatMap(p -> FileTest.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k) = k(Result.flatMap(p -> FileTest.isSymbolicLink(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileTest` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks.
    ///
    /// - Bool ops (`exists`, `isDirectory`, `isRegularFile`): resolve path; return `Ok(false)` for dangling symlinks.
    /// - `isSymbolicLink`: passed through unchanged (always checks the link itself).
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - FileTest) + FileTest =
        let resolveBool = (path, op) -> match Fs.FsLayer.resolveRealPath(path) {
            case Ok(p)                               => op(p)
            case Err(IoError(ErrorKind.NotFound, _)) => Ok(false)
            case Err(e)                              => Err(e)
        };
        run { f() } with handler FileTest {
            def exists(filename, k)         = k(resolveBool(filename, p -> FileTest.exists(p)))
            def isDirectory(filename, k)    = k(resolveBool(filename, p -> FileTest.isDirectory(p)))
            def isRegularFile(filename, k)  = k(resolveBool(filename, p -> FileTest.isRegularFile(p)))
            def isSymbolicLink(filename, k) = k(FileTest.isSymbolicLink(filename))
        }

}
