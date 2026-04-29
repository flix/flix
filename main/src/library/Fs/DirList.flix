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

    use Fs.DirList
    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPost}

    ///
    /// An effect used to list the contents of a directory.
    ///
    /// Use the `DirList.runWithFileSystem` handler.
    ///
    pub eff DirList {

        ///
        /// Returns a list with the names of all files and directories in the given directory `f`.
        ///
        def list(f: String): Result[IoError, List[String]]

    }

    ///
    /// Handles the `DirList` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DirList` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - DirList) + IO = x ->
        run { f(x) } with handler DirList {
            def list(filename, k) = k(Fs.FsLayer.list(filename))
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
    pub def runWithFileSystem(f: Unit -> a \ ef): a \ (ef - DirList) + FileSystem =
        run { f() } with handler DirList {
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

    ///
    /// Middleware that intercepts the `DirList` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(DirList.list(r(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(Result.flatMap(p -> DirList.list(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(Result.flatMap(p -> DirList.list(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(Result.flatMap(p -> DirList.list(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(Result.flatMap(p -> DirList.list(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler DirList {
            def list(filename, k) = k(Result.flatMap(p -> DirList.list(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `DirList` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks.
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - DirList) + DirList =
        let resolve = p -> Fs.FsLayer.resolveRealPathOrSelf(p);
        run { f() } with handler DirList {
            def list(filename, k) = k(DirList.list(resolve(filename)))
        }

}
