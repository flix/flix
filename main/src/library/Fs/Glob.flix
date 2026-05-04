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

pub mod Fs.Glob {

    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPost}

    ///
    /// An effect used to find files matching a glob pattern.
    ///
    pub eff Glob {

        ///
        /// Returns a list of paths under `base` that match the given glob `pattern`.
        ///
        def glob(base: String, pattern: String): Result[IoError, List[String]]

    }

    ///
    /// Handles the `Glob` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Glob` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Glob) + IO = x ->
        run { f(x) } with handler Glob {
            def glob(base, pattern, k) = k(Fs.FsLayer.glob(base, pattern))
        }

    ///
    /// Runs the `Glob` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Glob` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Glob) + IO = handle(f)()

    ///
    /// Handles the `Glob` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def runWithFileSystem(f: Unit -> a \ ef): a \ (ef - Glob) + FileSystem =
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Fs.FileSystem.glob(base, pattern))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - Glob) + {Glob, Logger} =
        run { f() } with handler Glob {
            def glob(base, pattern, k) = {
                logPre("glob", base);
                let result = Glob.glob(base, pattern);
                logPost("glob", base, ls -> "${List.length(ls)} matches", result);
                k(result)
            }
        }

    ///
    /// Middleware that intercepts the `Glob` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Glob.glob(r(base), pattern))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, validating that all base paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Result.flatMap(p -> Glob.glob(p, pattern), c(base)))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, validating that all base paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Result.flatMap(p -> Glob.glob(p, pattern), c(base)))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, validating that all base paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Result.flatMap(p -> Glob.glob(p, pattern), c(base)))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, validating that all base paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Result.flatMap(p -> Glob.glob(p, pattern), c(base)))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, validating that all base paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Result.flatMap(p -> Glob.glob(p, pattern), c(base)))
        }

    ///
    /// Middleware that intercepts the `Glob` effect, resolving all base paths
    /// to their real filesystem targets by following symlinks.
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - Glob) + Glob =
        let resolve = p -> Fs.FsLayer.resolveRealPathOrSelf(p);
        run { f() } with handler Glob {
            def glob(base, pattern, k) = k(Glob.glob(resolve(base), pattern))
        }

}
