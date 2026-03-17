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

    use Fs.Glob
    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPost}

    ///
    /// Handles the `Glob` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - Glob) + FileSystem = x ->
        run { f(x) } with handler Glob {
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

}
