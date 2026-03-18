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

pub mod Fs.FileRead {

    use Fs.FileRead
    use Fs.FileStat
    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPost}

    ///
    /// Handles the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileRead) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                handleWithFileSystem(f)
            )(x)
        )

    ///
    /// Runs the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileRead) + IO = handle(f)()

    ///
    /// Handles the `FileRead` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - FileRead) + FileSystem = x ->
        run { f(x) } with handler FileRead {
            def read(filename, k)      = k(Fs.FileSystem.read(filename))
            def readLines(filename, k) = k(Fs.FileSystem.readLines(filename))
            def readBytes(filename, k) = k(Fs.FileSystem.readBytes(filename))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileRead) + {FileRead, Logger} =
        run { f() } with handler FileRead {
            def read(filename, k) = {
                logPre("read", filename);
                let result = FileRead.read(filename);
                logPost("read", filename, s -> "${String.length(s)} chars", result);
                k(result)
            }
            def readLines(filename, k) = {
                logPre("readLines", filename);
                let result = FileRead.readLines(filename);
                logPost("readLines", filename, ls -> "${List.length(ls)} lines", result);
                k(result)
            }
            def readBytes(filename, k) = {
                logPre("readBytes", filename);
                let result = FileRead.readBytes(filename);
                logPost("readBytes", filename, bs -> "${Vector.length(bs)} bytes", result);
                k(result)
            }
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(FileRead.read(r(filename)))
            def readLines(filename, k) = k(FileRead.readLines(r(filename)))
            def readBytes(filename, k) = k(FileRead.readBytes(r(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(p -> FileRead.read(p), c(filename)))
            def readLines(filename, k) = k(Result.flatMap(p -> FileRead.readLines(p), c(filename)))
            def readBytes(filename, k) = k(Result.flatMap(p -> FileRead.readBytes(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(p -> FileRead.read(p), c(filename)))
            def readLines(filename, k) = k(Result.flatMap(p -> FileRead.readLines(p), c(filename)))
            def readBytes(filename, k) = k(Result.flatMap(p -> FileRead.readBytes(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(p -> FileRead.read(p), c(filename)))
            def readLines(filename, k) = k(Result.flatMap(p -> FileRead.readLines(p), c(filename)))
            def readBytes(filename, k) = k(Result.flatMap(p -> FileRead.readBytes(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, rejecting reads of files
    /// larger than `maxBytes`. The file size is checked via `FileStat.size` before
    /// reading. For string-based reads, the limit applies to the on-disk file size
    /// (not the in-memory string length).
    ///
    pub def withTransferLimit(maxBytes: Int64, f: Unit -> a \ ef): a \ (ef - FileRead) + {FileRead, FileStat} =
        let check = sz -> Fs.FsLayer.checkTransferLimit(maxBytes, sz);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(_ -> FileRead.read(filename), Result.flatMap(check, FileStat.size(filename))))
            def readLines(filename, k) = k(Result.flatMap(_ -> FileRead.readLines(filename), Result.flatMap(check, FileStat.size(filename))))
            def readBytes(filename, k) = k(Result.flatMap(_ -> FileRead.readBytes(filename), Result.flatMap(check, FileStat.size(filename))))
        }

}
