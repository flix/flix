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
    use Fs.Size

    ///
    /// An effect used to read file contents (string, lines, bytes).
    ///
    pub eff FileRead {

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

        ///
        /// Returns a vector of all the bytes in the given file `f`.
        ///
        def readBytes(f: String): Result[IoError, Vector[Int8]]

    }

    ///
    /// Handles the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileRead) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                y -> runWithFileSystem(() -> f(y))
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
    pub def runWithFileSystem(f: Unit -> a \ ef): a \ (ef - FileRead) + FileSystem =
        run { f() } with handler FileRead {
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
    /// Middleware that intercepts the `FileRead` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(p -> FileRead.read(p), c(filename)))
            def readLines(filename, k) = k(Result.flatMap(p -> FileRead.readLines(p), c(filename)))
            def readBytes(filename, k) = k(Result.flatMap(p -> FileRead.readBytes(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(p -> FileRead.read(p), c(filename)))
            def readLines(filename, k) = k(Result.flatMap(p -> FileRead.readLines(p), c(filename)))
            def readBytes(filename, k) = k(Result.flatMap(p -> FileRead.readBytes(p), c(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks.
    ///
    /// Action ops (`read`, `readLines`, `readBytes`): resolve path with fallback to original.
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let resolve = p -> Fs.FsLayer.resolveRealPathOrSelf(p);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(FileRead.read(resolve(filename)))
            def readLines(filename, k) = k(FileRead.readLines(resolve(filename)))
            def readBytes(filename, k) = k(FileRead.readBytes(resolve(filename)))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, rejecting reads of files
    /// larger than `maxBytes`. The file size is checked via `FileStat.size` before
    /// reading. For string-based reads, the limit applies to the on-disk file size
    /// (not the in-memory string length).
    ///
    pub def withTransferLimit(maxSize: Size, f: Unit -> a \ ef): a \ (ef - FileRead) + {FileRead, FileStat} =
        let check = sz -> Fs.FsLayer.checkTransferLimit(maxSize, sz);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Result.flatMap(_ -> FileRead.read(filename), Result.flatMap(check, FileStat.size(filename))))
            def readLines(filename, k) = k(Result.flatMap(_ -> FileRead.readLines(filename), Result.flatMap(check, FileStat.size(filename))))
            def readBytes(filename, k) = k(Result.flatMap(_ -> FileRead.readBytes(filename), Result.flatMap(check, FileStat.size(filename))))
        }

    ///
    /// Middleware that intercepts the `FileRead` effect, verifying file checksums
    /// on read. If a sidecar checksum file exists (e.g., `"data.txt.sha256"` when
    /// `sidecar` is `".sha256"`), the file's contents are hashed and compared
    /// against the stored checksum.
    ///
    /// `hash` is a pure function from file bytes to digest bytes.
    /// `sidecar` is the file suffix (e.g., `".sha256"`).
    ///
    /// If no sidecar file exists, the read proceeds normally.
    /// If the checksum does not match, returns `Err(IoError(ChecksumMismatch, ...))`.
    ///
    pub def withChecksum(hash: Vector[Int8] -> Vector[Int8], sidecar: String,
                         f: Unit -> a \ ef): a \ (ef - FileRead) + FileRead =
        let computeHex = bytes -> Fs.FsLayer.bytesToHex(hash(bytes));
        let doReadBytes = file -> FileRead.readBytes(file);
        let doReadSidecar = file -> FileRead.read(file);
        run { f() } with handler FileRead {
            def read(filename, k)      = k(Fs.FsLayer.checksumOnRead(filename, sidecar, computeHex, FileRead.read(filename), doReadBytes, doReadSidecar))
            def readLines(filename, k) = k(Fs.FsLayer.checksumOnRead(filename, sidecar, computeHex, FileRead.readLines(filename), doReadBytes, doReadSidecar))
            def readBytes(filename, k) = k(Fs.FsLayer.checksumOnReadBytes(filename, sidecar, computeHex, FileRead.readBytes(filename), doReadSidecar))
        }

}
