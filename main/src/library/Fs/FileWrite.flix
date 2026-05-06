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

pub mod Fs.FileWrite {

    use Fs.FileStat
    use Fs.FileRead
    use Fs.FileSystem
    use Fs.FileWrite
    use Fs.FsLayer.{logPre, logPre2, logPost, logPost2}
    use Fs.Size
    use Fs.Size.bytes
    use Fs.CopyOption
    use Fs.MoveOption

    ///
    /// An effect used to write files, append to files, truncate files, and create directories.
    ///
    pub eff FileWrite {

        ///
        /// Writes `str` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def write(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Writes `lines` to the given file `f`.
        ///
        /// Creates `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Appends `str` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def append(data: {str = String}, f: String): Result[IoError, Unit]

        ///
        /// Appends `lines` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

        ///
        /// Moves (renames) the file or directory `src` to `dst` with the given `options`.
        ///
        def moveWith(src: {src = String}, dst: String, opts: Set[MoveOption]): Result[IoError, Unit]

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

        ///
        /// Creates the directory `d` and all its parent directories.
        ///
        def mkDirs(d: String): Result[IoError, Unit]

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

    ///
    /// Convenience function: copies `src` to `dst` with no options.
    ///
    pub def copy(src: {src = String}, dst: String): Result[IoError, Unit] \ FileWrite =
        Fs.FileWrite.copyWith(src, dst, Set.empty())

    ///
    /// Convenience function: moves `src` to `dst` with no options.
    ///
    pub def move(src: {src = String}, dst: String): Result[IoError, Unit] \ FileWrite =
        Fs.FileWrite.moveWith(src, dst, Set.empty())

    ///
    /// Handles the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileWrite) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                y -> runWithFileSystem(() -> f(y))
            )(x)
        )

    ///
    /// Runs the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileWrite) + IO = handle(f)()

    ///
    /// Handles the `FileWrite` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def runWithFileSystem(f: Unit -> a \ ef): a \ (ef - FileWrite) + FileSystem =
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Fs.FileSystem.write(data, file))
            def writeLines(data, file, k)   = k(Fs.FileSystem.writeLines(data, file))
            def writeBytes(data, file, k)   = k(Fs.FileSystem.writeBytes(data, file))
            def append(data, file, k)       = k(Fs.FileSystem.append(data, file))
            def appendLines(data, file, k)  = k(Fs.FileSystem.appendLines(data, file))
            def appendBytes(data, file, k)  = k(Fs.FileSystem.appendBytes(data, file))
            def truncate(file, k)           = k(Fs.FileSystem.truncate(file))
            def copyWith(src, dst, opts, k) = k(Fs.FileSystem.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k) = k(Fs.FileSystem.moveWith(src, dst, opts))
            def delete(file, k)             = k(Fs.FileSystem.delete(file))
            def mkDir(d, k)                 = k(Fs.FileSystem.mkDir(d))
            def mkDirs(d, k)                = k(Fs.FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)        = k(Fs.FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileWrite) + {FileWrite, Logger} =
        run { f() } with handler FileWrite {
            def write(data, file, k) = {
                logPre("write", file);
                let result = FileWrite.write(data, file);
                logPost("write", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def writeLines(data, file, k) = {
                logPre("writeLines", file);
                let result = FileWrite.writeLines(data, file);
                logPost("writeLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def writeBytes(data, file, k) = {
                logPre("writeBytes", file);
                let result = FileWrite.writeBytes(data, file);
                logPost("writeBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def append(data, file, k) = {
                logPre("append", file);
                let result = FileWrite.append(data, file);
                logPost("append", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def appendLines(data, file, k) = {
                logPre("appendLines", file);
                let result = FileWrite.appendLines(data, file);
                logPost("appendLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def appendBytes(data, file, k) = {
                logPre("appendBytes", file);
                let result = FileWrite.appendBytes(data, file);
                logPost("appendBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def truncate(file, k) = {
                logPre("truncate", file);
                let result = FileWrite.truncate(file);
                logPost("truncate", file, _ -> "done", result);
                k(result)
            }
            def copyWith(src, dst, opts, k) = {
                logPre2("copy", src#src, dst);
                let result = FileWrite.copyWith(src, dst, opts);
                logPost2("copy", src#src, dst, result);
                k(result)
            }
            def moveWith(src, dst, opts, k) = {
                logPre2("move", src#src, dst);
                let result = FileWrite.moveWith(src, dst, opts);
                logPost2("move", src#src, dst, result);
                k(result)
            }
            def delete(file, k) = {
                logPre("delete", file);
                let result = FileWrite.delete(file);
                logPost("delete", file, _ -> "done", result);
                k(result)
            }
            def mkDir(d, k) = {
                logPre("mkDir", d);
                let result = FileWrite.mkDir(d);
                logPost("mkDir", d, _ -> "done", result);
                k(result)
            }
            def mkDirs(d, k) = {
                logPre("mkDirs", d);
                let result = FileWrite.mkDirs(d);
                logPost("mkDirs", d, _ -> "done", result);
                k(result)
            }
            def mkTempDir(prefix, k) = {
                logPre("mkTempDir", prefix);
                let result = FileWrite.mkTempDir(prefix);
                logPost("mkTempDir", prefix, path -> path, result);
                k(result)
            }
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged. The `mkTempDir` prefix is not resolved.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(FileWrite.write(data, r(file)))
            def writeLines(data, file, k)   = k(FileWrite.writeLines(data, r(file)))
            def writeBytes(data, file, k)   = k(FileWrite.writeBytes(data, r(file)))
            def append(data, file, k)       = k(FileWrite.append(data, r(file)))
            def appendLines(data, file, k)  = k(FileWrite.appendLines(data, r(file)))
            def appendBytes(data, file, k)  = k(FileWrite.appendBytes(data, r(file)))
            def truncate(file, k)           = k(FileWrite.truncate(r(file)))
            def copyWith(src, dst, opts, k) = k(FileWrite.copyWith(src = r(src#src), r(dst), opts))
            def moveWith(src, dst, opts, k) = k(FileWrite.moveWith(src = r(src#src), r(dst), opts))
            def delete(file, k)             = k(FileWrite.delete(r(file)))
            def mkDir(d, k)                 = k(FileWrite.mkDir(r(d)))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(r(d)))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since it creates directories outside the chroot.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(p -> FileWrite.write(data, p), c(file)))
            def writeLines(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeBytes(data, p), c(file)))
            def append(data, file, k)       = k(Result.flatMap(p -> FileWrite.append(data, p), c(file)))
            def appendLines(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendBytes(data, p), c(file)))
            def truncate(file, k)           = k(Result.flatMap(p -> FileWrite.truncate(p), c(file)))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)             = k(Result.flatMap(p -> FileWrite.delete(p), c(file)))
            def mkDir(d, k)                 = k(Result.flatMap(p -> FileWrite.mkDir(p), c(d)))
            def mkDirs(d, k)                = k(Result.flatMap(p -> FileWrite.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)       = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted inside a chroot")))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since the temp directory location cannot be verified.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(p -> FileWrite.write(data, p), c(file)))
            def writeLines(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeBytes(data, p), c(file)))
            def append(data, file, k)       = k(Result.flatMap(p -> FileWrite.append(data, p), c(file)))
            def appendLines(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendBytes(data, p), c(file)))
            def truncate(file, k)           = k(Result.flatMap(p -> FileWrite.truncate(p), c(file)))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)             = k(Result.flatMap(p -> FileWrite.delete(p), c(file)))
            def mkDir(d, k)                 = k(Result.flatMap(p -> FileWrite.mkDir(p), c(d)))
            def mkDirs(d, k)                = k(Result.flatMap(p -> FileWrite.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)       = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted with an allow list")))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is allowed since deny lists are default-allow.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(p -> FileWrite.write(data, p), c(file)))
            def writeLines(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeBytes(data, p), c(file)))
            def append(data, file, k)       = k(Result.flatMap(p -> FileWrite.append(data, p), c(file)))
            def appendLines(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendBytes(data, p), c(file)))
            def truncate(file, k)           = k(Result.flatMap(p -> FileWrite.truncate(p), c(file)))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)             = k(Result.flatMap(p -> FileWrite.delete(p), c(file)))
            def mkDir(d, k)                 = k(Result.flatMap(p -> FileWrite.mkDir(p), c(d)))
            def mkDirs(d, k)                = k(Result.flatMap(p -> FileWrite.mkDirs(p), c(d)))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since the temp directory location cannot be verified.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(p -> FileWrite.write(data, p), c(file)))
            def writeLines(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeBytes(data, p), c(file)))
            def append(data, file, k)       = k(Result.flatMap(p -> FileWrite.append(data, p), c(file)))
            def appendLines(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendBytes(data, p), c(file)))
            def truncate(file, k)           = k(Result.flatMap(p -> FileWrite.truncate(p), c(file)))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)             = k(Result.flatMap(p -> FileWrite.delete(p), c(file)))
            def mkDir(d, k)                 = k(Result.flatMap(p -> FileWrite.mkDir(p), c(d)))
            def mkDirs(d, k)                = k(Result.flatMap(p -> FileWrite.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)       = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted with an allow glob")))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is allowed since deny globs are default-allow.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(p -> FileWrite.write(data, p), c(file)))
            def writeLines(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)   = k(Result.flatMap(p -> FileWrite.writeBytes(data, p), c(file)))
            def append(data, file, k)       = k(Result.flatMap(p -> FileWrite.append(data, p), c(file)))
            def appendLines(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)  = k(Result.flatMap(p -> FileWrite.appendBytes(data, p), c(file)))
            def truncate(file, k)           = k(Result.flatMap(p -> FileWrite.truncate(p), c(file)))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k) = k(Result.flatMap(s -> Result.flatMap(d -> FileWrite.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)             = k(Result.flatMap(p -> FileWrite.delete(p), c(file)))
            def mkDir(d, k)                 = k(Result.flatMap(p -> FileWrite.mkDir(p), c(d)))
            def mkDirs(d, k)                = k(Result.flatMap(p -> FileWrite.mkDirs(p), c(d)))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks.
    ///
    /// Action ops (`write`, `append`, `copy`, etc.): resolve path with fallback to original.
    /// `mkTempDir`: passed through unchanged (no path to resolve).
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let resolve = p -> Fs.FsLayer.resolveRealPathOrSelf(p);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(FileWrite.write(data, resolve(file)))
            def writeLines(data, file, k)   = k(FileWrite.writeLines(data, resolve(file)))
            def writeBytes(data, file, k)   = k(FileWrite.writeBytes(data, resolve(file)))
            def append(data, file, k)       = k(FileWrite.append(data, resolve(file)))
            def appendLines(data, file, k)  = k(FileWrite.appendLines(data, resolve(file)))
            def appendBytes(data, file, k)  = k(FileWrite.appendBytes(data, resolve(file)))
            def truncate(file, k)           = k(FileWrite.truncate(resolve(file)))
            def copyWith(src, dst, opts, k) = k(FileWrite.copyWith(src = resolve(src#src), resolve(dst), opts))
            def moveWith(src, dst, opts, k) = k(FileWrite.moveWith(src = resolve(src#src), resolve(dst), opts))
            def delete(file, k)             = k(FileWrite.delete(resolve(file)))
            def mkDir(d, k)                 = k(FileWrite.mkDir(resolve(d)))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(resolve(d)))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, logging each write
    /// operation via the `Logger` effect at `Debug` level without performing
    /// the actual filesystem modification. All write operations return `Ok(())`;
    /// `mkTempDir` returns `Ok("<dry-run>")`.
    ///
    pub def withDryRun(f: Unit -> a \ ef): a \ (ef - FileWrite) + Logger =
        use Logger.Severity;
        use RichString.{text, bold, cyan, yellow, gray};
        let tag = yellow("[dry-run]");
        run { f() } with handler FileWrite {
            def write(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("write") + text(" ") + cyan(file) + gray(" (${String.length(data#str)} chars)"));
                k(Ok(()))
            }
            def writeLines(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("writeLines") + text(" ") + cyan(file) + gray(" (${List.length(data#lines)} lines)"));
                k(Ok(()))
            }
            def writeBytes(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("writeBytes") + text(" ") + cyan(file) + gray(" (${Vector.length(data)} bytes)"));
                k(Ok(()))
            }
            def append(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("append") + text(" ") + cyan(file) + gray(" (${String.length(data#str)} chars)"));
                k(Ok(()))
            }
            def appendLines(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("appendLines") + text(" ") + cyan(file) + gray(" (${List.length(data#lines)} lines)"));
                k(Ok(()))
            }
            def appendBytes(data, file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("appendBytes") + text(" ") + cyan(file) + gray(" (${Vector.length(data)} bytes)"));
                k(Ok(()))
            }
            def truncate(file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("truncate") + text(" ") + cyan(file));
                k(Ok(()))
            }
            def copyWith(src, dst, _opts, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("copy") + text(" ") + cyan(src#src) + gray(" -> ") + cyan(dst));
                k(Ok(()))
            }
            def moveWith(src, dst, _opts, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("move") + text(" ") + cyan(src#src) + gray(" -> ") + cyan(dst));
                k(Ok(()))
            }
            def delete(file, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("delete") + text(" ") + cyan(file));
                k(Ok(()))
            }
            def mkDir(d, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("mkDir") + text(" ") + cyan(d));
                k(Ok(()))
            }
            def mkDirs(d, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("mkDirs") + text(" ") + cyan(d));
                k(Ok(()))
            }
            def mkTempDir(prefix, k) = {
                Logger.log(Severity.Debug, tag + text(" ") + bold("mkTempDir") + text(" ") + cyan(prefix));
                k(Ok("<dry-run>"))
            }
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, blocking all write
    /// operations with a `PermissionDenied` error. This is useful for
    /// sandboxing code that should only observe the filesystem.
    ///
    pub def withReadOnly(f: Unit -> a \ ef): a \ (ef - FileWrite) =
        use IoError.IoError;
        use IoError.ErrorKind;
        let denied = op -> Err(IoError(ErrorKind.PermissionDenied, "read-only: ${op} is not permitted"));
        run { f() } with handler FileWrite {
            def write(_data, _file, k)         = k(denied("write"))
            def writeLines(_data, _file, k)    = k(denied("writeLines"))
            def writeBytes(_data, _file, k)    = k(denied("writeBytes"))
            def append(_data, _file, k)        = k(denied("append"))
            def appendLines(_data, _file, k)   = k(denied("appendLines"))
            def appendBytes(_data, _file, k)   = k(denied("appendBytes"))
            def truncate(_file, k)             = k(denied("truncate"))
            def copyWith(_src, _dst, _opts, k) = k(denied("copy"))
            def moveWith(_src, _dst, _opts, k) = k(denied("move"))
            def delete(_file, k)               = k(denied("delete"))
            def mkDir(_d, k)                   = k(denied("mkDir"))
            def mkDirs(_d, k)                  = k(denied("mkDirs"))
            def mkTempDir(_prefix, k)          = k(Err(IoError(ErrorKind.PermissionDenied, "read-only: mkTempDir is not permitted")))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, making `write`,
    /// `writeLines`, and `writeBytes` atomic. Data is first written to a
    /// temporary file in the same directory, then atomically moved into place.
    /// On failure the temporary file is cleaned up.
    ///
    /// All other operations (append, truncate, copy, move, delete, mkdir) are
    /// passed through unchanged.
    ///
    pub def withAtomicWrite(f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let mv  = (tmp, dst) -> FileWrite.moveWith(src = tmp, dst, Set#{Fs.MoveOption.AtomicMove, Fs.MoveOption.ReplaceExisting});
        let del = tmp -> FileWrite.delete(tmp);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Fs.FsLayer.atomicWrite(file, tmp -> FileWrite.write(data, tmp), mv, del))
            def writeLines(data, file, k)   = k(Fs.FsLayer.atomicWrite(file, tmp -> FileWrite.writeLines(data, tmp), mv, del))
            def writeBytes(data, file, k)   = k(Fs.FsLayer.atomicWrite(file, tmp -> FileWrite.writeBytes(data, tmp), mv, del))
            def append(data, file, k)       = k(FileWrite.append(data, file))
            def appendLines(data, file, k)  = k(FileWrite.appendLines(data, file))
            def appendBytes(data, file, k)  = k(FileWrite.appendBytes(data, file))
            def truncate(file, k)           = k(FileWrite.truncate(file))
            def copyWith(src, dst, opts, k) = k(FileWrite.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k) = k(FileWrite.moveWith(src, dst, opts))
            def delete(file, k)             = k(FileWrite.delete(file))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, creating a backup
    /// of existing files before destructive operations. Before `write`,
    /// `writeLines`, `writeBytes`, `truncate`, and `delete`, the existing
    /// file is copied to `file + suffix` (e.g., `"data.txt.bak"` when
    /// suffix is `".bak"`).
    ///
    /// For `copyWith` and `moveWith`, the destination file is backed up.
    ///
    /// If the file does not exist, no backup is created and the operation
    /// proceeds normally. If the backup fails for any other reason, the
    /// original operation is aborted and the backup error is returned.
    ///
    /// If a backup file already exists, it is overwritten. Only the most
    /// recent previous version of each file is preserved.
    ///
    /// Append, mkdir, and mkTempDir operations are passed through unchanged.
    ///
    pub def withBackup(suffix: String, f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let doCopy = (src, dst) -> FileWrite.copyWith(src = src, dst, Set#{Fs.CopyOption.ReplaceExisting});
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileWrite.write(data, file)))
            def writeLines(data, file, k)   = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileWrite.writeLines(data, file)))
            def writeBytes(data, file, k)   = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileWrite.writeBytes(data, file)))
            def truncate(file, k)           = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileWrite.truncate(file)))
            def delete(file, k)             = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileWrite.delete(file)))
            def copyWith(src, dst, opts, k) = k(Fs.FsLayer.backup(dst, suffix, doCopy, () -> FileWrite.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k) = k(Fs.FsLayer.backup(dst, suffix, doCopy, () -> FileWrite.moveWith(src, dst, opts)))
            def append(data, file, k)       = k(FileWrite.append(data, file))
            def appendLines(data, file, k)  = k(FileWrite.appendLines(data, file))
            def appendBytes(data, file, k)  = k(FileWrite.appendBytes(data, file))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, automatically creating
    /// parent directories before write, append, copy, and move operations.
    ///
    /// Before `write`, `writeLines`, `writeBytes`, `append`, `appendLines`,
    /// `appendBytes`, `copyWith`, and `moveWith`, the parent directory of the
    /// target file is created via `mkDirs`. If the parent directory already
    /// exists, this is a no-op.
    ///
    /// If parent directory creation fails, the original operation is aborted
    /// and the error is returned.
    ///
    /// `truncate`, `delete`, `mkDir`, `mkDirs`, and `mkTempDir` are passed
    /// through unchanged.
    ///
    pub def withMkParentDirs(f: Unit -> a \ ef): a \ (ef - FileWrite) + FileWrite =
        let mk = d -> FileWrite.mkDirs(d);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.write(data, file)))
            def writeLines(data, file, k)   = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.writeLines(data, file)))
            def writeBytes(data, file, k)   = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.writeBytes(data, file)))
            def append(data, file, k)       = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.append(data, file)))
            def appendLines(data, file, k)  = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.appendLines(data, file)))
            def appendBytes(data, file, k)  = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileWrite.appendBytes(data, file)))
            def copyWith(src, dst, opts, k) = k(Fs.FsLayer.ensureParentDirs(dst, mk, () -> FileWrite.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k) = k(Fs.FsLayer.ensureParentDirs(dst, mk, () -> FileWrite.moveWith(src, dst, opts)))
            def truncate(file, k)           = k(FileWrite.truncate(file))
            def delete(file, k)             = k(FileWrite.delete(file))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, automatically rotating
    /// files when they reach a size threshold before append operations.
    ///
    /// Before `append`, `appendLines`, and `appendBytes`, the target file's size
    /// is checked via `FileStat.size`. If it meets or exceeds `maxSize`, existing
    /// rotated files are shifted (`file.1` → `file.2`, etc.), the oldest
    /// (`file.{maxFiles}`) is deleted, and the current file is moved to `file.1`.
    /// The append then writes to the now-absent original path.
    ///
    /// All other operations are passed through unchanged.
    ///
    pub def withSizeRotation(maxSize: Size, maxFiles: Int32, f: Unit -> a \ ef): a \ (ef - FileWrite) + {FileWrite, FileStat} =
        let doSize   = file -> FileStat.size(file);
        let doMove   = (src, dst) -> FileWrite.moveWith(src = src, dst, Set#{Fs.MoveOption.ReplaceExisting});
        let doDelete = file -> FileWrite.delete(file);
        run { f() } with handler FileWrite {
            // Append ops — rotate if needed before appending
            def append(data, file, k)       = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileWrite.append(data, file)))
            def appendLines(data, file, k)  = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileWrite.appendLines(data, file)))
            def appendBytes(data, file, k)  = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileWrite.appendBytes(data, file)))
            // Other write ops — pass through
            def write(data, file, k)        = k(FileWrite.write(data, file))
            def writeLines(data, file, k)   = k(FileWrite.writeLines(data, file))
            def writeBytes(data, file, k)   = k(FileWrite.writeBytes(data, file))
            def truncate(file, k)           = k(FileWrite.truncate(file))
            def copyWith(src, dst, opts, k) = k(FileWrite.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k) = k(FileWrite.moveWith(src, dst, opts))
            def delete(file, k)             = k(FileWrite.delete(file))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, rejecting write and
    /// append operations where the payload exceeds `maxBytes`. For `copyWith`,
    /// the source file size is checked via `FileStat.size`.
    ///
    /// String-based operations estimate byte count as `charCount * 4` (the
    /// maximum bytes per character in UTF-8). Use `writeBytes`/`appendBytes`
    /// for exact enforcement.
    ///
    /// Operations that don't transfer data (`truncate`, `moveWith`, `delete`,
    /// `mkDir`, `mkDirs`, `mkTempDir`) are passed through unchanged.
    ///
    pub def withTransferLimit(maxSize: Size, f: Unit -> a \ ef): a \ (ef - FileWrite) + {FileWrite, FileStat} =
        let check = sz -> Fs.FsLayer.checkTransferLimit(maxSize, sz);
        let estStr = s -> Fs.FsLayer.estimateStringBytes(s);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(Result.flatMap(_ -> FileWrite.write(data, file), check(estStr(data#str))))
            def writeLines(data, file, k)   = k(Result.flatMap(_ -> FileWrite.writeLines(data, file), check(Fs.FsLayer.estimateStringListBytes(data#lines))))
            def writeBytes(data, file, k)   = k(Result.flatMap(_ -> FileWrite.writeBytes(data, file), check(bytes(Int32.toInt64(Vector.length(data))))))
            def append(data, file, k)       = k(Result.flatMap(_ -> FileWrite.append(data, file), check(estStr(data#str))))
            def appendLines(data, file, k)  = k(Result.flatMap(_ -> FileWrite.appendLines(data, file), check(Fs.FsLayer.estimateStringListBytes(data#lines))))
            def appendBytes(data, file, k)  = k(Result.flatMap(_ -> FileWrite.appendBytes(data, file), check(bytes(Int32.toInt64(Vector.length(data))))))
            def truncate(file, k)           = k(FileWrite.truncate(file))
            def copyWith(src, dst, opts, k) = k(Result.flatMap(sz -> Result.flatMap(_ -> FileWrite.copyWith(src, dst, opts), check(sz)), FileStat.size(src#src)))
            def moveWith(src, dst, opts, k) = k(FileWrite.moveWith(src, dst, opts))
            def delete(file, k)             = k(FileWrite.delete(file))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, tracking file modification
    /// times across writes and checking for external modifications before each write.
    ///
    /// Before `write`, `writeLines`, `writeBytes`, `append`, `appendLines`,
    /// `appendBytes`, and `truncate`, the file's current modification time is
    /// compared against the last recorded snapshot. If they differ, a `Conflict`
    /// error is returned. After a successful write, the modification time is
    /// re-snapshotted for subsequent checks.
    ///
    /// For `copyWith` and `moveWith`, the conflict check is applied to the
    /// destination path.
    ///
    /// After `delete`, the snapshot for that file is removed.
    ///
    /// `mkDir`, `mkDirs`, and `mkTempDir` are passed through unchanged.
    ///
    /// Note: Unlike the `FileSystem` version, this middleware does not snapshot
    /// on read operations. Conflict detection begins after the first write to
    /// each file, catching write-write conflicts from external processes.
    ///
    pub def withConflictCheck(f: Unit -> a \ ef): a \ (ef - FileWrite) + {FileWrite, FileStat} =
        region rc {
            let snapshots: MutMap[String, Int64, rc] = MutMap.empty(rc);
            let getModTime = file -> FileStat.modificationTime(file);
            let snap = file -> Fs.FsLayer.snapshotModTime(file, snapshots, getModTime);
            let check = file -> Fs.FsLayer.conflictCheck(file, snapshots, getModTime);
            let checkAndResnap = (file, doWrite) ->
                forM (_ <- check(file); v <- doWrite())
                    yield { snap(file); v };
            run { f() } with handler FileWrite {
                // ── Write ops — conflict check before, re-snapshot after ──
                def write(data, file, k)        = k(checkAndResnap(file, () -> FileWrite.write(data, file)))
                def writeLines(data, file, k)   = k(checkAndResnap(file, () -> FileWrite.writeLines(data, file)))
                def writeBytes(data, file, k)   = k(checkAndResnap(file, () -> FileWrite.writeBytes(data, file)))
                def append(data, file, k)       = k(checkAndResnap(file, () -> FileWrite.append(data, file)))
                def appendLines(data, file, k)  = k(checkAndResnap(file, () -> FileWrite.appendLines(data, file)))
                def appendBytes(data, file, k)  = k(checkAndResnap(file, () -> FileWrite.appendBytes(data, file)))
                def truncate(file, k)           = k(checkAndResnap(file, () -> FileWrite.truncate(file)))
                def delete(file, k) = {
                    match check(file) {
                        case Err(e) => k(Err(e))
                        case Ok(_)  =>
                            let result = FileWrite.delete(file);
                            match result {
                                case Ok(_) => MutMap.remove(file, snapshots)
                                case _     => ()
                            };
                            k(result)
                    }
                }
                // ── Copy/move — conflict check on destination ──
                def copyWith(src, dst, opts, k) = k(checkAndResnap(dst, () -> FileWrite.copyWith(src, dst, opts)))
                def moveWith(src, dst, opts, k) = k(checkAndResnap(dst, () -> FileWrite.moveWith(src, dst, opts)))
                // ── Directory ops — pass through ──
                def mkDir(d, k)                 = k(FileWrite.mkDir(d))
                def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
                def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
            }
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, computing checksums
    /// after write operations and storing them as sidecar files.
    ///
    /// `hash` is a pure function from file bytes to digest bytes.
    /// `sidecar` is the file suffix (e.g., `".sha256"`).
    ///
    /// After `write`, `writeLines`, `writeBytes`, `append`, `appendLines`,
    /// `appendBytes`, `truncate`, and `copyWith`, the file is re-read and
    /// its checksum sidecar is updated.
    ///
    /// After `moveWith`, the source's sidecar is moved to the destination path.
    /// After `delete`, the sidecar file is also removed.
    ///
    /// `mkDir`, `mkDirs`, and `mkTempDir` are passed through unchanged.
    ///
    pub def withChecksum(hash: Vector[Int8] -> Vector[Int8], sidecar: String,
                         f: Unit -> a \ ef): a \ (ef - FileWrite) + {FileWrite, FileRead} =
        let computeHex = bytes -> Fs.FsLayer.bytesToHex(hash(bytes));
        let updateSidecar = file -> Fs.FsLayer.updateSidecar(file, sidecar, computeHex, f1 -> FileRead.readBytes(f1), (str, f1) -> FileWrite.write(str = str, f1));
        let afterWrite = (file, doWrite) -> Fs.FsLayer.checksumAfterWrite(file, doWrite, updateSidecar);
        let doMoveSidecar = (s, d) -> FileWrite.moveWith(src = s, d, Set#{Fs.MoveOption.ReplaceExisting});
        let doDeleteSidecar = file -> FileWrite.delete(file);
        run { f() } with handler FileWrite {
            def write(data, file, k)        = k(afterWrite(file, () -> FileWrite.write(data, file)))
            def writeLines(data, file, k)   = k(afterWrite(file, () -> FileWrite.writeLines(data, file)))
            def writeBytes(data, file, k)   = k(afterWrite(file, () -> FileWrite.writeBytes(data, file)))
            def append(data, file, k)       = k(afterWrite(file, () -> FileWrite.append(data, file)))
            def appendLines(data, file, k)  = k(afterWrite(file, () -> FileWrite.appendLines(data, file)))
            def appendBytes(data, file, k)  = k(afterWrite(file, () -> FileWrite.appendBytes(data, file)))
            def truncate(file, k)           = k(afterWrite(file, () -> FileWrite.truncate(file)))
            def copyWith(src, dst, opts, k) = k(afterWrite(dst, () -> FileWrite.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k) = k(Fs.FsLayer.checksumAfterMove(src#src, dst, sidecar, FileWrite.moveWith(src, dst, opts), doMoveSidecar))
            def delete(file, k)             = k(Fs.FsLayer.checksumAfterDelete(file, sidecar, FileWrite.delete(file), doDeleteSidecar))
            def mkDir(d, k)                 = k(FileWrite.mkDir(d))
            def mkDirs(d, k)                = k(FileWrite.mkDirs(d))
            def mkTempDir(prefix, k)        = k(FileWrite.mkTempDir(prefix))
        }

}
