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

pub mod Fs.FileSystem {

    use Fs.FileSystem
    use Fs.FsLayer.{logPre, logPre2, logPost, logPost2}
    use Fs.Size
    use Fs.Size.bytes
    use IoError.IoError
    use IoError.ErrorKind
    use Time.Clock
    use Fs.CopyOption
    use Fs.MoveOption

    ///
    /// An effect used to perform all filesystem operations.
    ///
    pub eff FileSystem {

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

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

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

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

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

        ///
        /// Returns a list with the names of all files and directories in the given directory `f`.
        ///
        def list(f: String): Result[IoError, List[String]]

        ///
        /// Returns a list of paths under `base` that match the given glob `pattern`.
        ///
        def glob(base: String, pattern: String): Result[IoError, List[String]]

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
    pub def copy(src: {src = String}, dst: String): Result[IoError, Unit] \ FileSystem =
        Fs.FileSystem.copyWith(src, dst, Set.empty())

    ///
    /// Convenience function: moves `src` to `dst` with no options.
    ///
    pub def move(src: {src = String}, dst: String): Result[IoError, Unit] \ FileSystem =
        Fs.FileSystem.moveWith(src, dst, Set.empty())

    ///
    /// Handles the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileSystem) + IO = x ->
        run {
            f(x)
        } with handler FileSystem {
            def exists(filename, k)           = k(Fs.FsLayer.exists(filename))
            def isDirectory(filename, k)      = k(Fs.FsLayer.isDirectory(filename))
            def isRegularFile(filename, k)    = k(Fs.FsLayer.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(Fs.FsLayer.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(Fs.FsLayer.isReadable(filename))
            def isWritable(filename, k)       = k(Fs.FsLayer.isWritable(filename))
            def isExecutable(filename, k)     = k(Fs.FsLayer.isExecutable(filename))
            def accessTime(filename, k)       = k(Fs.FsLayer.accessTime(filename))
            def creationTime(filename, k)     = k(Fs.FsLayer.creationTime(filename))
            def modificationTime(filename, k) = k(Fs.FsLayer.modificationTime(filename))
            def size(filename, k)             = k(Fs.FsLayer.size(filename))
            def read(filename, k)             = k(Fs.FsLayer.read(filename))
            def readLines(filename, k)        = k(Fs.FsLayer.readLines(filename))
            def readBytes(filename, k)        = k(Fs.FsLayer.readBytes(filename))
            def list(filename, k)             = k(Fs.FsLayer.list(filename))
            def glob(base, pattern, k)        = k(Fs.FsLayer.glob(base, pattern))
            def write(data, file, k)          = k(Fs.FsLayer.write(data, file))
            def writeLines(data, file, k)     = k(Fs.FsLayer.writeLines(data, file))
            def writeBytes(data, file, k)     = k(Fs.FsLayer.writeBytes(data, file))
            def append(data, file, k)         = k(Fs.FsLayer.append(data, file))
            def appendLines(data, file, k)    = k(Fs.FsLayer.appendLines(data, file))
            def appendBytes(data, file, k)    = k(Fs.FsLayer.appendBytes(data, file))
            def truncate(file, k)             = k(Fs.FsLayer.truncate(file))
            def copyWith(src, dst, opts, k)   = k(Fs.FsLayer.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k)   = k(Fs.FsLayer.moveWith(src, dst, opts))
            def delete(file, k)               = k(Fs.FsLayer.delete(file))
            def mkDir(d, k)                   = k(Fs.FsLayer.mkDir(d))
            def mkDirs(d, k)                  = k(Fs.FsLayer.mkDirs(d))
            def mkTempDir(prefix, k)          = k(Fs.FsLayer.mkTempDir(prefix))
        }

    ///
    /// Runs the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileSystem) + IO = handle(f)()

    ///
    /// Middleware that intercepts the `FileSystem` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileSystem) + {FileSystem, Logger} =
        run { f() } with handler FileSystem {
            def exists(filename, k) = {
                logPre("exists", filename);
                let result = FileSystem.exists(filename);
                logPost("exists", filename, b -> "${b}", result);
                k(result)
            }
            def isDirectory(filename, k) = {
                logPre("isDirectory", filename);
                let result = FileSystem.isDirectory(filename);
                logPost("isDirectory", filename, b -> "${b}", result);
                k(result)
            }
            def isRegularFile(filename, k) = {
                logPre("isRegularFile", filename);
                let result = FileSystem.isRegularFile(filename);
                logPost("isRegularFile", filename, b -> "${b}", result);
                k(result)
            }
            def isSymbolicLink(filename, k) = {
                logPre("isSymbolicLink", filename);
                let result = FileSystem.isSymbolicLink(filename);
                logPost("isSymbolicLink", filename, b -> "${b}", result);
                k(result)
            }
            def isReadable(filename, k) = {
                logPre("isReadable", filename);
                let result = FileSystem.isReadable(filename);
                logPost("isReadable", filename, b -> "${b}", result);
                k(result)
            }
            def isWritable(filename, k) = {
                logPre("isWritable", filename);
                let result = FileSystem.isWritable(filename);
                logPost("isWritable", filename, b -> "${b}", result);
                k(result)
            }
            def isExecutable(filename, k) = {
                logPre("isExecutable", filename);
                let result = FileSystem.isExecutable(filename);
                logPost("isExecutable", filename, b -> "${b}", result);
                k(result)
            }
            def accessTime(filename, k) = {
                logPre("accessTime", filename);
                let result = FileSystem.accessTime(filename);
                logPost("accessTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def creationTime(filename, k) = {
                logPre("creationTime", filename);
                let result = FileSystem.creationTime(filename);
                logPost("creationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def modificationTime(filename, k) = {
                logPre("modificationTime", filename);
                let result = FileSystem.modificationTime(filename);
                logPost("modificationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def size(filename, k) = {
                logPre("size", filename);
                let result = FileSystem.size(filename);
                logPost("size", filename, s -> "${s}", result);
                k(result)
            }
            def read(filename, k) = {
                logPre("read", filename);
                let result = FileSystem.read(filename);
                logPost("read", filename, s -> "${String.length(s)} chars", result);
                k(result)
            }
            def readLines(filename, k) = {
                logPre("readLines", filename);
                let result = FileSystem.readLines(filename);
                logPost("readLines", filename, ls -> "${List.length(ls)} lines", result);
                k(result)
            }
            def readBytes(filename, k) = {
                logPre("readBytes", filename);
                let result = FileSystem.readBytes(filename);
                logPost("readBytes", filename, bs -> "${Vector.length(bs)} bytes", result);
                k(result)
            }
            def list(filename, k) = {
                logPre("list", filename);
                let result = FileSystem.list(filename);
                logPost("list", filename, ls -> "${List.length(ls)} entries", result);
                k(result)
            }
            def glob(base, pattern, k) = {
                logPre("glob", base);
                let result = FileSystem.glob(base, pattern);
                logPost("glob", base, ls -> "${List.length(ls)} matches", result);
                k(result)
            }
            def write(data, file, k) = {
                logPre("write", file);
                let result = FileSystem.write(data, file);
                logPost("write", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def writeLines(data, file, k) = {
                logPre("writeLines", file);
                let result = FileSystem.writeLines(data, file);
                logPost("writeLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def writeBytes(data, file, k) = {
                logPre("writeBytes", file);
                let result = FileSystem.writeBytes(data, file);
                logPost("writeBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def append(data, file, k) = {
                logPre("append", file);
                let result = FileSystem.append(data, file);
                logPost("append", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def appendLines(data, file, k) = {
                logPre("appendLines", file);
                let result = FileSystem.appendLines(data, file);
                logPost("appendLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def appendBytes(data, file, k) = {
                logPre("appendBytes", file);
                let result = FileSystem.appendBytes(data, file);
                logPost("appendBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def truncate(file, k) = {
                logPre("truncate", file);
                let result = FileSystem.truncate(file);
                logPost("truncate", file, _ -> "done", result);
                k(result)
            }
            def copyWith(src, dst, opts, k) = {
                logPre2("copy", src#src, dst);
                let result = FileSystem.copyWith(src, dst, opts);
                logPost2("copy", src#src, dst, result);
                k(result)
            }
            def moveWith(src, dst, opts, k) = {
                logPre2("move", src#src, dst);
                let result = FileSystem.moveWith(src, dst, opts);
                logPost2("move", src#src, dst, result);
                k(result)
            }
            def delete(file, k) = {
                logPre("delete", file);
                let result = FileSystem.delete(file);
                logPost("delete", file, _ -> "done", result);
                k(result)
            }
            def mkDir(d, k) = {
                logPre("mkDir", d);
                let result = FileSystem.mkDir(d);
                logPost("mkDir", d, _ -> "done", result);
                k(result)
            }
            def mkDirs(d, k) = {
                logPre("mkDirs", d);
                let result = FileSystem.mkDirs(d);
                logPost("mkDirs", d, _ -> "done", result);
                k(result)
            }
            def mkTempDir(prefix, k) = {
                logPre("mkTempDir", prefix);
                let result = FileSystem.mkTempDir(prefix);
                logPost("mkTempDir", prefix, path -> path, result);
                k(result)
            }
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, resolving all file paths
    /// relative to the given `baseDir` using Java's `Path.resolve` semantics.
    ///
    /// Absolute paths are passed through unchanged. The `mkTempDir` prefix is not resolved.
    ///
    pub def withBaseDir(baseDir: String, f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let r = p -> Fs.FsLayer.resolve(baseDir, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(FileSystem.exists(r(filename)))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(r(filename)))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(r(filename)))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(r(filename)))
            def isReadable(filename, k)       = k(FileSystem.isReadable(r(filename)))
            def isWritable(filename, k)       = k(FileSystem.isWritable(r(filename)))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(r(filename)))
            def accessTime(filename, k)       = k(FileSystem.accessTime(r(filename)))
            def creationTime(filename, k)     = k(FileSystem.creationTime(r(filename)))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(r(filename)))
            def size(filename, k)             = k(FileSystem.size(r(filename)))
            def read(filename, k)             = k(FileSystem.read(r(filename)))
            def readLines(filename, k)        = k(FileSystem.readLines(r(filename)))
            def readBytes(filename, k)        = k(FileSystem.readBytes(r(filename)))
            def list(filename, k)             = k(FileSystem.list(r(filename)))
            def glob(base, pattern, k)        = k(FileSystem.glob(r(base), pattern))
            def write(data, file, k)          = k(FileSystem.write(data, r(file)))
            def writeLines(data, file, k)     = k(FileSystem.writeLines(data, r(file)))
            def writeBytes(data, file, k)     = k(FileSystem.writeBytes(data, r(file)))
            def append(data, file, k)         = k(FileSystem.append(data, r(file)))
            def appendLines(data, file, k)    = k(FileSystem.appendLines(data, r(file)))
            def appendBytes(data, file, k)    = k(FileSystem.appendBytes(data, r(file)))
            def truncate(file, k)             = k(FileSystem.truncate(r(file)))
            def copyWith(src, dst, opts, k)   = k(FileSystem.copyWith(src = r(src#src), r(dst), opts))
            def moveWith(src, dst, opts, k)   = k(FileSystem.moveWith(src = r(src#src), r(dst), opts))
            def delete(file, k)               = k(FileSystem.delete(r(file)))
            def mkDir(d, k)                   = k(FileSystem.mkDir(r(d)))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(r(d)))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, validating that all file paths
    /// resolve within the given `chrootDir`. Rejects paths that escape the chroot with
    /// a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since it creates directories outside the chroot.
    ///
    pub def withChroot(chrootDir: String, f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.chroot(chrootDir, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(Result.flatMap(p -> FileSystem.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileSystem.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileSystem.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileSystem.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileSystem.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileSystem.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileSystem.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), c(filename)))
            def read(filename, k)             = k(Result.flatMap(p -> FileSystem.read(p), c(filename)))
            def readLines(filename, k)        = k(Result.flatMap(p -> FileSystem.readLines(p), c(filename)))
            def readBytes(filename, k)        = k(Result.flatMap(p -> FileSystem.readBytes(p), c(filename)))
            def list(filename, k)             = k(Result.flatMap(p -> FileSystem.list(p), c(filename)))
            def glob(base, pattern, k)        = k(Result.flatMap(p -> FileSystem.glob(p, pattern), c(base)))
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)         = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted inside a chroot")))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, validating that all file paths
    /// resolve within one of the given `allowedDirs`. Rejects paths outside the
    /// allow list with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since the temp directory location cannot be verified.
    ///
    pub def withAllowList(allowedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.allowList(allowedDirs, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(Result.flatMap(p -> FileSystem.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileSystem.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileSystem.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileSystem.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileSystem.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileSystem.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileSystem.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), c(filename)))
            def read(filename, k)             = k(Result.flatMap(p -> FileSystem.read(p), c(filename)))
            def readLines(filename, k)        = k(Result.flatMap(p -> FileSystem.readLines(p), c(filename)))
            def readBytes(filename, k)        = k(Result.flatMap(p -> FileSystem.readBytes(p), c(filename)))
            def list(filename, k)             = k(Result.flatMap(p -> FileSystem.list(p), c(filename)))
            def glob(base, pattern, k)        = k(Result.flatMap(p -> FileSystem.glob(p, pattern), c(base)))
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)         = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted with an allow list")))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, validating that all file paths
    /// do not resolve within any of the given `deniedDirs`. Rejects paths inside a
    /// denied directory with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is allowed since deny lists are default-allow.
    ///
    pub def withDenyList(deniedDirs: Nel[String], f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let c = p -> Fs.FsLayer.denyList(deniedDirs, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(Result.flatMap(p -> FileSystem.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileSystem.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileSystem.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileSystem.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileSystem.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileSystem.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileSystem.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), c(filename)))
            def read(filename, k)             = k(Result.flatMap(p -> FileSystem.read(p), c(filename)))
            def readLines(filename, k)        = k(Result.flatMap(p -> FileSystem.readLines(p), c(filename)))
            def readBytes(filename, k)        = k(Result.flatMap(p -> FileSystem.readBytes(p), c(filename)))
            def list(filename, k)             = k(Result.flatMap(p -> FileSystem.list(p), c(filename)))
            def glob(base, pattern, k)        = k(Result.flatMap(p -> FileSystem.glob(p, pattern), c(base)))
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, validating that all file paths
    /// match at least one of the given glob `patterns`. Rejects paths that do not match
    /// with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is blocked since the temp directory location cannot be verified.
    ///
    pub def withAllowGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        use IoError.IoError;
        use IoError.ErrorKind;
        let c = p -> Fs.FsLayer.allowGlob(patterns, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(Result.flatMap(p -> FileSystem.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileSystem.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileSystem.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileSystem.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileSystem.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileSystem.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileSystem.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), c(filename)))
            def read(filename, k)             = k(Result.flatMap(p -> FileSystem.read(p), c(filename)))
            def readLines(filename, k)        = k(Result.flatMap(p -> FileSystem.readLines(p), c(filename)))
            def readBytes(filename, k)        = k(Result.flatMap(p -> FileSystem.readBytes(p), c(filename)))
            def list(filename, k)             = k(Result.flatMap(p -> FileSystem.list(p), c(filename)))
            def glob(base, pattern, k)        = k(Result.flatMap(p -> FileSystem.glob(p, pattern), c(base)))
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
            def mkTempDir(_prefix, k)         = k(Err(IoError(ErrorKind.PermissionDenied, "mkTempDir is not permitted with an allow glob")))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, validating that all file paths
    /// do not match any of the given glob `patterns`. Rejects paths that match a denied
    /// pattern with a `PermissionDenied` error.
    ///
    /// The `mkTempDir` operation is allowed since deny globs are default-allow.
    ///
    pub def withDenyGlob(patterns: Nel[String], f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let c = p -> Fs.FsLayer.denyGlob(patterns, p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(Result.flatMap(p -> FileSystem.exists(p), c(filename)))
            def isDirectory(filename, k)      = k(Result.flatMap(p -> FileSystem.isDirectory(p), c(filename)))
            def isRegularFile(filename, k)    = k(Result.flatMap(p -> FileSystem.isRegularFile(p), c(filename)))
            def isSymbolicLink(filename, k)   = k(Result.flatMap(p -> FileSystem.isSymbolicLink(p), c(filename)))
            def isReadable(filename, k)       = k(Result.flatMap(p -> FileSystem.isReadable(p), c(filename)))
            def isWritable(filename, k)       = k(Result.flatMap(p -> FileSystem.isWritable(p), c(filename)))
            def isExecutable(filename, k)     = k(Result.flatMap(p -> FileSystem.isExecutable(p), c(filename)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), c(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), c(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), c(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), c(filename)))
            def read(filename, k)             = k(Result.flatMap(p -> FileSystem.read(p), c(filename)))
            def readLines(filename, k)        = k(Result.flatMap(p -> FileSystem.readLines(p), c(filename)))
            def readBytes(filename, k)        = k(Result.flatMap(p -> FileSystem.readBytes(p), c(filename)))
            def list(filename, k)             = k(Result.flatMap(p -> FileSystem.list(p), c(filename)))
            def glob(base, pattern, k)        = k(Result.flatMap(p -> FileSystem.glob(p, pattern), c(base)))
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.copyWith(src = s, d, opts), c(dst)), c(src#src)))
            def moveWith(src, dst, opts, k)   = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.moveWith(src = s, d, opts), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, resolving all file paths
    /// to their real filesystem targets by following symlinks. This gives opt-in
    /// "follow symlinks" behavior (traditional Unix `stat` vs `lstat` semantics).
    ///
    /// - Bool ops (`exists`, `isDirectory`, etc.): resolve path; return `Ok(false)` for dangling symlinks.
    /// - Value ops (`accessTime`, `size`, etc.): resolve path; propagate all errors.
    /// - Action ops (`read`, `write`, etc.): resolve path with fallback to original (for file creation).
    /// - `isSymbolicLink`: passed through unchanged (always checks the link itself).
    /// - `mkTempDir`: passed through unchanged (no path to resolve).
    ///
    pub def withFollowLinks(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let resolveBool = (path, op) -> match Fs.FsLayer.resolveRealPath(path) {
            case Ok(p)                               => op(p)
            case Err(IoError(ErrorKind.NotFound, _)) => Ok(false)
            case Err(e)                              => Err(e)
        };
        let resolve = p -> Fs.FsLayer.resolveRealPathOrSelf(p);
        run { f() } with handler FileSystem {
            def exists(filename, k)           = k(resolveBool(filename, p -> FileSystem.exists(p)))
            def isDirectory(filename, k)      = k(resolveBool(filename, p -> FileSystem.isDirectory(p)))
            def isRegularFile(filename, k)    = k(resolveBool(filename, p -> FileSystem.isRegularFile(p)))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(resolveBool(filename, p -> FileSystem.isReadable(p)))
            def isWritable(filename, k)       = k(resolveBool(filename, p -> FileSystem.isWritable(p)))
            def isExecutable(filename, k)     = k(resolveBool(filename, p -> FileSystem.isExecutable(p)))
            def accessTime(filename, k)       = k(Result.flatMap(p -> FileSystem.accessTime(p), Fs.FsLayer.resolveRealPath(filename)))
            def creationTime(filename, k)     = k(Result.flatMap(p -> FileSystem.creationTime(p), Fs.FsLayer.resolveRealPath(filename)))
            def modificationTime(filename, k) = k(Result.flatMap(p -> FileSystem.modificationTime(p), Fs.FsLayer.resolveRealPath(filename)))
            def size(filename, k)             = k(Result.flatMap(p -> FileSystem.size(p), Fs.FsLayer.resolveRealPath(filename)))
            def read(filename, k)             = k(FileSystem.read(resolve(filename)))
            def readLines(filename, k)        = k(FileSystem.readLines(resolve(filename)))
            def readBytes(filename, k)        = k(FileSystem.readBytes(resolve(filename)))
            def list(filename, k)             = k(FileSystem.list(resolve(filename)))
            def glob(base, pattern, k)        = k(FileSystem.glob(resolve(base), pattern))
            def write(data, file, k)          = k(FileSystem.write(data, resolve(file)))
            def writeLines(data, file, k)     = k(FileSystem.writeLines(data, resolve(file)))
            def writeBytes(data, file, k)     = k(FileSystem.writeBytes(data, resolve(file)))
            def append(data, file, k)         = k(FileSystem.append(data, resolve(file)))
            def appendLines(data, file, k)    = k(FileSystem.appendLines(data, resolve(file)))
            def appendBytes(data, file, k)    = k(FileSystem.appendBytes(data, resolve(file)))
            def truncate(file, k)             = k(FileSystem.truncate(resolve(file)))
            def copyWith(src, dst, opts, k)   = k(FileSystem.copyWith(src = resolve(src#src), resolve(dst), opts))
            def moveWith(src, dst, opts, k)   = k(FileSystem.moveWith(src = resolve(src#src), resolve(dst), opts))
            def delete(file, k)               = k(FileSystem.delete(resolve(file)))
            def mkDir(d, k)                   = k(FileSystem.mkDir(resolve(d)))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(resolve(d)))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, logging each write
    /// operation via the `Logger` effect at `Debug` level without performing
    /// the actual filesystem modification. All write operations return `Ok(())`;
    /// `mkTempDir` returns `Ok("<dry-run>")`. Read, stat, and test operations
    /// are passed through to the underlying `FileSystem` effect unchanged.
    ///
    pub def withDryRun(f: Unit -> a \ ef): a \ (ef - FileSystem) + {FileSystem, Logger} =
        use Logger.Severity;
        use RichString.{text, bold, cyan, yellow, gray};
        let tag = yellow("[dry-run]");
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))
            // Write ops — dry-run (log + return Ok)
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
    /// Middleware that intercepts the `FileSystem` effect, layering an in-memory
    /// writable store on top of the real filesystem. Writes are captured in memory
    /// (never touching disk), and subsequent reads see the written data. The real
    /// filesystem is never modified. This is useful for safe experimentation,
    /// testing, and sandboxing.
    ///
    pub def withMemoryOverlay(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        region rc {
            let o = Fs.MemoryOverlay.fresh(rc);
            run { f() } with handler FileSystem {
                // ── Stat/test ops — delegate on None ───────────────
                def exists(f1, k) = k(match Fs.MemoryOverlay.tryExists(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.exists(f1)
                })
                def isDirectory(f1, k) = k(match Fs.MemoryOverlay.tryIsDirectory(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isDirectory(f1)
                })
                def isRegularFile(f1, k) = k(match Fs.MemoryOverlay.tryIsRegularFile(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isRegularFile(f1)
                })
                def isSymbolicLink(f1, k) = k(match Fs.MemoryOverlay.tryIsSymbolicLink(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isSymbolicLink(f1)
                })
                def isReadable(f1, k) = k(match Fs.MemoryOverlay.tryIsReadable(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isReadable(f1)
                })
                def isWritable(f1, k) = k(match Fs.MemoryOverlay.tryIsWritable(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isWritable(f1)
                })
                def isExecutable(f1, k) = k(match Fs.MemoryOverlay.tryIsExecutable(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.isExecutable(f1)
                })
                def accessTime(f1, k) = k(match Fs.MemoryOverlay.tryTime(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.accessTime(f1)
                })
                def creationTime(f1, k) = k(match Fs.MemoryOverlay.tryTime(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.creationTime(f1)
                })
                def modificationTime(f1, k) = k(match Fs.MemoryOverlay.tryTime(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.modificationTime(f1)
                })
                def size(f1, k) = k(match Fs.MemoryOverlay.trySize(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.size(f1)
                })
                def read(f1, k) = k(match Fs.MemoryOverlay.tryRead(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.read(f1)
                })
                def readLines(f1, k) = k(match Fs.MemoryOverlay.tryReadLines(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.readLines(f1)
                })
                def readBytes(f1, k) = k(match Fs.MemoryOverlay.tryReadBytes(f1, o) {
                    case Some(r) => r
                    case None    => FileSystem.readBytes(f1)
                })

                // ── List/glob — pass lower result to Overlay ────────
                def list(f1, k) = k(Fs.MemoryOverlay.list(f1, FileSystem.list(f1), o))
                def glob(base, pattern, k) = k(Fs.MemoryOverlay.glob(base, pattern, FileSystem.glob(base, pattern), o))

                // ── Write ops — direct forwarding ──────────────────
                def write(data, file, k)      = k(Fs.MemoryOverlay.write(file, data#str, o))
                def writeLines(data, file, k) = k(Fs.MemoryOverlay.writeLines(file, data#lines, o))
                def writeBytes(data, file, k) = k(Fs.MemoryOverlay.writeBytes(file, data, o))
                def truncate(file, k)         = k(Fs.MemoryOverlay.truncate(file, o))
                def delete(file, k)           = k(Fs.MemoryOverlay.delete(file, o))
                def mkDir(d, k)               = k(Fs.MemoryOverlay.mkDir(d, o))
                def mkDirs(d, k)              = k(Fs.MemoryOverlay.mkDirs(d, o))
                def mkTempDir(prefix, k)      = k(Fs.MemoryOverlay.mkTempDir(prefix, o))

                // ── Append ops — fetch lower content if needed ─────
                def append(data, file, k) = {
                    let lower = if (Fs.MemoryOverlay.needsLower(file, o)) Result.toOption(FileSystem.read(file)) else None;
                    k(Fs.MemoryOverlay.append(file, data#str, lower, o))
                }
                def appendLines(data, file, k) = {
                    let lower = if (Fs.MemoryOverlay.needsLower(file, o)) Result.toOption(FileSystem.read(file)) else None;
                    k(Fs.MemoryOverlay.appendLines(file, data#lines, lower, o))
                }
                def appendBytes(data, file, k) = {
                    let lower = if (Fs.MemoryOverlay.needsLower(file, o)) Result.toOption(FileSystem.readBytes(file)) else None;
                    k(Fs.MemoryOverlay.appendBytes(file, data, lower, o))
                }

                // ── Copy — fetch lower content if needed ────────────
                def copyWith(src, dst, _opts, k) = {
                    let lower = if (Fs.MemoryOverlay.needsLower(src#src, o))
                        Result.toOption(FileSystem.readBytes(src#src))
                    else None;
                    k(Fs.MemoryOverlay.copy(src#src, dst, lower, o))
                }

                // ── Move — fetch lower content if needed ────────────
                def moveWith(src, dst, _opts, k) = {
                    let lower = if (Fs.MemoryOverlay.needsLower(src#src, o))
                        Result.toOption(FileSystem.readBytes(src#src))
                    else None;
                    k(Fs.MemoryOverlay.move(src#src, dst, lower, o))
                }
            }
        }

    ///
    /// Terminal handler that intercepts the `FileSystem` effect with a fully
    /// in-memory filesystem. The filesystem starts empty; reads of non-written
    /// files return `NotFound`. No real filesystem access occurs. This is
    /// useful for testing, sandboxing, and deterministic simulations.
    ///
    pub def withInMemoryFS(f: Unit -> a \ ef): a \ (ef - FileSystem) + Clock =
        region rc {
            let o = Fs.InMemoryFs.fresh(rc);
            run { f() } with handler FileSystem {
                def exists(f1, k)               = k(Fs.InMemoryFs.exists(f1, o))
                def isDirectory(f1, k)          = k(Fs.InMemoryFs.isDirectory(f1, o))
                def isRegularFile(f1, k)        = k(Fs.InMemoryFs.isRegularFile(f1, o))
                def isSymbolicLink(f1, k)       = k(Fs.InMemoryFs.isSymbolicLink(f1, o))
                def isReadable(f1, k)           = k(Fs.InMemoryFs.isReadable(f1, o))
                def isWritable(f1, k)           = k(Fs.InMemoryFs.isWritable(f1, o))
                def isExecutable(f1, k)         = k(Fs.InMemoryFs.isExecutable(f1, o))
                def accessTime(f1, k)           = k(Fs.InMemoryFs.accessTime(f1, o))
                def creationTime(f1, k)         = k(Fs.InMemoryFs.creationTime(f1, o))
                def modificationTime(f1, k)     = k(Fs.InMemoryFs.modificationTime(f1, o))
                def size(f1, k)                 = k(Fs.InMemoryFs.size(f1, o))
                def read(f1, k)                 = k(Fs.InMemoryFs.read(f1, o))
                def readLines(f1, k)            = k(Fs.InMemoryFs.readLines(f1, o))
                def readBytes(f1, k)            = k(Fs.InMemoryFs.readBytes(f1, o))
                def list(f1, k)                 = k(Fs.InMemoryFs.list(f1, o))
                def glob(base, pattern, k)      = k(Fs.InMemoryFs.glob(base, pattern, o))
                def write(data, file, k)        = k(Fs.InMemoryFs.write(file, data#str, o))
                def writeLines(data, file, k)   = k(Fs.InMemoryFs.writeLines(file, data#lines, o))
                def writeBytes(data, file, k)   = k(Fs.InMemoryFs.writeBytes(file, data, o))
                def append(data, file, k)       = k(Fs.InMemoryFs.append(file, data#str, o))
                def appendLines(data, file, k)  = k(Fs.InMemoryFs.appendLines(file, data#lines, o))
                def appendBytes(data, file, k)  = k(Fs.InMemoryFs.appendBytes(file, data, o))
                def truncate(file, k)           = k(Fs.InMemoryFs.truncate(file, o))
                def copyWith(src, dst, opts, k) = k(Fs.InMemoryFs.copy(src#src, dst, opts, o))
                def moveWith(src, dst, opts, k) = k(Fs.InMemoryFs.move(src#src, dst, opts, o))
                def delete(file, k)             = k(Fs.InMemoryFs.delete(file, o))
                def mkDir(d, k)                 = k(Fs.InMemoryFs.mkDir(d, o))
                def mkDirs(d, k)                = k(Fs.InMemoryFs.mkDirs(d, o))
                def mkTempDir(prefix, k)        = k(Fs.InMemoryFs.mkTempDir(prefix, o))
            }
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, blocking all write
    /// operations with a `PermissionDenied` error while passing read, stat,
    /// and test operations through to the underlying `FileSystem` effect
    /// unchanged. This is useful for sandboxing code that should only observe
    /// the filesystem.
    ///
    pub def withReadOnly(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        use IoError.IoError;
        use IoError.ErrorKind;
        let denied = op -> Err(IoError(ErrorKind.PermissionDenied, "read-only: ${op} is not permitted"));
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))
            // Write ops — blocked
            def write(_data, _file, k)          = k(denied("write"))
            def writeLines(_data, _file, k)     = k(denied("writeLines"))
            def writeBytes(_data, _file, k)     = k(denied("writeBytes"))
            def append(_data, _file, k)         = k(denied("append"))
            def appendLines(_data, _file, k)    = k(denied("appendLines"))
            def appendBytes(_data, _file, k)    = k(denied("appendBytes"))
            def truncate(_file, k)              = k(denied("truncate"))
            def copyWith(_src, _dst, _opts, k)  = k(denied("copy"))
            def moveWith(_src, _dst, _opts, k)  = k(denied("move"))
            def delete(_file, k)                = k(denied("delete"))
            def mkDir(_d, k)                    = k(denied("mkDir"))
            def mkDirs(_d, k)                   = k(denied("mkDirs"))
            def mkTempDir(_prefix, k)           = k(Err(IoError(ErrorKind.PermissionDenied, "read-only: mkTempDir is not permitted")))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, making `write`,
    /// `writeLines`, and `writeBytes` atomic. Data is first written to a
    /// temporary file in the same directory, then atomically moved into place.
    /// On failure the temporary file is cleaned up.
    ///
    /// All other operations (read, stat, test, list, glob, append, truncate,
    /// copy, move, delete, mkdir) are passed through unchanged.
    ///
    pub def withAtomicWrite(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let mv  = (tmp, dst) -> FileSystem.moveWith(src = tmp, dst, Set#{Fs.MoveOption.AtomicMove, Fs.MoveOption.ReplaceExisting});
        let del = tmp -> FileSystem.delete(tmp);
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // Overwrite ops — atomic write-to-temp then rename
            def write(data, file, k)          = k(Fs.FsLayer.atomicWrite(file, tmp -> FileSystem.write(data, tmp), mv, del))
            def writeLines(data, file, k)     = k(Fs.FsLayer.atomicWrite(file, tmp -> FileSystem.writeLines(data, tmp), mv, del))
            def writeBytes(data, file, k)     = k(Fs.FsLayer.atomicWrite(file, tmp -> FileSystem.writeBytes(data, tmp), mv, del))

            // Other write ops — pass through
            def append(data, file, k)         = k(FileSystem.append(data, file))
            def appendLines(data, file, k)    = k(FileSystem.appendLines(data, file))
            def appendBytes(data, file, k)    = k(FileSystem.appendBytes(data, file))
            def truncate(file, k)             = k(FileSystem.truncate(file))
            def copyWith(src, dst, opts, k)   = k(FileSystem.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k)   = k(FileSystem.moveWith(src, dst, opts))
            def delete(file, k)               = k(FileSystem.delete(file))
            def mkDir(d, k)                   = k(FileSystem.mkDir(d))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, creating a backup
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
    /// Read, stat, test, list, glob, append, mkdir, and mkTempDir operations
    /// are passed through unchanged.
    ///
    pub def withBackup(suffix: String, f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let doCopy = (src, dst) -> FileSystem.copyWith(src = src, dst, Set#{Fs.CopyOption.ReplaceExisting});
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // Destructive ops — backup then proceed
            def write(data, file, k)          = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileSystem.write(data, file)))
            def writeLines(data, file, k)     = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileSystem.writeLines(data, file)))
            def writeBytes(data, file, k)     = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileSystem.writeBytes(data, file)))
            def truncate(file, k)             = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileSystem.truncate(file)))
            def delete(file, k)               = k(Fs.FsLayer.backup(file, suffix, doCopy, () -> FileSystem.delete(file)))
            def copyWith(src, dst, opts, k)   = k(Fs.FsLayer.backup(dst, suffix, doCopy, () -> FileSystem.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k)   = k(Fs.FsLayer.backup(dst, suffix, doCopy, () -> FileSystem.moveWith(src, dst, opts)))

            // Non-destructive write ops — pass through
            def append(data, file, k)         = k(FileSystem.append(data, file))
            def appendLines(data, file, k)    = k(FileSystem.appendLines(data, file))
            def appendBytes(data, file, k)    = k(FileSystem.appendBytes(data, file))
            def mkDir(d, k)                   = k(FileSystem.mkDir(d))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, automatically creating
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
    /// Read, stat, test, list, glob, `truncate`, `delete`, `mkDir`, `mkDirs`,
    /// and `mkTempDir` are passed through unchanged.
    ///
    pub def withMkParentDirs(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let mk = d -> FileSystem.mkDirs(d);
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // Write/append/copy/move — ensure parent dirs exist
            def write(data, file, k)          = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.write(data, file)))
            def writeLines(data, file, k)     = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.writeLines(data, file)))
            def writeBytes(data, file, k)     = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.writeBytes(data, file)))
            def append(data, file, k)         = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.append(data, file)))
            def appendLines(data, file, k)    = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.appendLines(data, file)))
            def appendBytes(data, file, k)    = k(Fs.FsLayer.ensureParentDirs(file, mk, () -> FileSystem.appendBytes(data, file)))
            def copyWith(src, dst, opts, k)   = k(Fs.FsLayer.ensureParentDirs(dst, mk, () -> FileSystem.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k)   = k(Fs.FsLayer.ensureParentDirs(dst, mk, () -> FileSystem.moveWith(src, dst, opts)))

            // Other write ops — pass through
            def truncate(file, k)             = k(FileSystem.truncate(file))
            def delete(file, k)               = k(FileSystem.delete(file))
            def mkDir(d, k)                   = k(FileSystem.mkDir(d))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, rejecting read, write,
    /// and append operations where the data exceeds `maxBytes`. For reads and
    /// `copyWith`, the file size is checked via `FileSystem.size`.
    ///
    /// String-based operations estimate byte count as `charCount * 4` (the
    /// maximum bytes per character in UTF-8). Use byte-based operations for
    /// exact enforcement.
    ///
    /// Operations that don't transfer data (`truncate`, `moveWith`, `delete`,
    /// `mkDir`, `mkDirs`, `mkTempDir`) and metadata operations are passed
    /// through unchanged.
    ///
    pub def withTransferLimit(maxSize: Size, f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let check = sz -> Fs.FsLayer.checkTransferLimit(maxSize, sz);
        let estStr = s -> Fs.FsLayer.estimateStringBytes(s);
        run { f() } with handler FileSystem {
            // Stat/test/metadata ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // Read ops — check file size before reading
            def read(filename, k)             = k(Result.flatMap(_ -> FileSystem.read(filename), Result.flatMap(check, FileSystem.size(filename))))
            def readLines(filename, k)        = k(Result.flatMap(_ -> FileSystem.readLines(filename), Result.flatMap(check, FileSystem.size(filename))))
            def readBytes(filename, k)        = k(Result.flatMap(_ -> FileSystem.readBytes(filename), Result.flatMap(check, FileSystem.size(filename))))

            // Write ops — check payload size
            def write(data, file, k)          = k(Result.flatMap(_ -> FileSystem.write(data, file), check(estStr(data#str))))
            def writeLines(data, file, k)     = k(Result.flatMap(_ -> FileSystem.writeLines(data, file), check(Fs.FsLayer.estimateStringListBytes(data#lines))))
            def writeBytes(data, file, k)     = k(Result.flatMap(_ -> FileSystem.writeBytes(data, file), check(bytes(Int32.toInt64(Vector.length(data))))))
            def append(data, file, k)         = k(Result.flatMap(_ -> FileSystem.append(data, file), check(estStr(data#str))))
            def appendLines(data, file, k)    = k(Result.flatMap(_ -> FileSystem.appendLines(data, file), check(Fs.FsLayer.estimateStringListBytes(data#lines))))
            def appendBytes(data, file, k)    = k(Result.flatMap(_ -> FileSystem.appendBytes(data, file), check(bytes(Int32.toInt64(Vector.length(data))))))
            def copyWith(src, dst, opts, k)   = k(Result.flatMap(sz -> Result.flatMap(_ -> FileSystem.copyWith(src, dst, opts), check(sz)), FileSystem.size(src#src)))

            // Non-data-transfer ops — pass through
            def truncate(file, k)             = k(FileSystem.truncate(file))
            def moveWith(src, dst, opts, k)   = k(FileSystem.moveWith(src, dst, opts))
            def delete(file, k)               = k(FileSystem.delete(file))
            def mkDir(d, k)                   = k(FileSystem.mkDir(d))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, providing optimistic
    /// concurrency control. When a file is read (`read`, `readLines`,
    /// `readBytes`), its modification time is snapshotted. When a file is
    /// subsequently written, the current modification time is compared against
    /// the snapshot. If the file was modified externally, the operation returns
    /// `Err(Conflict(...))`.
    ///
    /// After a successful write, the modification time is re-snapshotted to
    /// prevent false positives on subsequent writes to the same file.
    ///
    /// Files that were never read have no snapshot and are written without
    /// conflict checks.
    ///
    /// The following operations are passed through unchanged:
    /// `exists`, `isDirectory`, `isRegularFile`, `isSymbolicLink`,
    /// `isReadable`, `isWritable`, `isExecutable`, `accessTime`,
    /// `creationTime`, `modificationTime`, `size`, `list`, `glob`,
    /// `mkDir`, `mkDirs`, and `mkTempDir`.
    ///
    pub def withConflictCheck(f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        region rc {
            let snapshots: MutMap[String, Int64, rc] = MutMap.empty(rc);
            let getModTime = file -> FileSystem.modificationTime(file);
            let snap = file -> Fs.FsLayer.snapshotModTime(file, snapshots, getModTime);
            let check = file -> Fs.FsLayer.conflictCheck(file, snapshots, getModTime);
            let checkAndResnap = (file, doWrite) ->
                forM (_ <- check(file); v <- doWrite())
                    yield { snap(file); v };
            run { f() } with handler FileSystem {
                // ── Stat/test/perm/time/size — pass through ──
                def exists(filename, k)           = k(FileSystem.exists(filename))
                def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
                def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
                def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
                def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
                def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
                def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
                def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
                def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
                def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
                def size(filename, k)             = k(FileSystem.size(filename))

                // ── List/glob — pass through ──
                def list(filename, k)             = k(FileSystem.list(filename))
                def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

                // ── Read ops — snapshot mod time after success ──
                def read(filename, k) = {
                    let result = FileSystem.read(filename);
                    match result {
                        case Ok(_) => snap(filename)
                        case _     => ()
                    };
                    k(result)
                }
                def readLines(filename, k) = {
                    let result = FileSystem.readLines(filename);
                    match result {
                        case Ok(_) => snap(filename)
                        case _     => ()
                    };
                    k(result)
                }
                def readBytes(filename, k) = {
                    let result = FileSystem.readBytes(filename);
                    match result {
                        case Ok(_) => snap(filename)
                        case _     => ()
                    };
                    k(result)
                }

                // ── Write ops — conflict check before, re-snapshot after ──
                def write(data, file, k)          = k(checkAndResnap(file, () -> FileSystem.write(data, file)))
                def writeLines(data, file, k)     = k(checkAndResnap(file, () -> FileSystem.writeLines(data, file)))
                def writeBytes(data, file, k)     = k(checkAndResnap(file, () -> FileSystem.writeBytes(data, file)))
                def append(data, file, k)         = k(checkAndResnap(file, () -> FileSystem.append(data, file)))
                def appendLines(data, file, k)    = k(checkAndResnap(file, () -> FileSystem.appendLines(data, file)))
                def appendBytes(data, file, k)    = k(checkAndResnap(file, () -> FileSystem.appendBytes(data, file)))
                def truncate(file, k)             = k(checkAndResnap(file, () -> FileSystem.truncate(file)))
                def delete(file, k) = {
                    match check(file) {
                        case Err(e) => k(Err(e))
                        case Ok(_)  =>
                            let result = FileSystem.delete(file);
                            match result {
                                case Ok(_) => MutMap.remove(file, snapshots)
                                case _     => ()
                            };
                            k(result)
                    }
                }

                // ── Copy/move — conflict check on destination ──
                def copyWith(src, dst, opts, k)   = k(checkAndResnap(dst, () -> FileSystem.copyWith(src, dst, opts)))
                def moveWith(src, dst, opts, k)   = k(checkAndResnap(dst, () -> FileSystem.moveWith(src, dst, opts)))

                // ── Directory ops — pass through ──
                def mkDir(d, k)                   = k(FileSystem.mkDir(d))
                def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
                def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
            }
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, automatically rotating
    /// files when they reach a size threshold before append operations.
    ///
    /// Before `append`, `appendLines`, and `appendBytes`, the target file's size
    /// is checked. If it meets or exceeds `maxSize`, existing rotated files are
    /// shifted (`file.1` → `file.2`, etc.), the oldest (`file.{maxFiles}`) is
    /// deleted, and the current file is moved to `file.1`. The append then
    /// writes to the now-absent original path.
    ///
    /// All other operations are passed through unchanged.
    ///
    pub def withSizeRotation(maxSize: Size, maxFiles: Int32, f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let doSize   = file -> FileSystem.size(file);
        let doMove   = (src, dst) -> FileSystem.moveWith(src = src, dst, Set#{Fs.MoveOption.ReplaceExisting});
        let doDelete = file -> FileSystem.delete(file);
        run { f() } with handler FileSystem {
            // Read/stat/test ops — pass through
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def read(filename, k)             = k(FileSystem.read(filename))
            def readLines(filename, k)        = k(FileSystem.readLines(filename))
            def readBytes(filename, k)        = k(FileSystem.readBytes(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // Append ops — rotate if needed before appending
            def append(data, file, k)      = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileSystem.append(data, file)))
            def appendLines(data, file, k) = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileSystem.appendLines(data, file)))
            def appendBytes(data, file, k) = k(Fs.FsLayer.rotateIfNeeded(file, maxSize, maxFiles, doSize, doMove, doDelete, () -> FileSystem.appendBytes(data, file)))

            // Other write ops — pass through
            def write(data, file, k)          = k(FileSystem.write(data, file))
            def writeLines(data, file, k)     = k(FileSystem.writeLines(data, file))
            def writeBytes(data, file, k)     = k(FileSystem.writeBytes(data, file))
            def truncate(file, k)             = k(FileSystem.truncate(file))
            def copyWith(src, dst, opts, k)   = k(FileSystem.copyWith(src, dst, opts))
            def moveWith(src, dst, opts, k)   = k(FileSystem.moveWith(src, dst, opts))
            def delete(file, k)               = k(FileSystem.delete(file))
            def mkDir(d, k)                   = k(FileSystem.mkDir(d))
            def mkDirs(d, k)                  = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)          = k(FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileSystem` effect, providing data integrity
    /// via checksum sidecar files. Write operations produce checksum sidecars;
    /// read operations verify against them.
    ///
    /// `hash` is a pure function from file bytes to digest bytes.
    /// `sidecar` is the file suffix (e.g., `".sha256"`).
    ///
    /// After write, append, truncate, and copy operations, the file is re-read
    /// and a sidecar containing the hex digest is written. After move, the
    /// sidecar is moved. After delete, the sidecar is removed.
    ///
    /// On read, if a sidecar exists, the file's contents are hashed and compared
    /// against the stored checksum. If no sidecar exists, the read proceeds
    /// normally.
    ///
    /// Stat, test, permission, time, size, list, and glob operations are
    /// passed through unchanged.
    ///
    pub def withChecksum(hash: Vector[Int8] -> Vector[Int8], sidecar: String,
                         f: Unit -> a \ ef): a \ (ef - FileSystem) + FileSystem =
        let computeHex = bytes -> Fs.FsLayer.bytesToHex(hash(bytes));
        let doReadBytes = file -> FileSystem.readBytes(file);
        let doReadSidecar = file -> FileSystem.read(file);
        let updateSidecar = file -> Fs.FsLayer.updateSidecar(file, sidecar, computeHex, doReadBytes, (str, f1) -> FileSystem.write(str = str, f1));
        let afterWrite = (file, doWrite) -> Fs.FsLayer.checksumAfterWrite(file, doWrite, updateSidecar);
        let doMoveSidecar = (s, d) -> FileSystem.moveWith(src = s, d, Set#{Fs.MoveOption.ReplaceExisting});
        let doDeleteSidecar = file -> FileSystem.delete(file);
        run { f() } with handler FileSystem {
            // ── Stat/test/perm/time/size — pass through ──
            def exists(filename, k)           = k(FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(FileSystem.modificationTime(filename))
            def size(filename, k)             = k(FileSystem.size(filename))
            def list(filename, k)             = k(FileSystem.list(filename))
            def glob(base, pattern, k)        = k(FileSystem.glob(base, pattern))

            // ── Read ops — verify checksum after read ──
            def read(filename, k)      = k(Fs.FsLayer.checksumOnRead(filename, sidecar, computeHex, FileSystem.read(filename), doReadBytes, doReadSidecar))
            def readLines(filename, k) = k(Fs.FsLayer.checksumOnRead(filename, sidecar, computeHex, FileSystem.readLines(filename), doReadBytes, doReadSidecar))
            def readBytes(filename, k) = k(Fs.FsLayer.checksumOnReadBytes(filename, sidecar, computeHex, FileSystem.readBytes(filename), doReadSidecar))

            // ── Write ops — update checksum sidecar after write ──
            def write(data, file, k)        = k(afterWrite(file, () -> FileSystem.write(data, file)))
            def writeLines(data, file, k)   = k(afterWrite(file, () -> FileSystem.writeLines(data, file)))
            def writeBytes(data, file, k)   = k(afterWrite(file, () -> FileSystem.writeBytes(data, file)))
            def append(data, file, k)       = k(afterWrite(file, () -> FileSystem.append(data, file)))
            def appendLines(data, file, k)  = k(afterWrite(file, () -> FileSystem.appendLines(data, file)))
            def appendBytes(data, file, k)  = k(afterWrite(file, () -> FileSystem.appendBytes(data, file)))
            def truncate(file, k)           = k(afterWrite(file, () -> FileSystem.truncate(file)))
            def copyWith(src, dst, opts, k) = k(afterWrite(dst, () -> FileSystem.copyWith(src, dst, opts)))
            def moveWith(src, dst, opts, k) = k(Fs.FsLayer.checksumAfterMove(src#src, dst, sidecar, FileSystem.moveWith(src, dst, opts), doMoveSidecar))
            def delete(file, k)             = k(Fs.FsLayer.checksumAfterDelete(file, sidecar, FileSystem.delete(file), doDeleteSidecar))

            // ── Directory ops — pass through ──
            def mkDir(d, k)       = k(FileSystem.mkDir(d))
            def mkDirs(d, k)      = k(FileSystem.mkDirs(d))
            def mkTempDir(prefix, k) = k(FileSystem.mkTempDir(prefix))
        }

}
