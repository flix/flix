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
    use Fs.FsLayer.{logPre, logPost}
    use IoError.IoError

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
            def write(data, file, k)          = k(Fs.FsLayer.write(data, file))
            def writeLines(data, file, k)     = k(Fs.FsLayer.writeLines(data, file))
            def writeBytes(data, file, k)     = k(Fs.FsLayer.writeBytes(data, file))
            def append(data, file, k)         = k(Fs.FsLayer.append(data, file))
            def appendLines(data, file, k)    = k(Fs.FsLayer.appendLines(data, file))
            def appendBytes(data, file, k)    = k(Fs.FsLayer.appendBytes(data, file))
            def truncate(file, k)             = k(Fs.FsLayer.truncate(file))
            def move(src, dst, k)             = k(Fs.FsLayer.move(src, dst))
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
                logPost("size", filename, s -> "${s} bytes", result);
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
            def move(src, dst, k) = {
                logPre("move", src#src);
                let result = FileSystem.move(src, dst);
                logPost("move", src#src, _ -> dst, result);
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
            def write(data, file, k)          = k(FileSystem.write(data, r(file)))
            def writeLines(data, file, k)     = k(FileSystem.writeLines(data, r(file)))
            def writeBytes(data, file, k)     = k(FileSystem.writeBytes(data, r(file)))
            def append(data, file, k)         = k(FileSystem.append(data, r(file)))
            def appendLines(data, file, k)    = k(FileSystem.appendLines(data, r(file)))
            def appendBytes(data, file, k)    = k(FileSystem.appendBytes(data, r(file)))
            def truncate(file, k)             = k(FileSystem.truncate(r(file)))
            def move(src, dst, k)             = k(FileSystem.move(src = r(src#src), r(dst)))
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
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def move(src, dst, k)             = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.move(src = s, d), c(dst)), c(src#src)))
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
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def move(src, dst, k)             = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.move(src = s, d), c(dst)), c(src#src)))
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
            def write(data, file, k)          = k(Result.flatMap(p -> FileSystem.write(data, p), c(file)))
            def writeLines(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeLines(data, p), c(file)))
            def writeBytes(data, file, k)     = k(Result.flatMap(p -> FileSystem.writeBytes(data, p), c(file)))
            def append(data, file, k)         = k(Result.flatMap(p -> FileSystem.append(data, p), c(file)))
            def appendLines(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendLines(data, p), c(file)))
            def appendBytes(data, file, k)    = k(Result.flatMap(p -> FileSystem.appendBytes(data, p), c(file)))
            def truncate(file, k)             = k(Result.flatMap(p -> FileSystem.truncate(p), c(file)))
            def move(src, dst, k)             = k(Result.flatMap(s -> Result.flatMap(d -> FileSystem.move(src = s, d), c(dst)), c(src#src)))
            def delete(file, k)               = k(Result.flatMap(p -> FileSystem.delete(p), c(file)))
            def mkDir(d, k)                   = k(Result.flatMap(p -> FileSystem.mkDir(p), c(d)))
            def mkDirs(d, k)                  = k(Result.flatMap(p -> FileSystem.mkDirs(p), c(d)))
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
            def move(src, dst, k) = {
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

}
