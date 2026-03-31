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

mod Fs.FsLayer {

    import java.io.File
    import java.io.IOException
    import java.io.UncheckedIOException
    import java.lang.Class
    import java.lang.IllegalArgumentException
    import java.lang.{Object => JObject}
    import java.lang.UnsupportedOperationException
    import java.nio.charset.CharacterCodingException
    import java.nio.charset.StandardCharsets
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.attribute.FileAttribute
    import java.nio.file.FileAlreadyExistsException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.NotDirectoryException
    import java.nio.file.OpenOption
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.CopyOption
    import java.nio.file.StandardCopyOption
    import java.nio.file.FileSystems
    import java.nio.file.FileVisitOption
    import java.nio.file.NoSuchFileException
    import java.nio.file.PathMatcher
    import java.nio.file.StandardOpenOption
    import java.util.stream.BaseStream
    import java.util.HexFormat
    import java.util.UUID
    import java.util.stream.Stream

    use Fs.Size
    use Fs.Size.bytes
    use Fs.Size.toBytes
    use IoError.IoError
    use IoError.ErrorKind
    use RichString.{text, bold, cyan, green, red, gray}

    // ─── Symlink safety ────────────────────────────────────────────────

    ///
    /// Returns a singleton vector containing `NOFOLLOW_LINKS`.
    ///
    /// Used by file-test and attribute operations so they inspect the
    /// symbolic link itself rather than silently dereferencing to its target.
    ///
    def noFollow(): Vector[LinkOption] =
        Vector#{LinkOption.NOFOLLOW_LINKS}

    // ─── Logging helpers ───────────────────────────────────────────────

    ///
    /// Logs a pre-operation message at `Debug` level.
    ///
    pub def logPre(op: String, path: String): Unit \ Logger =
        Logger.log(Severity.Debug,
            gray("-->") + text(" ") + bold(op) + text(" ") + cyan(path))

    ///
    /// Logs a pre-operation message for two-path operations (copy/move) at `Debug` level.
    ///
    pub def logPre2(op: String, src: String, dst: String): Unit \ Logger =
        Logger.log(Severity.Debug,
            gray("-->") + text(" ") + bold(op) + text(" ") + cyan(src) +
            gray(" -> ") + cyan(dst))

    ///
    /// Logs a post-operation message: `Debug` on success, `Warn` on error.
    ///
    pub def logPost(op: String, path: String, describe: a -> String, result: Result[IoError, a]): Unit \ Logger =
        match result {
            case Ok(v)    =>
                Logger.log(Severity.Debug,
                    gray("<--") + text(" ") + green("OK") + text(" ") +
                    bold(op) + text(" ") + cyan(path) +
                    gray(" (${describe(v)})"))
            case Err(err) =>
                Logger.log(Severity.Warn,
                    gray("<--") + text(" ") + red("ERR") + text(" ") +
                    bold(op) + text(" ") + cyan(path) +
                    text(" ") + red("${err}"))
        }

    ///
    /// Logs a post-operation message for two-path operations (copy/move).
    ///
    pub def logPost2(op: String, src: String, dst: String, result: Result[IoError, Unit]): Unit \ Logger =
        match result {
            case Ok(_)    =>
                Logger.log(Severity.Debug,
                    gray("<--") + text(" ") + green("OK") + text(" ") +
                    bold(op) + text(" ") + cyan(src) +
                    gray(" -> ") + cyan(dst))
            case Err(err) =>
                Logger.log(Severity.Warn,
                    gray("<--") + text(" ") + red("ERR") + text(" ") +
                    bold(op) + text(" ") + cyan(src) +
                    gray(" -> ") + cyan(dst) +
                    text(" ") + red("${err}"))
        }

    // ─── File-test operations ──────────────────────────────────────────

    pub def exists(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.exists(Paths.get(f), noFollow()))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isDirectory(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isDirectory(Paths.get(f), noFollow()))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isRegularFile(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isRegularFile(Paths.get(f), noFollow()))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isSymbolicLink(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isSymbolicLink(Paths.get(f)))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isReadable(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isReadable(Paths.get(f)))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isWritable(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isWritable(Paths.get(f)))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isExecutable(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isExecutable(Paths.get(f)))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    // ─── File-time operations ──────────────────────────────────────────

    pub def accessTime(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(fileAttributes(Paths.get(f)).lastAccessTime().toMillis())
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def creationTime(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(fileAttributes(Paths.get(f)).creationTime().toMillis())
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def modificationTime(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(fileAttributes(Paths.get(f)).lastModifiedTime().toMillis())
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── File-stat operations ──────────────────────────────────────────

    pub def size(f: String): Result[IoError, Size] \ IO =
        try {
            Ok(bytes(fileAttributes(Paths.get(f)).size()))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Read operations ───────────────────────────────────────────────

    pub def read(f: String): Result[IoError, String] \ IO =
        try {
            Ok(String.fromBytes(Array.toVector(Files.readAllBytes(Paths.get(f)))))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def readLines(f: String): Result[IoError, List[String]] \ IO =
        try {
            Ok(ToFlix.toFlix(Files.readAllLines(Paths.get(f))))
        } catch {
            case ex: InvalidPathException      => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException       => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: CharacterCodingException  => Err(IoError(ErrorKind.InvalidData, ex.getMessage()))
            case ex: IOException               => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def readBytes(f: String): Result[IoError, Vector[Int8]] \ IO =
        try {
            Ok(Array.toVector(Files.readAllBytes(Paths.get(f))))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Directory listing ─────────────────────────────────────────────

    pub def list(f: String): Result[IoError, List[String]] \ IO =
        let file = new File(f);
        let arr = file.list();
        if (Object.isNull(arr))
            if (not file.exists())
                Err(IoError(ErrorKind.NotFound, f))
            else if (not file.isDirectory())
                Err(IoError(ErrorKind.NotDirectory, f))
            else
                Err(IoError(ErrorKind.Other, "Unable to list: ${f}"))
        else
            Ok(Array.toList(arr))

    // ─── Glob operation ─────────────────────────────────────────────────

    pub def glob(base: String, pattern: String): Result[IoError, List[String]] \ IO =
        try {
            let basePath = Paths.get(base);
            let matcher = FileSystems.getDefault().getPathMatcher("glob:${pattern}");
            let stream = Files.walk(basePath);
            let bs: BaseStream[Path, Stream[Path]] = checked_cast(stream);
            let jiter = bs.iterator();
            def loop(acc) = {
                if (jiter.hasNext())
                    let path = jiter.next();
                    let rel = basePath.relativize(path);
                    if (matcher.matches(rel))
                        loop(path.toString() :: acc)
                    else
                        loop(acc)
                else
                    acc
            };
            let result = List.reverse(loop(Nil));
            bs.close();
            Ok(result)
        } catch {
            case ex: InvalidPathException     => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IllegalArgumentException => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
            case ex: UncheckedIOException     => Err(IoError(ErrorKind.Other, ex.getMessage()))
            case ex: IOException              => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Write operations ──────────────────────────────────────────────

    pub def write(data: {str = String}, f: String): Result[IoError, Unit] \ IO =
        try {
            let bytes = data#str.getBytes(StandardCharsets.UTF_8);
            Files.write(Paths.get(f), bytes);
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit] \ IO =
        try {
            let bf = Files.newBufferedWriter(Paths.get(f));
            List.forEach(line -> {
                bf.write(line, 0, String.length(line));
                bf.newLine()
            }, data#lines);
            bf.close();
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit] \ IO =
        try {
            region rc {
                let bytes = Vector.toArray(rc, data);
                Files.write(Paths.get(f), bytes);
                Ok(())
            }
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Append operations ─────────────────────────────────────────────

    pub def append(data: {str = String}, f: String): Result[IoError, Unit] \ IO =
        try {
            let bytes = data#str.getBytes(StandardCharsets.UTF_8);
            let opts: Vector[OpenOption] = Vector#{
                checked_cast(StandardOpenOption.APPEND),
                checked_cast(StandardOpenOption.CREATE)
            };
            Files.write(Paths.get(f), bytes, opts);
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit] \ IO =
        try {
            let opts: Vector[OpenOption] = Vector#{
                checked_cast(StandardOpenOption.APPEND),
                checked_cast(StandardOpenOption.CREATE)
            };
            let bf = Files.newBufferedWriter(Paths.get(f), opts);
            List.forEach(line -> {
                bf.write(line, 0, String.length(line));
                bf.newLine()
            }, data#lines);
            bf.close();
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit] \ IO =
        try {
            region rc {
                let bytes = Vector.toArray(rc, data);
                let opts: Vector[OpenOption] = Vector#{
                    checked_cast(StandardOpenOption.APPEND),
                    checked_cast(StandardOpenOption.CREATE)
                };
                Files.write(Paths.get(f), bytes, opts);
                Ok(())
            }
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Truncate ──────────────────────────────────────────────────────

    pub def truncate(f: String): Result[IoError, Unit] \ IO =
        try {
            region rc {
                let opt: OpenOption = checked_cast(StandardOpenOption.TRUNCATE_EXISTING);
                let arr: Array[Int8, rc] = Array#{} @ rc;
                Files.write(Paths.get(f), arr, opt);
                Ok(())
            }
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Copy operation ────────────────────────────────────────────────

    pub def copyWith(src: {src = String}, dst: String, opts: Set[Fs.CopyOption]): Result[IoError, Unit] \ IO =
        try {
            let copyOpts: Vector[CopyOption] = Set.foldLeft((acc, opt) -> match opt {
                case Fs.CopyOption.ReplaceExisting => Vector.append(acc, Vector#{checked_cast(StandardCopyOption.REPLACE_EXISTING)})
                case Fs.CopyOption.CopyAttributes  => Vector.append(acc, Vector#{checked_cast(StandardCopyOption.COPY_ATTRIBUTES)})
            }, (Vector#{}: Vector[CopyOption]), opts);
            discard Files.copy(Paths.get(src#src), Paths.get(dst), copyOpts);
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: NoSuchFileException           => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def copy(src: {src = String}, dst: String): Result[IoError, Unit] \ IO =
        copyWith(src, dst, Set.empty())

    // ─── Move operation ────────────────────────────────────────────────

    pub def moveWith(src: {src = String}, dst: String, opts: Set[Fs.MoveOption]): Result[IoError, Unit] \ IO =
        try {
            let copyOpts: Vector[CopyOption] = Set.foldLeft((acc, opt) -> match opt {
                case Fs.MoveOption.AtomicMove      => Vector.append(acc, Vector#{checked_cast(StandardCopyOption.ATOMIC_MOVE)})
                case Fs.MoveOption.ReplaceExisting => Vector.append(acc, Vector#{checked_cast(StandardCopyOption.REPLACE_EXISTING)})
            }, (Vector#{}: Vector[CopyOption]), opts);
            discard Files.move(Paths.get(src#src), Paths.get(dst), copyOpts);
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: NoSuchFileException           => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def move(src: {src = String}, dst: String): Result[IoError, Unit] \ IO =
        moveWith(src, dst, Set.empty())

    // ─── Delete operation ────────────────────────────────────────────────

    pub def delete(f: String): Result[IoError, Unit] \ IO =
        try {
            Files.delete(Paths.get(f));
            Ok(())
        } catch {
            case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NoSuchFileException   => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Directory operations ──────────────────────────────────────────

    pub def mkDir(d: String): Result[IoError, Unit] \ IO =
        try {
            Files.createDirectory(Paths.get(d));
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def mkDirs(d: String): Result[IoError, Unit] \ IO =
        try {
            Files.createDirectories(Paths.get(d));
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def mkTempDir(prefix: String): Result[IoError, String] \ IO =
        try {
            let path = Files.createTempDirectory(prefix);
            Ok(path.toString())
        } catch {
            case ex: IllegalArgumentException      => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Path helper ────────────────────────────────────────────────────

    ///
    /// Resolves `path` against `baseDir` using Java's `Path.resolve` semantics:
    ///
    ///   - If `path` is absolute, it is returned as-is.
    ///   - If `path` is relative, it is joined to `baseDir`.
    ///
    pub def resolve(baseDir: String, path: String): String =
        unsafe IO { Paths.get(baseDir).resolve(path).toString() }

    ///
    /// Resolves `path` to its real filesystem location by following all symbolic links.
    ///
    /// Uses Java's `Path.toRealPath` to resolve every symlink component in the path.
    /// If the file does not exist yet (e.g., a write target), the parent directory is
    /// resolved instead and the filename is appended. This ensures that a symlink in
    /// an ancestor directory is still caught, while allowing writes to new files.
    ///
    /// Falls back to `normalize()` only when neither the path nor its parent can be
    /// resolved (e.g., the entire directory tree is missing).
    ///
    def resolveSymLink(path: String): Path =
        unsafe IO {
            let p = Paths.get(path).toAbsolutePath();
            try {
                p.toRealPath()
            } catch {
                case _: IOException =>
                    let parent = p.getParent();
                    if (Object.isNull(parent))
                        p.normalize()
                    else
                        try {
                            parent.toRealPath().resolve(p.getFileName())
                        } catch {
                            case _: IOException => p.normalize()
                        }
            }
        }

    ///
    /// Resolves `path` to its real filesystem target by following all symlinks.
    /// Returns `Ok(realPath)` if the path exists, or an appropriate `IoError` if the
    /// path is invalid, does not exist, or cannot be resolved.
    ///
    pub def resolveRealPath(path: String): Result[IoError, String] =
        unsafe IO {
            try {
                Ok(Paths.get(path).toRealPath().toString())
            } catch {
                case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
                case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
            }
        }

    ///
    /// Resolves `path` to its real filesystem target by following all symlinks.
    /// Falls back to the original `path` if resolution fails (e.g., the file does not
    /// exist yet). Useful for write/create operations where the target may not exist.
    ///
    pub def resolveRealPathOrSelf(path: String): String =
        match resolveRealPath(path) {
            case Ok(p) => p
            case _     => path
        }

    ///
    /// Resolves `path` against `chrootDir` and validates that the resolved result
    /// stays within the chroot directory. Symlinks are resolved to their real
    /// filesystem targets before the boundary check.
    ///
    /// Returns `Ok(resolvedPath)` if the path is within bounds,
    /// or `Err(PermissionDenied)` if the path escapes the chroot.
    ///
    pub def chroot(chrootDir: String, path: String): Result[IoError, String] =
        unsafe IO {
            try {
                let base = Paths.get(chrootDir).toRealPath();
                let resolved = resolveSymLink(Fs.FsLayer.resolve(chrootDir, path));
                if (resolved.startsWith(base))
                    Ok(resolved.toString())
                else
                    Err(IoError(ErrorKind.PermissionDenied,
                        "Path '${path}' escapes chroot '${chrootDir}'"))
            } catch {
                case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
                case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
            }
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it falls within
    /// at least one of the `allowedDirs`. Returns `Ok(resolvedPath)` if allowed,
    /// or `Err(PermissionDenied)` if the path is outside every allowed directory.
    ///
    pub def allowList(allowedDirs: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            try {
                let resolved = resolveSymLink(path);
                let allowed = Nel.exists(
                    dir -> {
                        let base = Paths.get(dir).toRealPath();
                        resolved.startsWith(base)
                    },
                    allowedDirs
                );
                if (allowed)
                    Ok(resolved.toString())
                else
                    Err(IoError(ErrorKind.PermissionDenied,
                        "Path '${path}' is not within any allowed directory"))
            } catch {
                case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
                case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
            }
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it does not fall
    /// within any of the `deniedDirs`. Returns `Ok(resolvedPath)` if permitted,
    /// or `Err(PermissionDenied)` if the path is within a denied directory.
    ///
    pub def denyList(deniedDirs: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            try {
                let resolved = resolveSymLink(path);
                let denied = Nel.exists(
                    dir -> {
                        let base = Paths.get(dir).toRealPath();
                        resolved.startsWith(base)
                    },
                    deniedDirs
                );
                if (denied)
                    Err(IoError(ErrorKind.PermissionDenied,
                        "Path '${path}' is within a denied directory"))
                else
                    Ok(resolved.toString())
            } catch {
                case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                case ex: NoSuchFileException  => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
                case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
            }
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it matches
    /// at least one of the given glob `patterns`. Returns `Ok(resolvedPath)` if allowed,
    /// or `Err(PermissionDenied)` if the path does not match any pattern.
    ///
    pub def allowGlob(patterns: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            try {
                let resolved = resolveSymLink(path);
                let allowed = Nel.exists(
                    pattern -> {
                        let matcher = FileSystems.getDefault().getPathMatcher("glob:${pattern}");
                        matcher.matches(resolved)
                    },
                    patterns
                );
                if (allowed)
                    Ok(resolved.toString())
                else
                    Err(IoError(ErrorKind.PermissionDenied,
                        "Path '${path}' does not match any allowed glob pattern"))
            } catch {
                case ex: IllegalArgumentException      => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            }
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it does not match
    /// any of the given glob `patterns`. Returns `Ok(resolvedPath)` if permitted,
    /// or `Err(PermissionDenied)` if the path matches a denied pattern.
    ///
    pub def denyGlob(patterns: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            try {
                let resolved = resolveSymLink(path);
                let denied = Nel.exists(
                    pattern -> {
                        let matcher = FileSystems.getDefault().getPathMatcher("glob:${pattern}");
                        matcher.matches(resolved)
                    },
                    patterns
                );
                if (denied)
                    Err(IoError(ErrorKind.PermissionDenied,
                        "Path '${path}' matches a denied glob pattern"))
                else
                    Ok(resolved.toString())
            } catch {
                case ex: IllegalArgumentException      => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            }
        }

    // ─── Atomic write helper ────────────────────────────────────────────

    ///
    /// Performs an atomic write to `file` by writing to a temporary sibling path
    /// first, then atomically moving it into place.
    ///
    /// - `doWrite(tmp)` writes data to the temporary path.
    /// - `doMoveAtomic(tmp, file)` atomically moves the temp file to the target.
    /// - `doDelete(tmp)` cleans up the temp file on failure.
    ///
    /// The caller supplies these as effect operations so that this helper works
    /// with any effect (`FileWrite`, `FileSystem`, etc.).
    ///
    pub def atomicWrite(
        file: String,
        doWrite: String -> Result[IoError, Unit] \ ef,
        doMoveAtomic: String -> String -> Result[IoError, Unit] \ ef,
        doDelete: String -> Result[IoError, Unit] \ ef
    ): Result[IoError, Unit] \ ef =
        let tmp = tempPathFor(file);
        match doWrite(tmp) {
            case Ok(_) => match doMoveAtomic(tmp, file) {
                case Ok(_)  => Ok(())
                case Err(e) => { discard doDelete(tmp); Err(e) }
            }
            case Err(e) => { discard doDelete(tmp); Err(e) }
        }

    ///
    /// Generates a unique temporary file path in the same directory as `target`.
    ///
    /// Uses UUID for uniqueness. The result is a sibling of `target` with the
    /// pattern `.~atomic~<name>.<uuid>.tmp`.
    ///
    def tempPathFor(target: String): String =
        unsafe IO {
            let path = Paths.get(target);
            let parent = path.getParent();
            let parentDir = if (Object.isNull(parent)) Paths.get(".") else parent;
            let name = path.getFileName().toString();
            let uuid = UUID.randomUUID().toString();
            parentDir.resolve(".~atomic~${name}.${uuid}.tmp").toString()
        }

    // ─── Parent directory helper ────────────────────────────────────────

    ///
    /// Returns the parent directory of `path`, or `None` if the path has no parent
    /// (e.g., a bare filename like `"test.txt"`).
    ///
    pub def parentDir(path: String): Option[String] =
        unsafe IO {
            let parent = Paths.get(path).getParent();
            if (Object.isNull(parent)) None else Some(parent.toString())
        }

    ///
    /// Ensures the parent directory of `file` exists before executing `doAction`.
    ///
    /// - If `file` has no parent (bare filename), `doAction()` proceeds directly.
    /// - If `doMkDirs(parent)` succeeds, `doAction()` proceeds.
    /// - If `doMkDirs(parent)` fails, the error is returned and `doAction` is
    ///   not performed.
    ///
    pub def ensureParentDirs(
        file: String,
        doMkDirs: String -> Result[IoError, Unit] \ ef,
        doAction: Unit -> Result[IoError, a] \ ef
    ): Result[IoError, a] \ ef =
        match parentDir(file) {
            case None    => doAction()
            case Some(d) => match doMkDirs(d) {
                case Ok(_)  => doAction()
                case Err(e) => Err(e)
            }
        }

    // ─── Backup helper ───────────────────────────────────────────────

    ///
    /// Performs a backup of `file` to `file + suffix` before executing `doAction`.
    ///
    /// - `doCopy(file, bak)` copies the file to the backup path.
    /// - If the copy succeeds, `doAction()` is executed.
    /// - If the copy fails with `NotFound`, the file doesn't exist yet, so
    ///   `doAction()` proceeds without backup.
    /// - If the copy fails with any other error, the operation is aborted.
    ///
    pub def backup(
        file: String,
        suffix: String,
        doCopy: String -> String -> Result[IoError, Unit] \ ef,
        doAction: Unit -> Result[IoError, a] \ ef
    ): Result[IoError, a] \ ef =
        let bak = file + suffix;
        match doCopy(file, bak) {
            case Ok(_)                              => doAction()
            case Err(IoError(ErrorKind.NotFound, _)) => doAction()
            case Err(e)                             => Err(e)
        }

    // ─── Transfer limit helper ──────────────────────────────────────────

    ///
    /// Checks that `actualSize` does not exceed `maxSize`.
    /// Returns `Ok(())` if within the limit, or `Err(TransferLimitExceeded)` otherwise.
    ///
    pub def checkTransferLimit(maxSize: Size, actualSize: Size): Result[IoError, Unit] =
        let maxBytes = toBytes(maxSize);
        let actualBytes = toBytes(actualSize);
        if (actualBytes <= maxBytes)
            Ok(())
        else
            Err(IoError(ErrorKind.TransferLimitExceeded,
                "transfer limit exceeded: ${actualSize} > ${maxSize}"))

    ///
    /// Estimates the UTF-8 byte size of a string by multiplying the character
    /// count by 4 (the maximum bytes per character in UTF-8).
    ///
    pub def estimateStringBytes(s: String): Size =
        bytes(Int32.toInt64(String.length(s)) * 4i64)

    ///
    /// Estimates the UTF-8 byte size of a list of strings by multiplying the
    /// total character count by 4 (the maximum bytes per character in UTF-8).
    ///
    pub def estimateStringListBytes(l: List[String]): Size =
        bytes(Int32.toInt64(List.sumWith(String.length, l)) * 4i64)

    // ─── Conflict check helpers ────────────────────────────────────────

    ///
    /// Snapshots the modification time of `file` into `snapshots`.
    ///
    /// If `getModTime` fails (file deleted, etc.), silently skips.
    ///
    pub def snapshotModTime(
        file: String,
        snapshots: MutMap[String, Int64, r],
        getModTime: String -> Result[IoError, Int64] \ ef
    ): Unit \ { ef, r } =
        match getModTime(file) {
            case Ok(t) => MutMap.put(file, t, snapshots)
            case _     => ()
        }

    ///
    /// Checks whether `file` was modified externally since last snapshot.
    ///
    /// - No snapshot → `Ok()` (file was never read, nothing to conflict with).
    /// - Mod time matches snapshot → `Ok()`.
    /// - Mod time differs → `Err(Conflict(...))`.
    /// - File deleted (`NotFound`) → `Ok()` (file gone, proceed).
    /// - Other `getModTime` error → propagated.
    ///
    pub def conflictCheck(
        file: String,
        snapshots: MutMap[String, Int64, r],
        getModTime: String -> Result[IoError, Int64] \ ef
    ): Result[IoError, Unit] \ { ef, r } =
        match MutMap.get(file, snapshots) {
            case None          => Ok(())
            case Some(oldTime) =>
                match getModTime(file) {
                    case Ok(curTime) =>
                        if (curTime == oldTime)
                            Ok(())
                        else
                            Err(IoError(ErrorKind.Conflict,
                                "Conflict on '${file}': expected mtime ${oldTime}, found ${curTime}"))
                    case Err(IoError(ErrorKind.NotFound, _)) => Ok(())
                    case Err(e) => Err(e)
                }
        }

    // ─── Checksum helpers ──────────────────────────────────────────────

    ///
    /// Converts a vector of bytes to a lowercase hex string.
    ///
    pub def bytesToHex(v: Vector[Int8]): String = region rc {
        let arr = Vector.toArray(rc, v);
        unsafe IO { HexFormat.of().formatHex(arr) }
    }

    ///
    /// Verifies `actual` (a hex digest string) against the checksum stored in
    /// the sidecar file `file + sidecar`.
    ///
    /// - If the sidecar does not exist (`NotFound`), returns `Ok(())`.
    /// - If the sidecar exists and matches, returns `Ok(())`.
    /// - If the sidecar exists and does not match, returns
    ///   `Err(IoError(ChecksumMismatch, ...))`.
    ///
    pub def verifyChecksum(
        file: String,
        sidecar: String,
        actual: String,
        doReadSidecar: String -> Result[IoError, String] \ ef
    ): Result[IoError, Unit] \ ef =
        match doReadSidecar(file + sidecar) {
            case Ok(expected) =>
                if (String.trim(expected) == actual)
                    Ok(())
                else
                    Err(IoError(ErrorKind.ChecksumMismatch,
                        "Checksum mismatch for '${file}': expected ${String.trim(expected)}, got ${actual}"))
            case Err(IoError(ErrorKind.NotFound, _)) => Ok(())
            case Err(e) => Err(e)
        }

    ///
    /// Reads the bytes of `file`, hashes them with `computeHex`, and writes
    /// the hex digest to the sidecar path `file + sidecar`.
    ///
    pub def updateSidecar(
        file: String,
        sidecar: String,
        computeHex: Vector[Int8] -> String,
        doReadBytes: String -> Result[IoError, Vector[Int8]] \ ef1,
        doWriteSidecar: (String, String) -> Result[IoError, Unit] \ ef2
    ): Result[IoError, Unit] \ ef1 + ef2 =
        forM (bytes <- doReadBytes(file);
              _     <- doWriteSidecar(computeHex(bytes), file + sidecar))
            yield ()

    ///
    /// Performs `doWrite`, then computes and writes a checksum sidecar for `file`.
    ///
    /// `doUpdateSidecar(file)` should read the file bytes, hash them, and
    /// write the hex digest to the sidecar path.
    ///
    pub def checksumAfterWrite(
        file: String,
        doWrite: Unit -> Result[IoError, Unit] \ ef1,
        doUpdateSidecar: String -> Result[IoError, Unit] \ ef2
    ): Result[IoError, Unit] \ ef1 + ef2 =
        Result.flatMap(_ -> doUpdateSidecar(file), doWrite())

    ///
    /// Verifies a `readBytes` result against a checksum sidecar.
    ///
    /// The returned bytes are hashed via `computeHex` and compared against
    /// the sidecar. If no sidecar exists, the result passes through.
    ///
    pub def checksumOnReadBytes(
        file: String,
        sidecar: String,
        computeHex: Vector[Int8] -> String,
        readResult: Result[IoError, Vector[Int8]],
        doReadSidecar: String -> Result[IoError, String] \ ef
    ): Result[IoError, Vector[Int8]] \ ef =
        forM (bytes <- readResult;
              _     <- verifyChecksum(file, sidecar, computeHex(bytes), doReadSidecar))
            yield bytes

    ///
    /// Verifies a `read` or `readLines` result against a checksum sidecar.
    ///
    /// Since the result type (`String` or `List[String]`) does not preserve
    /// raw bytes, `doReadBytes` is called to obtain the on-disk bytes for
    /// hashing. If no sidecar exists, the result passes through.
    ///
    pub def checksumOnRead(
        file: String,
        sidecar: String,
        computeHex: Vector[Int8] -> String,
        readResult: Result[IoError, a],
        doReadBytes: String -> Result[IoError, Vector[Int8]] \ ef,
        doReadSidecar: String -> Result[IoError, String] \ ef
    ): Result[IoError, a] \ ef =
        forM (v     <- readResult;
              bytes <- doReadBytes(file);
              _     <- verifyChecksum(file, sidecar, computeHex(bytes), doReadSidecar))
            yield v

    ///
    /// Moves the checksum sidecar after a successful `moveWith`.
    ///
    /// If the source sidecar does not exist, the move result is returned
    /// unchanged. Sidecar move errors are discarded.
    ///
    pub def checksumAfterMove(
        src: String,
        dst: String,
        sidecar: String,
        moveResult: Result[IoError, Unit],
        doMoveSidecar: String -> String -> Result[IoError, Unit] \ ef
    ): Result[IoError, Unit] \ ef =
        match moveResult {
            case Ok(_)  => { discard doMoveSidecar(src + sidecar, dst + sidecar); Ok(()) }
            case Err(e) => Err(e)
        }

    ///
    /// Deletes the checksum sidecar after a successful `delete`.
    ///
    /// If the sidecar does not exist, the delete result is returned
    /// unchanged. Sidecar delete errors are discarded.
    ///
    pub def checksumAfterDelete(
        file: String,
        sidecar: String,
        deleteResult: Result[IoError, Unit],
        doDeleteSidecar: String -> Result[IoError, Unit] \ ef
    ): Result[IoError, Unit] \ ef =
        match deleteResult {
            case Ok(_)  => { discard doDeleteSidecar(file + sidecar); Ok(()) }
            case Err(e) => Err(e)
        }

    // ─── Size rotation helper ──────────────────────────────────────────

    ///
    /// Rotates `file` if its size meets or exceeds `maxSize`, then executes `doAction`.
    ///
    /// Rotation shifts existing numbered files (`file.1` … `file.{maxFiles}`)
    /// upward by one, deletes the oldest (`file.{maxFiles}`), and moves `file`
    /// to `file.1`. The `doAction` then writes to the now-absent original path.
    ///
    /// If the file does not exist (`NotFound`), rotation is skipped and
    /// `doAction` proceeds directly (append will create it).
    ///
    pub def rotateIfNeeded(
        file:     String,
        maxSize:  Size,
        maxFiles: Int32,
        doSize:   String -> Result[IoError, Size] \ ef1,
        doMove:   String -> String -> Result[IoError, Unit] \ ef2,
        doDelete: String -> Result[IoError, Unit] \ ef2,
        doAction: Unit -> Result[IoError, a] \ ef2
    ): Result[IoError, a] \ ef1 + ef2 =
        match doSize(file) {
            case Err(IoError(ErrorKind.NotFound, _)) => doAction()
            case Err(e)                              => Err(e)
            case Ok(sz) =>
                if (toBytes(sz) < toBytes(maxSize))
                    doAction()
                else {
                    // Delete the oldest rotated file (ignore NotFound)
                    discard match doDelete("${file}.${maxFiles}") {
                        case Err(IoError(ErrorKind.NotFound, _)) => Ok(())
                        case other                               => other
                    };
                    // Shift file.{i} → file.{i+1} for i from maxFiles-1 down to 1
                    def shift(i) = {
                        if (i < 1)
                            Ok(())
                        else
                            match doMove("${file}.${i}", "${file}.${i + 1}") {
                                case Ok(_)                               => shift(i - 1)
                                case Err(IoError(ErrorKind.NotFound, _)) => shift(i - 1)
                                case Err(e)                              => Err(e)
                            }
                    };
                    match shift(maxFiles - 1) {
                        case Err(e) => Err(e)
                        case Ok(_)  =>
                            // Move the current file to file.1
                            match doMove(file, "${file}.1") {
                                case Err(e) => Err(e)
                                case Ok(_)  => doAction()
                            }
                    }
                }
        }

    // ─── Private helper ────────────────────────────────────────────────

    ///
    /// Returns the attributes of the given file `f`.
    /// May throw `IOException` and `SecurityException`.
    ///
    def fileAttributes(path: Path): BasicFileAttributes \ IO =
        Files.readAttributes(
            path,
            Class.forName("java.nio.file.attribute.BasicFileAttributes"),
            noFollow()
        )

}
