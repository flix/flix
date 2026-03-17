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
    import java.lang.Class
    import java.lang.IllegalArgumentException
    import java.lang.UnsupportedOperationException
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
    import java.util.UUID
    import java.util.stream.Stream

    use IoError.IoError
    use IoError.ErrorKind
    use RichString.{text, bold, cyan, green, red, gray}

    // ─── Logging helpers ───────────────────────────────────────────────

    ///
    /// Logs a pre-operation message at `Debug` level.
    ///
    pub def logPre(op: String, path: String): Unit \ Logger =
        Logger.log(Severity.Debug,
            gray("-->") + text(" ") + bold(op) + text(" ") + cyan(path))

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

    // ─── File-test operations ──────────────────────────────────────────

    pub def exists(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.exists(Paths.get(f), (...{}: Vector[LinkOption])))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isDirectory(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isDirectory(Paths.get(f), (...{}: Vector[LinkOption])))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
        }

    pub def isRegularFile(f: String): Result[IoError, Bool] \ IO =
        try {
            Ok(Files.isRegularFile(Paths.get(f), (...{}: Vector[LinkOption])))
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
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def creationTime(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(fileAttributes(Paths.get(f)).creationTime().toMillis())
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def modificationTime(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(fileAttributes(Paths.get(f)).lastModifiedTime().toMillis())
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── File-stat operations ──────────────────────────────────────────

    pub def size(f: String): Result[IoError, Int64] \ IO =
        try {
            Ok(Files.size(Paths.get(f)))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Read operations ───────────────────────────────────────────────

    pub def read(f: String): Result[IoError, String] \ IO =
        try {
            Ok(String.fromBytes(Array.toVector(Files.readAllBytes(Paths.get(f)))))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def readLines(f: String): Result[IoError, List[String]] \ IO =
        try {
            Ok(ToFlix.toFlix(Files.readAllLines(Paths.get(f))))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def readBytes(f: String): Result[IoError, Vector[Int8]] \ IO =
        try {
            Ok(Array.toVector(Files.readAllBytes(Paths.get(f))))
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Directory listing ─────────────────────────────────────────────

    pub def list(f: String): Result[IoError, List[String]] \ IO =
        try {
            let file = new File(f);
            Ok(Array.toList(file.list()))
        } catch {
            case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: NotDirectoryException => Err(IoError(ErrorKind.NotDirectory, ex.getMessage()))
            case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Glob operation ─────────────────────────────────────────────────

    pub def glob(base: String, pattern: String): Result[IoError, List[String]] \ IO =
        try {
            region rc {
                let basePath = Paths.get(base);
                let matcher = FileSystems.getDefault().getPathMatcher("glob:${pattern}");
                let stream = Files.walk(basePath, (...{}: Vector[FileVisitOption]));
                let iter = Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[Path]), stream);
                let result = iter
                    |> Iterator.filterMap(path -> {
                        let rel = basePath.relativize(path);
                        if (matcher.matches(rel))
                            Some(path.toString())
                        else
                            None
                    })
                    |> Iterator.toList;
                let bs: BaseStream = checked_cast(stream);
                bs.close();
                Ok(result)
            }
        } catch {
            case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    // ─── Write operations ──────────────────────────────────────────────

    pub def write(data: {str = String}, f: String): Result[IoError, Unit] \ IO =
        try {
            let bytes = data#str.getBytes(StandardCharsets.UTF_8);
            Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit] \ IO =
        try {
            let bf = Files.newBufferedWriter(Paths.get(f), (...{}: Vector[OpenOption]));
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
                Files.write(Paths.get(f), bytes, (...{}: Vector[OpenOption]));
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
            let opts: Vector[OpenOption] = ...{
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
            let opts: Vector[OpenOption] = ...{
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
                let opts: Vector[OpenOption] = ...{
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
                Files.write(Paths.get(f), arr, ...{opt});
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
            case ex: InvalidPathException       => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: FileAlreadyExistsException => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: NoSuchFileException        => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException                => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
            case ex: InvalidPathException       => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: FileAlreadyExistsException => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: NoSuchFileException        => Err(IoError(ErrorKind.NotFound, ex.getMessage()))
            case ex: IOException                => Err(IoError(ErrorKind.Other, ex.getMessage()))
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
            Files.createDirectory(Paths.get(d), (...{}: Vector[FileAttribute]));
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def mkDirs(d: String): Result[IoError, Unit] \ IO =
        try {
            Files.createDirectories(Paths.get(d), (...{}: Vector[FileAttribute]));
            Ok(())
        } catch {
            case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
            case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
            case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
            case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

    pub def mkTempDir(prefix: String): Result[IoError, String] \ IO =
        try {
            let path = Files.createTempDirectory(prefix, (...{}: Vector[FileAttribute]));
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
    /// Resolves `path` against `chrootDir` and validates that the normalized result
    /// stays within the chroot directory.
    ///
    /// Returns `Ok(resolvedPath)` if the path is within bounds,
    /// or `Err(PermissionDenied)` if the path escapes the chroot.
    ///
    pub def chroot(chrootDir: String, path: String): Result[IoError, String] =
        unsafe IO {
            let base = Paths.get(chrootDir).toAbsolutePath().normalize();
            let resolved = base.resolve(path).normalize();
            if (resolved.startsWith(base))
                Ok(resolved.toString())
            else
                Err(IoError(ErrorKind.PermissionDenied,
                    "Path '${path}' escapes chroot '${chrootDir}'"))
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it falls within
    /// at least one of the `allowedDirs`. Returns `Ok(resolvedPath)` if allowed,
    /// or `Err(PermissionDenied)` if the path is outside every allowed directory.
    ///
    pub def allowList(allowedDirs: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            let resolved = Paths.get(path).toAbsolutePath().normalize();
            let allowed = Nel.exists(
                dir -> {
                    let base = Paths.get(dir).toAbsolutePath().normalize();
                    resolved.startsWith(base)
                },
                allowedDirs
            );
            if (allowed)
                Ok(resolved.toString())
            else
                Err(IoError(ErrorKind.PermissionDenied,
                    "Path '${path}' is not within any allowed directory"))
        }

    ///
    /// Resolves `path` to an absolute, normalized form and checks that it does not fall
    /// within any of the `deniedDirs`. Returns `Ok(resolvedPath)` if permitted,
    /// or `Err(PermissionDenied)` if the path is within a denied directory.
    ///
    pub def denyList(deniedDirs: Nel[String], path: String): Result[IoError, String] =
        unsafe IO {
            let resolved = Paths.get(path).toAbsolutePath().normalize();
            let denied = Nel.exists(
                dir -> {
                    let base = Paths.get(dir).toAbsolutePath().normalize();
                    resolved.startsWith(base)
                },
                deniedDirs
            );
            if (denied)
                Err(IoError(ErrorKind.PermissionDenied,
                    "Path '${path}' is within a denied directory"))
            else
                Ok(resolved.toString())
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

    // ─── Private helper ────────────────────────────────────────────────

    ///
    /// Returns the attributes of the given file `f`.
    /// May throw `IOException` and `SecurityException`.
    ///
    def fileAttributes(path: Path): BasicFileAttributes \ IO =
        Files.readAttributes(
            path,
            Class.forName("java.nio.file.attribute.BasicFileAttributes"),
            (...{}: Vector[LinkOption])
        )

}
