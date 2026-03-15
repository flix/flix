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
    import java.nio.file.StandardOpenOption

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
