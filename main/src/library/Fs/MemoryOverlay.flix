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

mod Fs.MemoryOverlay {

    import java.nio.file.FileSystems
    import java.nio.file.PathMatcher
    import java.nio.file.Paths

    use IoError.IoError
    use IoError.ErrorKind

    /// In-memory overlay state, parameterized by region.
    ///
    /// - `files`:     map from normalized absolute paths to in-memory content (text or bytes).
    /// - `dirs`:      set of normalized absolute paths registered as directories.
    /// - `whiteouts`: set of normalized absolute paths deleted in the overlay (hides lower-layer entries).
    /// - `counter`:   monotonically increasing counter for generating unique temp directory names.
    pub struct MemoryOverlay[r: Region] {
        mut files: Map[String, OverlayEntry],
        mut dirs: Set[String],
        mut whiteouts: Set[String],
        mut counter: Int32
    }

    /// The content of an overlay entry.
    pub enum OverlayEntry with ToString {
        case BinaryEntry(Vector[Int8]),
        case TextEntry(String)
    }

    /// Creates a fresh, empty overlay in region `rc`.
    pub def fresh(rc: Region[r]): Fs.MemoryOverlay[r] \ r =
        new Fs.MemoryOverlay @ rc { files = Map.empty(), dirs = Set.empty(), whiteouts = Set.empty(), counter = 0 }

    // ─── Path helpers (private) ───────────────────────────────────────

    def normalize(path: String): String =
        unsafe IO { Paths.get(path).toAbsolutePath().normalize().toString() }

    def parentDir(path: String): Option[String] =
        unsafe IO {
            let p = Paths.get(path).getParent();
            if (Object.isNull(p)) None else Some(p.toString())
        }

    def fileName(path: String): String =
        unsafe IO {
            let p = Paths.get(path).getFileName();
            if (Object.isNull(p)) path else p.toString()
        }

    def notFound(path: String): Result[IoError, a] =
        Err(IoError(ErrorKind.NotFound, "overlay: '${path}' does not exist"))

    // ─── Low-level state queries (private) ────────────────────────────

    def hasFile(path: String, o: Fs.MemoryOverlay[r]): Bool \ r =
        Map.memberOf(normalize(path), o->files)

    def hasDir(path: String, o: Fs.MemoryOverlay[r]): Bool \ r =
        Set.memberOf(normalize(path), o->dirs)

    def hasEntry(path: String, o: Fs.MemoryOverlay[r]): Bool \ r =
        hasFile(path, o) or hasDir(path, o)

    def isWhiteout(path: String, o: Fs.MemoryOverlay[r]): Bool \ r =
        Set.memberOf(normalize(path), o->whiteouts)

    // ─── Handler-level methods — reads return Option[Result] ──────────

    /// Overlay-aware exists check.
    pub def tryExists(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasEntry(path, o))        Some(Ok(true))
        else if (isWhiteout(path, o)) Some(Ok(false))
        else                          None

    /// Overlay-aware isDirectory check.
    pub def tryIsDirectory(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasDir(path, o))          Some(Ok(true))
        else if (hasFile(path, o))    Some(Ok(false))
        else if (isWhiteout(path, o)) Some(Ok(false))
        else                          None

    /// Overlay-aware isRegularFile check.
    pub def tryIsRegularFile(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasFile(path, o))         Some(Ok(true))
        else if (hasDir(path, o))     Some(Ok(false))
        else if (isWhiteout(path, o)) Some(Ok(false))
        else                          None

    /// Overlay-aware isSymbolicLink check. In-memory entries are never symlinks.
    pub def tryIsSymbolicLink(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasEntry(path, o))        Some(Ok(false))
        else if (isWhiteout(path, o)) Some(Ok(false))
        else                          None

    /// Overlay-aware isReadable. In-memory entries are always readable.
    pub def tryIsReadable(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasEntry(path, o))        Some(Ok(true))
        else if (isWhiteout(path, o)) Some(notFound(path))
        else                          None

    /// Overlay-aware isWritable. In-memory entries are always writable.
    pub def tryIsWritable(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasEntry(path, o))        Some(Ok(true))
        else if (isWhiteout(path, o)) Some(notFound(path))
        else                          None

    /// Overlay-aware isExecutable. In-memory entries are never executable.
    pub def tryIsExecutable(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Bool]] \ r =
        if (hasEntry(path, o))        Some(Ok(false))
        else if (isWhiteout(path, o)) Some(notFound(path))
        else                          None

    /// Overlay-aware time query. In-memory entries return 0.
    pub def tryTime(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Int64]] \ r =
        if (hasEntry(path, o))        Some(Ok(0i64))
        else if (isWhiteout(path, o)) Some(notFound(path))
        else                          None

    /// Overlay-aware size query. Computes from content length.
    pub def trySize(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Fs.Size]] \ r =
        use Fs.Size.bytes;
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(OverlayEntry.TextEntry(s))  => Some(Ok(Fs.FsLayer.estimateStringBytes(s)))
            case Some(OverlayEntry.BinaryEntry(b)) => Some(Ok(bytes(Int32.toInt64(Vector.length(b)))))
            case None =>
                if (isWhiteout(path, o)) Some(notFound(path))
                else None
        }

    /// Overlay-aware read (text).
    pub def tryRead(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, String]] \ r =
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(OverlayEntry.TextEntry(s))  => Some(Ok(s))
            case Some(OverlayEntry.BinaryEntry(b)) => Some(Ok(String.fromBytes(b)))
            case None =>
                if (isWhiteout(path, o)) Some(notFound(path))
                else None
        }

    /// Overlay-aware readLines.
    pub def tryReadLines(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, List[String]]] \ r =
        match tryRead(path, o) {
            case Some(Ok(s))  => Some(Ok(String.lines(s)))
            case Some(Err(e)) => Some(Err(e))
            case None         => None
        }

    /// Overlay-aware readBytes.
    pub def tryReadBytes(path: String, o: Fs.MemoryOverlay[r]): Option[Result[IoError, Vector[Int8]]] \ r =
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(OverlayEntry.BinaryEntry(b)) => Some(Ok(b))
            case Some(OverlayEntry.TextEntry(s))  => Some(Ok(String.toBytes(s)))
            case None =>
                if (isWhiteout(path, o)) Some(notFound(path))
                else None
        }

    // ─── Handler-level methods — writes return Result directly ────────

    /// Stores text content.
    pub def write(path: String, data: String, o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        o->files = Map.insert(norm, OverlayEntry.TextEntry(data), o->files);
        o->whiteouts = Set.remove(norm, o->whiteouts);
        Ok(())

    /// Stores lines as text content.
    pub def writeLines(path: String, lines: List[String], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        write(path, String.intercalate(String.lineSeparator(), lines), o)

    /// Stores byte content.
    pub def writeBytes(path: String, data: Vector[Int8], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        o->files = Map.insert(norm, OverlayEntry.BinaryEntry(data), o->files);
        o->whiteouts = Set.remove(norm, o->whiteouts);
        Ok(())

    /// Appends text. `lower` is the current content from the lower layer (if not in overlay/whiteout).
    pub def append(path: String, data: String, lower: Option[String], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        let current = match Map.get(norm, o->files) {
            case Some(OverlayEntry.TextEntry(s))  => s
            case Some(OverlayEntry.BinaryEntry(b)) => String.fromBytes(b)
            case None => Option.getWithDefault("", lower)
        };
        write(path, current + data, o)

    /// Appends lines. `lower` is the current content from the lower layer.
    pub def appendLines(path: String, lines: List[String], lower: Option[String], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let suffix = String.intercalate(String.lineSeparator(), lines);
        append(path, suffix, lower, o)

    /// Appends bytes. `lower` is the current content from the lower layer.
    pub def appendBytes(path: String, data: Vector[Int8], lower: Option[Vector[Int8]], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        let current = match Map.get(norm, o->files) {
            case Some(OverlayEntry.BinaryEntry(b)) => b
            case Some(OverlayEntry.TextEntry(s))  => String.toBytes(s)
            case None => Option.getWithDefault(Vector.empty(), lower)
        };
        writeBytes(path, Vector.append(current, data), o)

    /// Truncates a file (stores empty content).
    pub def truncate(path: String, o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        write(path, "", o)

    /// Moves an entry. `lower` is the source bytes from the lower layer if not in overlay.
    pub def move(src: String, dst: String, lower: Option[Vector[Int8]], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let normSrc = normalize(src);
        let entry = match Map.get(normSrc, o->files) {
            case Some(e) => Some(e)
            case None    => Option.map(b -> OverlayEntry.BinaryEntry(b), lower)
        };
        match entry {
            case Some(e) =>
                let normDst = normalize(dst);
                o->files = Map.insert(normDst, e, o->files);
                o->whiteouts = Set.remove(normDst, o->whiteouts);
                o->files = Map.remove(normSrc, o->files);
                o->dirs = Set.remove(normSrc, o->dirs);
                o->whiteouts = Set.insert(normSrc, o->whiteouts);
                Ok(())
            case None => notFound(src)
        }

    /// Copies an entry. `lower` is the source bytes from the lower layer if not in overlay.
    pub def copy(src: String, dst: String, lower: Option[Vector[Int8]], o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let normSrc = normalize(src);
        let entry = match Map.get(normSrc, o->files) {
            case Some(e) => Some(e)
            case None    => Option.map(b -> OverlayEntry.BinaryEntry(b), lower)
        };
        match entry {
            case Some(e) =>
                let normDst = normalize(dst);
                o->files = Map.insert(normDst, e, o->files);
                o->whiteouts = Set.remove(normDst, o->whiteouts);
                Ok(())
            case None => notFound(src)
        }

    /// Deletes an entry (adds whiteout).
    pub def delete(path: String, o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        o->files = Map.remove(norm, o->files);
        o->dirs = Set.remove(norm, o->dirs);
        o->whiteouts = Set.insert(norm, o->whiteouts);
        Ok(())

    /// Creates a directory.
    pub def mkDir(path: String, o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        o->dirs = Set.insert(norm, o->dirs);
        o->whiteouts = Set.remove(norm, o->whiteouts);
        Ok(())

    /// Creates a directory and all parents.
    pub def mkDirs(path: String, o: Fs.MemoryOverlay[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        o->dirs = Set.insert(norm, o->dirs);
        o->whiteouts = Set.remove(norm, o->whiteouts);
        match parentDir(norm) {
            case Some(parent) =>
                if (not Set.memberOf(parent, o->dirs)) mkDirs(parent, o) else Ok(())
            case None => Ok(())
        }

    /// Generates a unique temp directory and registers it.
    pub def mkTempDir(prefix: String, o: Fs.MemoryOverlay[r]): Result[IoError, String] \ r =
        let n = o->counter;
        o->counter = n + 1;
        let name = "${prefix}overlay-${n}";
        o->dirs = Set.insert(name, o->dirs);
        Ok(name)

    /// Merges overlay listing with lower-layer listing.
    /// `lower` is the result of listing from the real filesystem.
    pub def list(dir: String, lower: Result[IoError, List[String]], o: Fs.MemoryOverlay[r]): Result[IoError, List[String]] \ r =
        let norm = normalize(dir);
        if (isWhiteout(dir, o)) notFound(dir)
        else {
            let whiteoutNames = o->whiteouts
                |> Set.filter(p -> parentDir(p) == Some(norm))
                |> Set.map(p -> fileName(p));
            let overlayFileNames = Map.keysOf(o->files)
                |> Set.filter(p -> parentDir(p) == Some(norm))
                |> Set.map(p -> fileName(p));
            let overlayDirNames = o->dirs
                |> Set.filter(p -> parentDir(p) == Some(norm))
                |> Set.map(p -> fileName(p));
            let overlayNames = Set.union(overlayFileNames, overlayDirNames);
            let lowerNames = match lower {
                case Ok(names) => List.filter(n -> not Set.memberOf(n, whiteoutNames), names)
                case Err(_)    => Nil
            };
            let merged = List.append(Set.toList(overlayNames), lowerNames) |> List.distinct;
            Ok(merged)
        }

    /// Merges overlay entries with lower-layer glob results.
    /// `lower` is the result of globbing from the real filesystem.
    pub def glob(base: String, pattern: String, lower: Result[IoError, List[String]], o: Fs.MemoryOverlay[r]): Result[IoError, List[String]] \ r =
        if (isWhiteout(base, o)) notFound(base)
        else unsafe IO {
            let basePath = Paths.get(normalize(base));
            let matcher: PathMatcher = FileSystems.getDefault().getPathMatcher("glob:${pattern}");
            let matchesPattern = path -> {
                let p = Paths.get(path);
                let starts = p.startsWith(basePath);
                if (starts) {
                    let rel = basePath.relativize(p);
                    matcher.matches(rel)
                } else false
            };
            let overlayPaths = Set.union(
                Map.keysOf(o->files) |> Set.filter(matchesPattern),
                o->dirs              |> Set.filter(matchesPattern)
            );
            let whiteouts = o->whiteouts;
            let lowerPaths = match lower {
                case Ok(paths) => List.filter(p -> not Set.memberOf(normalize(p), whiteouts), paths)
                case Err(_)    => Nil
            };
            let merged = List.append(Set.toList(overlayPaths), lowerPaths) |> List.distinct;
            Ok(merged)
        }

    // ─── Helper for handler ───────────────────────────────────────────

    /// Returns true if the path needs lower-layer content (not in overlay, not whiteout'd).
    pub def needsLower(path: String, o: Fs.MemoryOverlay[r]): Bool \ r =
        let norm = normalize(path);
        not Map.memberOf(norm, o->files) and not Set.memberOf(norm, o->whiteouts)

}
