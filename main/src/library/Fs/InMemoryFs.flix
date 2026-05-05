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

mod Fs.InMemoryFs {

    import java.nio.file.FileSystems
    import java.nio.file.PathMatcher
    import java.nio.file.Paths

    use IoError.IoError
    use IoError.ErrorKind
    use Time.Clock

    /// Fully in-memory filesystem state, parameterized by region.
    ///
    /// - `files`:   map from normalized absolute paths to in-memory content (text or bytes).
    /// - `dirs`:    set of normalized absolute paths registered as directories.
    /// - `times`:   map from normalized absolute paths to `{atime, ctime, mtime}` records (ms since epoch).
    /// - `counter`: monotonically increasing counter for generating unique temp directory names.
    pub struct InMemoryFs[r: Region] {
        mut files: Map[String, Entry],
        mut dirs: Set[String],
        mut times: Map[String, {atime = Int64, ctime = Int64, mtime = Int64}],
        mut counter: Int32
    }

    /// The content of an in-memory entry.
    pub enum Entry with ToString {
        case BinaryEntry(Vector[Int8]),
        case TextEntry(String)
    }

    /// Creates a fresh, empty in-memory filesystem in region `rc`.
    pub def fresh(rc: Region[r]): Fs.InMemoryFs[r] \ r =
        new Fs.InMemoryFs @ rc { files = Map.empty(), dirs = Set.empty(), times = Map.empty(), counter = 0 }

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
        Err(IoError(ErrorKind.NotFound, "in-memory fs: '${path}' does not exist"))

    def alreadyExists(path: String): Result[IoError, a] =
        Err(IoError(ErrorKind.AlreadyExists, "in-memory fs: '${path}' already exists"))

    // ─── Timestamp helpers (private) ─────────────────────────────────

    /// Sets all three timestamps to now (for newly created entries).
    def touchNew(norm: String, o: Fs.InMemoryFs[r]): Unit \ r + Clock =
        let t = Clock.now();
        o->times = Map.insert(norm, {atime = t, ctime = t, mtime = t}, o->times)

    /// Updates atime and mtime to now, preserving ctime (for modifications).
    def touchModify(norm: String, o: Fs.InMemoryFs[r]): Unit \ r + Clock =
        let t = Clock.now();
        let ts = match Map.get(norm, o->times) {
            case Some(prev) => {atime = t, ctime = prev#ctime, mtime = t}
            case None       => {atime = t, ctime = t, mtime = t}
        };
        o->times = Map.insert(norm, ts, o->times)

    /// Updates atime to now, preserving ctime and mtime (for reads).
    def touchAccess(norm: String, o: Fs.InMemoryFs[r]): Unit \ r + Clock =
        let t = Clock.now();
        let ts = match Map.get(norm, o->times) {
            case Some(prev) => {atime = t, ctime = prev#ctime, mtime = prev#mtime}
            case None       => {atime = t, ctime = t, mtime = t}
        };
        o->times = Map.insert(norm, ts, o->times)

    // ─── Low-level state queries (private) ────────────────────────────

    def hasFile(path: String, o: Fs.InMemoryFs[r]): Bool \ r =
        Map.memberOf(normalize(path), o->files)

    def hasDir(path: String, o: Fs.InMemoryFs[r]): Bool \ r =
        Set.memberOf(normalize(path), o->dirs)

    def hasEntry(path: String, o: Fs.InMemoryFs[r]): Bool \ r =
        hasFile(path, o) or hasDir(path, o)

    // ─── Stat/test ops ───────────────────────────────────────────────

    pub def exists(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        Ok(hasEntry(path, o))

    pub def isDirectory(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        Ok(hasDir(path, o))

    pub def isRegularFile(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        Ok(hasFile(path, o))

    pub def isSymbolicLink(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        discard hasEntry(path, o);
        Ok(false)

    pub def isReadable(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        Ok(hasEntry(path, o))

    pub def isWritable(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        Ok(hasEntry(path, o))

    pub def isExecutable(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Bool] \ r =
        discard hasEntry(path, o);
        Ok(false)

    pub def accessTime(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Int64] \ r =
        let norm = normalize(path);
        match Map.get(norm, o->times) {
            case Some(ts) => Ok(ts#atime)
            case None     => notFound(path)
        }

    pub def creationTime(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Int64] \ r =
        let norm = normalize(path);
        match Map.get(norm, o->times) {
            case Some(ts) => Ok(ts#ctime)
            case None     => notFound(path)
        }

    pub def modificationTime(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Int64] \ r =
        let norm = normalize(path);
        match Map.get(norm, o->times) {
            case Some(ts) => Ok(ts#mtime)
            case None     => notFound(path)
        }

    pub def size(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Fs.Size] \ r =
        use Fs.Size.bytes;
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(Entry.TextEntry(s))   => Ok(Fs.FsLayer.estimateStringBytes(s))
            case Some(Entry.BinaryEntry(b)) => Ok(bytes(Int32.toInt64(Vector.length(b))))
            case None                       => notFound(path)
        }

    // ─── Read ops ────────────────────────────────────────────────────

    pub def read(path: String, o: Fs.InMemoryFs[r]): Result[IoError, String] \ r + Clock =
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(Entry.TextEntry(s))   => touchAccess(norm, o); Ok(s)
            case Some(Entry.BinaryEntry(b)) => touchAccess(norm, o); Ok(String.fromBytes(b))
            case None                       => notFound(path)
        }

    pub def readLines(path: String, o: Fs.InMemoryFs[r]): Result[IoError, List[String]] \ r + Clock =
        match read(path, o) {
            case Ok(s)  => Ok(String.lines(s))
            case Err(e) => Err(e)
        }

    pub def readBytes(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Vector[Int8]] \ r + Clock =
        let norm = normalize(path);
        match Map.get(norm, o->files) {
            case Some(Entry.BinaryEntry(b)) => touchAccess(norm, o); Ok(b)
            case Some(Entry.TextEntry(s))   => touchAccess(norm, o); Ok(String.toBytes(s))
            case None                       => notFound(path)
        }

    // ─── Write ops ───────────────────────────────────────────────────

    pub def write(path: String, data: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        let existed = Map.memberOf(norm, o->times);
        o->files = Map.insert(norm, Entry.TextEntry(data), o->files);
        if (existed) touchModify(norm, o) else touchNew(norm, o);
        Ok(())

    pub def writeLines(path: String, lines: List[String], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        write(path, String.intercalate(String.lineSeparator(), lines), o)

    pub def writeBytes(path: String, data: Vector[Int8], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        let existed = Map.memberOf(norm, o->times);
        o->files = Map.insert(norm, Entry.BinaryEntry(data), o->files);
        if (existed) touchModify(norm, o) else touchNew(norm, o);
        Ok(())

    // ─── Append ops ──────────────────────────────────────────────────

    pub def append(path: String, data: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        let current = match Map.get(norm, o->files) {
            case Some(Entry.TextEntry(s))   => s
            case Some(Entry.BinaryEntry(b)) => String.fromBytes(b)
            case None                       => ""
        };
        write(path, current + data, o)

    pub def appendLines(path: String, lines: List[String], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let suffix = String.intercalate(String.lineSeparator(), lines);
        append(path, suffix, o)

    pub def appendBytes(path: String, data: Vector[Int8], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        let current = match Map.get(norm, o->files) {
            case Some(Entry.BinaryEntry(b)) => b
            case Some(Entry.TextEntry(s))   => String.toBytes(s)
            case None                       => Vector.empty()
        };
        writeBytes(path, Vector.append(current, data), o)

    // ─── Other write ops ─────────────────────────────────────────────

    pub def truncate(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        if (hasFile(path, o)) write(path, "", o)
        else notFound(path)

    pub def copy(src: String, dst: String, opts: Set[Fs.CopyOption], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let normSrc = normalize(src);
        let normDst = normalize(dst);
        match Map.get(normSrc, o->files) {
            case Some(e) =>
                if (Map.memberOf(normDst, o->files) and not Set.memberOf(Fs.CopyOption.ReplaceExisting, opts))
                    alreadyExists(dst)
                else {
                    o->files = Map.insert(normDst, e, o->files);
                    if (Set.memberOf(Fs.CopyOption.CopyAttributes, opts))
                        match Map.get(normSrc, o->times) {
                            case Some(ts) => o->times = Map.insert(normDst, ts, o->times)
                            case None     => touchNew(normDst, o)
                        }
                    else
                        touchNew(normDst, o);
                    Ok(())
                }
            case None => notFound(src)
        }

    pub def move(src: String, dst: String, opts: Set[Fs.MoveOption], o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r =
        let normSrc = normalize(src);
        let normDst = normalize(dst);
        match Map.get(normSrc, o->files) {
            case Some(e) =>
                if (Map.memberOf(normDst, o->files) and not Set.memberOf(Fs.MoveOption.ReplaceExisting, opts))
                    alreadyExists(dst)
                else {
                    o->files = Map.insert(normDst, e, o->files);
                    o->files = Map.remove(normSrc, o->files);
                    o->dirs = Set.remove(normSrc, o->dirs);
                    match Map.get(normSrc, o->times) {
                        case Some(ts) => o->times = Map.insert(normDst, ts, o->times)
                        case None     => ()
                    };
                    o->times = Map.remove(normSrc, o->times);
                    Ok(())
                }
            case None => notFound(src)
        }

    pub def delete(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r =
        let norm = normalize(path);
        if (hasEntry(path, o)) {
            o->files = Map.remove(norm, o->files);
            o->dirs = Set.remove(norm, o->dirs);
            o->times = Map.remove(norm, o->times);
            Ok(())
        } else notFound(path)

    pub def mkDir(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        if (hasEntry(path, o)) alreadyExists(path)
        else {
            o->dirs = Set.insert(norm, o->dirs);
            touchNew(norm, o);
            Ok(())
        }

    pub def mkDirs(path: String, o: Fs.InMemoryFs[r]): Result[IoError, Unit] \ r + Clock =
        let norm = normalize(path);
        let alreadyExisted = Set.memberOf(norm, o->dirs);
        o->dirs = Set.insert(norm, o->dirs);
        if (not alreadyExisted) touchNew(norm, o) else ();
        match parentDir(norm) {
            case Some(parent) =>
                if (not Set.memberOf(parent, o->dirs)) mkDirs(parent, o) else Ok(())
            case None => Ok(())
        }

    pub def mkTempDir(prefix: String, o: Fs.InMemoryFs[r]): Result[IoError, String] \ r + Clock =
        let n = o->counter;
        o->counter = n + 1;
        let name = "${prefix}inmemory-${n}";
        o->dirs = Set.insert(name, o->dirs);
        touchNew(name, o);
        Ok(name)

    // ─── Listing ops ─────────────────────────────────────────────────

    pub def list(dir: String, o: Fs.InMemoryFs[r]): Result[IoError, List[String]] \ r =
        let norm = normalize(dir);
        if (not hasDir(dir, o)) notFound(dir)
        else {
            let fileNames = Map.keysOf(o->files)
                |> Set.filter(p -> parentDir(p) == Some(norm))
                |> Set.map(p -> fileName(p));
            let dirNames = o->dirs
                |> Set.filter(p -> parentDir(p) == Some(norm))
                |> Set.map(p -> fileName(p));
            Ok(Set.toList(Set.union(fileNames, dirNames)))
        }

    pub def glob(base: String, pattern: String, o: Fs.InMemoryFs[r]): Result[IoError, List[String]] \ r =
        if (not hasDir(base, o)) notFound(base)
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
            let matched = Set.union(
                Map.keysOf(o->files) |> Set.filter(matchesPattern),
                o->dirs              |> Set.filter(matchesPattern)
            );
            Ok(Set.toList(matched))
        }

}
