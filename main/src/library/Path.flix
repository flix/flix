/*
 * Copyright 2019 Magnus Madsen
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
namespace Path {

    ///
    /// Returns a path from the given string `s`.
    ///
    pub def new(s: String): Path & Impure =
        import new java.io.File(String) as newFile;
        Path(newFile(s))

    ///
    /// Returns `true` if the given path `p` exists.
    ///
    pub def exists(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.exists();
        let Path(javaFile) = p;
        javaFile.exists()
    })

    ///
    /// Returns the name of the directory or file denoted by the given path `p`.
    ///
    pub def getName(p: Path): Result[String, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.getName();
        let Path(javaFile) = p;
        javaFile.getName()
     })

    ///
    /// Optionally returns the path to the parent of the given path `p`.
    ///
    /// Returns `None` if `p` has no parent.
    ///
    pub def getParent(p: Path): Result[Option[Path], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.getParentFile();
        let Path(javaFile) = p;
        let parent = javaFile.getParentFile();
        if (isNull(parent))
            None
        else
            Some(Path(parent))
    })

    ///
    /// Returns a list of children for the given path `p`.
    ///
    /// Returns `None` if the path `p` does not denote a directory.
    ///
    pub def getChildren(p: Path): Result[Option[List[Path]], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.listFiles();
        let Path(javaFile) = p;
        let children = javaFile.listFiles();
        if (isNull(children))
            None
        else
            Some(children |> Array.toList |> List.map(p -> Path(p)))
    })

    ///
    /// Returns `true` if the given path `p` is a directory.
    ///
    pub def isDirectory(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.isDirectory();
        let Path(javaFile) = p;
        javaFile.isDirectory()
    })

    ///
    /// Returns `true` if the given path `p` is executable.
    ///
    pub def isExecutable(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canExecute();
        let Path(javaFile) = p;
        javaFile.canExecute()
    })

    ///
    /// Returns `true` if the given path `p` is readable.
    ///
    pub def isReadable(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canRead();
        let Path(javaFile) = p;
        javaFile.canRead()
    })

    ///
    /// Returns `true` if the given path `p` is a file.
    ///
    pub def isFile(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.isFile();
        let Path(javaFile) = p;
        javaFile.isFile()
    })

    ///
    /// Returns `true` if the given path `p` is a symbolic link.
    ///
    pub def isSymbolicLink(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.toPath();
        import java.nio.file.Files:isSymbolicLink(##java.nio.file.Path);
        let Path(javaFile) = p;
        let javaPath = javaFile.toPath();
        javaPath.isSymbolicLink()
    })

    ///
    /// Returns `true` if the given path `p` is writable.
    ///
    pub def isWritable(p: Path): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canWrite();
        let Path(javaFile) = p;
        javaFile.canWrite()
    })

    ///
    /// Returns a list of all the lines in the given path `p`.
    ///
    pub def readLines(p: Path): Result[List[String], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.toPath();
        import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
        let Path(javaFile) = p;
        let javaPath = javaFile.toPath();
        let reader = javaPath.newBufferedReader();
        readAll(reader)
    })

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: ##java.io.BufferedReader): List[String] & Impure =
        import java.io.BufferedReader.readLine();
        let line = r.readLine();
        if (isNull(line))
            Nil
        else
            line :: readAll(r)

    ///
    /// Writes all lines in the list `l` to the path `p`.
    ///
    pub def writeLines(p: Path, l: List[String]): Result[Unit, IOError] & Impure = tryWithCatch(_ -> {
        import new java.io.PrintWriter(##java.io.File) as newPrintWriter;
        import java.io.PrintWriter.close();
        let Path(javaFile) = p;
        let w = newPrintWriter(javaFile);
        writeAll(l, w);
        w.close();
        ()
    })

    ///
    /// Writes all lines in `l` to the writer `w`.
    ///
    // TODO: What about exceptions?
    def writeAll(l: List[String], w: ##java.io.PrintWriter): Unit & Impure =
        import java.io.PrintWriter.println(String);
        match l {
            case Nil => ()
            case x :: xs =>
                println(w, x);
                writeAll(xs, w)
        }

    ///
    /// Evaluates the given function `f` trapping any exception and returning a Result.
    ///
    def tryWithCatch[a, e](f: Unit -> a & e): Result[a, IOError] & e = try {
        let res = f();
        Ok(res)
    } catch {
        case _: ##java.io.IOException => Err(Generic("I/O Error"))
    }

    ///
    /// Represents an input/output error.
    ///
    pub enum IOError {
        case Generic(String)
    }
}

///
/// Represents a path.
///
opaque type Path = ##java.io.File
