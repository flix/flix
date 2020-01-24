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
    pub def new(s: String): Path =
        import java.io.File:__new__(String) as newFile;
        Path(newFile(s))

    ///
    /// Returns `true` if the given path `p` exists.
    ///
    pub def exists(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        import java.io.File.exists();
        let Path(javaFile) = p;
        javaFile.exists()
    })

    ///
    /// Returns the name of the directory or file denoted by the given path `p`.
    ///
    pub def getName(p: Path): Result[Str, IOError] = tryWithCatch(_ -> {
        import java.io.File.getName();
        let Path(javaFile) = p;
        javaFile.getName()
     })

    ///
    /// Optionally returns the path to the parent of the given path `p`.
    ///
    /// Returns `None` if `p` has no parent.
    ///
    pub def getParent(p: Path): Result[Option[Path], IOError] = tryWithCatch(_ -> {
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
    pub def getChildren(p: Path): Result[Option[List[Path]], IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        let children = native method java.io.File.listFiles(javaFile);
        if (isNull(children))
            None
        else
            Some(children |> Array.toList |> List.map(p -> Path(p)))
    })

    ///
    /// Returns `true` if the given path `p` is a directory.
    ///
    pub def isDirectory(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        import java.io.File.isDirectory();
        let Path(javaFile) = p;
        javaFile.isDirectory()
    })

    ///
    /// Returns `true` if the given path `p` is executable.
    ///
    pub def isExecutable(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        native method java.io.File.canExecute(javaFile) as Bool
    })

    ///
    /// Returns `true` if the given path `p` is readable.
    ///
    pub def isReadable(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        native method java.io.File.canRead(javaFile) as Bool
    })

    ///
    /// Returns `true` if the given path `p` is a file.
    ///
    pub def isFile(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        import java.io.File.isFile();
        let Path(javaFile) = p;
        javaFile.isFile()
    })

    ///
    /// Returns `true` if the given path `p` is a symbolic link.
    ///
    pub def isSymbolicLink(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        let javaPath = native method java.io.File.toPath(javaFile);
        native method java.nio.file.Files.isSymbolicLink(javaPath) as Bool
    })

    ///
    /// Returns `true` if the given path `p` is writable.
    ///
    pub def isWritable(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        import java.io.File.canWrite();
        let Path(javaFile) = p;
        javaFile.canWrite()
    })

    ///
    /// Returns a list of all the lines in the given path `p`.
    ///
    pub def readLines(p: Path): Result[List[Str], IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        let javaPath = native method java.io.File.toPath(javaFile);
        let reader = native method java.nio.file.Files.newBufferedReader(javaPath);
        readAll(reader)
    })

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: ##java.io.BufferedReader): List[Str] =
        let line = native method java.io.BufferedReader.readLine(r);
        if (isNull(line))
            Nil
        else
            line :: readAll(r)

    ///
    /// Writes all lines in the list `l` to the path `p`.
    ///
    pub def writeLines(p: Path, l: List[Str]): Result[Unit, IOError] = tryWithCatch(_ -> {
        import java.io.PrintWriter:__new__(##java.io.File) as newPrintWriter;
        let Path(javaFile) = p;
        let w = newPrintWriter(javaFile);
        writeAll(l, w);
        native method java.io.PrintWriter.close(w) as Unit;
        ()
    })

    ///
    /// Writes all lines in `l` to the writer `w`.
    ///
    def writeAll(l: List[Str], w: ##java.io.PrintWriter): Unit =
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
    def tryWithCatch[a](f: Unit -> a): Result[a, IOError] = try {
        let res = f();
        Ok(res)
    } catch {
        case _: ##java.io.IOException => Err(Generic("I/O Error"))
    }

    ///
    /// Represents an input/output error.
    ///
    pub enum IOError {
        case Generic(Str)
    }
}

///
/// Represents a path.
///
opaque type Path = ##java.io.File
