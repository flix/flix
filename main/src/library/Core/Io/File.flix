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

use Core/Io/IOError.IOError;
use Core/Io/InputStream.InputStream;
use Core/Io/OutputStream.OutputStream;
use Core/Io/File.File;

instance Eq[File] {

    def eq(x: File, y: File): Bool = {
        import java.io.File.equals(##java.lang.Object);
        let File(xJ) = x;
        let File(yJ) = y;
        xJ.equals(yJ)
    }

    def neq(x: File, y: File): Bool = not (x == y)

}

namespace Core/Io/File {
    ///
    /// Represents a File.
    ///
    pub opaque type File = ##java.io.File

    ///
    /// Returns a file from the given string `s`.
    ///
    pub def new(s: String): File & Impure =
        import new java.io.File(String) as newFile;
        File(newFile(s))

    ///
    /// Combines every string in `n` into a single path
    ///
    pub def newFromFragments(n: Nel[String]): File & Impure = {
        match n {
            case Nel(base, xs) => List.fold((p, s) -> p.resolve(s), new(base), xs)
        }
    }

    ///
    /// Appends the string `s` to the end of the path `p`
    ///
    pub def resolve(f: File, s: String): File & Impure = {
        import new java.io.File(##java.io.File, String) as newFile;
        let File(baseFile) = f;
        File(newFile(baseFile, s))
    }

    ///
    /// Creates all directories in the path `f` if they don't already exist
    ///
    pub def mkdirs(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.mkdirs();
        let File(javaFile) = f;
        javaFile.mkdirs()
    })

    ///
    /// Gets the canonical form of the `f` file
    ///
    pub def canonical(f: File): Result[File, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.getCanonicalFile();
        let File(javaFile) = f;
        File(javaFile.getCanonicalFile())
    })

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.exists();
        let File(javaFile) = f;
        javaFile.exists()
    })

    ///
    /// Returns the name of the directory or file denoted by the given file `f`.
    ///
    pub def getName(f: File): Result[String, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.getName();
        let File(javaFile) = f;
        javaFile.getName()
     })

    ///
    /// Optionally returns the file to the parent of the given file `f`.
    ///
    /// Returns `None` if `f` has no parent.
    ///
    pub def getParent(f: File): Result[Option[File], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.getParentFile();
        let File(javaFile) = f;
        let parent = javaFile.getParentFile();
        if (Object.isNull(parent))
            None
        else
            Some(File(parent))
    })

    ///
    /// Returns a list of children for the given file `f`.
    ///
    /// Returns `None` if the file `f` does not denote a directory.
    ///
    pub def getChildren(f: File): Result[Option[List[File]], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.listFiles();
        let File(javaFile) = f;
        let children = javaFile.listFiles();
        if (Object.isNull(children))
            None
        else
            Some(children |> Array.toList |> List.map(File))
    })

    ///
    /// Returns `true` if the given file `f` is a directory.
    ///
    pub def isDirectory(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.isDirectory();
        let File(javaFile) = f;
        javaFile.isDirectory()
    })

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canExecute();
        let File(javaFile) = f;
        javaFile.canExecute()
    })

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canRead();
        let File(javaFile) = f;
        javaFile.canRead()
    })

    ///
    /// Returns `true` if the given file `f` is a file.
    ///
    pub def isFile(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.isFile();
        let File(javaFile) = f;
        javaFile.isFile()
    })

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.toPath();
        import java.nio.file.Files:isSymbolicLink(##java.nio.file.Path);
        let File(javaFile) = f;
        let javaPath = javaFile.toPath();
        javaPath.isSymbolicLink()
    })

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: File): Result[Bool, IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.canWrite();
        let File(javaFile) = f;
        javaFile.canWrite()
    })

    ///
    /// Opens the specified `File` into an `InputStream`
    ///
    pub def newInputStream(f: File): Result[InputStream, IOError] & Impure = try {
        import new java.io.FileInputStream(##java.io.File) as newFileInputStream;
        let File(javaFile) = f;
        let javaInputStream = newFileInputStream(javaFile);
        let inputStream = InputStream.InputStream(javaInputStream as ##java.io.InputStream);
        Ok(inputStream)
    } catch {
        case _: ##java.io.FileNotFoundException => Err(IOError.FileNotFound(f))
    }

    ///
    /// Returns a list of all the lines in the given file `f`.
    ///
    pub def readLines(f: File): Result[List[String], IOError] & Impure = tryWithCatch(_ -> {
        import java.io.File.toPath();
        import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
        let File(javaFile) = f;
        let javaPath = javaFile.toPath();
        let reader = javaPath.newBufferedReader();
        readAll(reader)
    })

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: ##java.io.BufferedReader): List[String] & Impure = List.reverse(readAllHelper(r, Nil))

    ///
    /// Helper function for readAll.
    /// Reads all lines from the buffered reader `r` until it is empty, returning a backward list.
    ///
    def readAllHelper(r: ##java.io.BufferedReader, acc: List[String]): List[String] & Impure =
        import java.io.BufferedReader.readLine();
        let line = r.readLine();
        if (Object.isNull(line))
            acc
        else
            readAllHelper(r, line :: acc)

    ///
    /// Writes all lines in the list `l` to the file `f`.
    ///
    pub def writeLines(f: File, l: List[String]): Result[Unit, IOError] & Impure = tryWithCatch(_ -> {
        import new java.io.PrintWriter(##java.io.File) as newPrintWriter;
        import java.io.PrintWriter.close();
        let File(javaFile) = f;
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
    /// Opens the specified `File` into an `OutputStream`
    ///
    pub def newOutputStream(f: File): Result[OutputStream, IOError] & Impure = try {
        import new java.io.FileOutputStream(##java.io.File) as newFileOutputStream;
        let File(javaFile) = f;
        let outputStream = newFileOutputStream(javaFile);
        let stream = OutputStream.OutputStream(outputStream as ##java.io.OutputStream);
        Ok(stream)
    } catch {
        case _: ##java.io.FileNotFoundException => Err(IOError.FileNotFound(f))
    }

    ///
    /// Evaluates the given function `f` trapping any exception and returning a Result.
    ///
    def tryWithCatch[a, e](f: Unit -> a & e): Result[a, IOError] & e = try {
        let res = f();
        Ok(res)
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in File"))
    }
}
