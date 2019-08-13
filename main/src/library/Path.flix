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
    pub def new(s: Str): Path =
        let f = native new java.io.File(s : Str);
        Path(f)

    ///
    /// Returns `true` if the given path `p` exists.
    ///
    pub def exists(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        native method java.io.File.exists(javaFile) as Bool
    })

    ///
    /// Returns `true` if the given path `p` exists and is a directory.
    ///
    pub def isDirectory(p: Path): Result[Bool, IOError] = tryWithCatch(_ -> {
        let Path(javaFile) = p;
        native method java.io.File.isDirectory(javaFile) as Bool
    })

//    static boolean 	isExecutable​(Path path)
//    Tests whether a file is executable.
//    static boolean 	isHidden​(Path path)
//    Tells whether or not a file is considered hidden.
//    static boolean 	isReadable​(Path path)
//    Tests whether a file is readable.
//    static boolean 	isRegularFile​(Path path, LinkOption... options)
//    Tests whether a file is a regular file with opaque content.
//    static boolean 	isSameFile​(Path path, Path path2)
//    Tests if two paths locate the same file.
//    static boolean 	isSymbolicLink​(Path path)
//    Tests whether a file is a symbolic link.
//    static boolean 	isWritable​(Path path)
//
//static BufferedReader 	newBufferedReader​(Path path, Charset cs)
//Opens a file for reading, returning a BufferedReader that may be used to read text from the file in an efficient manner.
//static BufferedWriter 	newBufferedWriter​(Path path, Charset cs, OpenOption... options)
//Opens or creates a file for writing, returning a BufferedWriter that may be used to write text to the file in an efficient manner.
//static BufferedWriter 	newBufferedWriter​(Path path, OpenOption... options)

// static String 	readString​(Path path)
   // Reads all content from a file into a string, decoding from bytes to characters using the UTF-8 charset.

   //static String 	readString​(Path path, Charset cs)
   //Reads all characters from a file into a string, decoding from bytes to characters using the specified charset.




    ///
    /// Returns a list of all the lines in the given path `p`.
    ///
    pub def readLines(p: Path): List[Str] =
        let Path(javaFile) = p;
        let javaPath = native method java.io.File.toPath(javaFile);
        let reader = native method java.nio.file.Files.newBufferedReader(javaPath);
        readAll(reader)

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
    /// Evaluates the given function `f` trapping any exception and returning a Result.
    ///
    def tryWithCatch[a](f: Unit -> a): Result[a, IOError] = try {
        Ok(f())
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
type Path = Path(##java.io.File)
