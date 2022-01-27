/*
 * Copyright 2021 Magnus Madsen
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
namespace File {

    enum FileType with Eq, Order, ToString {
        case File,
        case Directory,
        case SymLink,
        case Other
    }

    ///
    /// Returns the access time of the given file `f` in milliseconds.
    ///
    pub def accessTime(f: String): Result[Int64, String] = ???

    ///
    /// Returns the creation time of the given file `f` in milliseconds.
    ///
    pub def creationTime(f: String): Result[Int64, String] = ???

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds
    ///
    pub def modificationTime(f: String): Result[Int64, String] = ???

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    pub def size(f: String): Result[Int64, String] = ???

    ///
    /// Returns the type of the given file `f`.
    ///
    pub def fileType(f: String): Result[FileType, String] = ???

    type alias StatInfo = {
        accessTime :: Int64,
        creationTime :: Int64,
        modificationTime :: Int64,
        size :: Int64,
        fileType :: FileType
    }

    ///
    /// Returns the statistics of the given file `f`.
    ///
    pub def stat(f: String): Result[StatInfo, String] = ???

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canRead();
            Ok(canRead(newFile(f)))
        } catch {
            case _: ##java.io.IOException => Err("I/O Error")
        }

    ///
    /// Returns a list of all the lines in the given file `f`.
    ///
    pub def readLines(f: String): Result[List[String], String] & Impure = try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
            let javaFile = newFile(f);
            let javaPath = javaFile.toPath();
            let reader = javaPath.newBufferedReader();
            Ok(readAll(reader))
        } catch {
            case _: ##java.io.IOException => Err("I/O Error")
        }

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

}
