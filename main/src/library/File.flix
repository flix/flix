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

    ///
    /// Returns `true` if the given file `file` exists.
    ///
    pub def exists(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.exists();
            Ok(exists(newFile(file)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `file` is a directory.
    ///
    pub def isDirectory(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.isDirectory();
            Ok(isDirectory(newFile(file)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `file` is a regular file.
    ///
    pub def isRegularFile(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:isRegularFile(##java.nio.file.Path, Array[##java.nio.file.LinkOption]);
            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let isRegular = isRegularFile(javaPath, []);
            Ok(isRegular)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:isSymbolicLink(##java.nio.file.Path);
            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let isSymbolic = javaPath.isSymbolicLink();
            Ok(isSymbolic)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canRead();
            Ok(canRead(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `file` is writable.
    ///
    pub def isWritable(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canWrite();
            Ok(canWrite(newFile(file)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `file` is executable.
    ///
    pub def isExecutable(file: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canExecute();
            Ok(canExecute(newFile(file)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
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
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
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

    ///
    /// Returns a string containing the given file `f`.
    ///
    pub def read(file: String): Result[String, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let reader = javaPath.newBufferedReader();
            Ok(readHelper(readAll(reader)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Helper function for read.
    /// Converts a list of strings to a string
    ///
    def readHelper(list: List[String]): String & Impure =
        List.foldLeft((x, y) -> x + "\n" + y, "", list)

    ///
    /// Returns an iterator of the given `file`
    ///
    pub def readLinesIter(file: String): Iterator[Result[String, String]] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
            import java.io.BufferedReader.readLine();

            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let reader = javaPath.newBufferedReader();
            let line = ref reader.readLine();

            let done = () -> Object.isNull(deref line);
            let next = () -> {
                let l = deref line;
                line := reader.readLine();
                Ok(l)
            };
            Iterator(done, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                let err = ref getMessage(ex);
                let done = () -> Object.isNull(deref err);
                let next = () -> {
                    Err(deref err)
                };
                Iterator(done, next)
        }

    ///
    /// Returns an array of all the bytes in the given file `file`.
    ///
    pub def readBytes(file: String): Result[Array[Int8], String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:readAllBytes(##java.nio.file.Path);

            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let bytes = readAllBytes(javaPath);

            Ok(bytes)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns an iterator of the bytes in the given `file` in chunks of size `chunkSize`.
    ///
    pub def readChunks(file: String, chunkSize: Int32): Iterator[Result[Array[Int8], String]] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:readAllBytes(##java.nio.file.Path);

            let javaFile = newFile(file);
            let javaPath = javaFile.toPath();
            let bytes = readAllBytes(javaPath);
            let byteCount = Array.length(bytes);
            let i = ref 0;

            let done = () -> deref i >= byteCount;
            let next = () -> {
                let j = deref i;
                if(j < byteCount) {
                    let end = {
                        if(j+chunkSize >= byteCount) byteCount-1
                        else j+chunkSize-1
                    };
                    let byteOfChuckSize = Array.slice(j, end, bytes);
                    i := j+chunkSize;
                    Ok(byteOfChuckSize)
                } else bug!("Empty Iterator!")
            };
            Iterator(done, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                let err = ref getMessage(ex);
                let done = () -> Object.isNull(deref err);
                let next = () -> {
                    Err(deref err)
                };
                Iterator(done, next)
        }

    ///
    /// Writes `data` to the given `file`.
    ///
    /// Creates `file` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file was created.
    ///
    pub def write(file: String, data: t): Result[Bool, String] & Impure with ToString[t] =
        try {
            import new java.io.File(String) as newFile;
            import new java.io.PrintWriter(##java.io.File) as newPrintWriter;
            import java.io.PrintWriter.println(String);
            import java.io.PrintWriter.close();
            import java.io.PrintWriter.checkError();

            let javaFile = newFile(file);
            let w = newPrintWriter(javaFile);

            w.println(ToString.toString(data));
            w.close();

            Ok(w.checkError())

        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

}
