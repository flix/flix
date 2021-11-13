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
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
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
        let line = readLine(r);
        if (Object.isNull(line))
            acc
        else
            readAllHelper(r, line :: acc)

    ///
    /// Returns a string containing the given file `f`.
    ///
    pub def read(f: String): Result[String, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:readString(##java.nio.file.Path);
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let result = readString(javaPath);
            Ok(result)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns a string containing the given file `f` with the options `opts`.
    /// TODO: add a note on what exactly the opts do
    ///
    pub def readWith(opts: {offset :: Int64, count :: Int32, charSet :: String}, f : String): Result[String, String] & Impure = {
        try {
            import new java.io.File(String) as newFile;
            import java.nio.charset.Charset:forName(String);
            import java.io.File.toPath();
            import java.nio.file.Files:readString(##java.nio.file.Path, ##java.nio.charset.Charset);
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let fileRead = readString(javaPath, forName(opts.charSet));
            // TODO: take care of offset before loading the whole file in memory
            Ok(readWithHelper(fileRead, opts.offset, opts.count))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }
    }

    ///
    /// Helper function for readWith.
    /// Takes a String and applys the options.
    ///
    def readWithHelper(f: String, offset: Int64, count: Int32): String & Pure =
        let fileLength = String.length(f);
        match Int64.tryToInt32(offset) {
            case None => String.dropRight(fileLength - count, f)
            case Some(n) =>
                let str = String.dropLeft(n, f);
                let strLength = String.length(str);
                String.dropRight(strLength - count, str)
        }

    ///
    /// Returns an iterator of the given file `f`
    ///
    pub def readLinesIter(f: String): Iterator[Result[String, String]] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);
            import java.io.BufferedReader.readLine();

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            let line = ref readLine(reader);

            let done = () -> Object.isNull(deref line);
            let next = () -> {
                let l = deref line;
                line := readLine(reader);
                Ok(l)
            };
            Iterator(done, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Iterator.singleton(Err(getMessage(ex)))
        }

    ///
    /// Returns an array of all the bytes in the given file `f`.
    ///
    pub def readBytes(f: String): Result[Array[Int8], String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:readAllBytes(##java.nio.file.Path);

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
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
    pub def readChunks(f: String, chunkSize: Int32): Iterator[Result[Array[Int8], String]] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path);

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            let line = ref readChunk(reader, chunkSize);

            let done = () -> {
                let l = deref line;
                Array.get(Array.length(l) - 1, l) == -1i8
            };
            let next = () -> {
                let l = deref line;
                line := readChunk(reader, chunkSize);
                Ok(l)
            };
            Iterator(done, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Iterator.singleton(Err(getMessage(ex)))
        }

    ///
    /// Reads bytes in chunks of size `chunkSize` from the given buffered reader `r`.
    ///
    def readChunk(r: ##java.io.BufferedReader, chunkSize: Int32): Array[Int8] & Impure =
        List.toArray(List.reverse(readChunkHelper(r, chunkSize, Nil)))

    ///
    /// Helper function for readchunk.
    /// Reads all lines from the buffered reader `r` of `chunksize`, returning a backward list.
    ///
    def readChunkHelper(r: ##java.io.BufferedReader, chunkSize: Int32, acc: List[Int8]): List[Int8] & Impure =
        import java.io.BufferedReader.read();
        if (chunkSize == 0)
            acc
        else
            match Int32.tryToInt8(read(r)) {
                case None       => ??? // TODO: throw exception or something?
                case Some(n)    => readChunkHelper(r, chunkSize - 1, n :: acc)
            }

    ///
    /// Writes `data` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file was created.
    ///
    pub def write(f: String, data: t): Result[Bool, String] & Impure with ToString[t] =
        try {
            import new java.io.File(String) as newFile;
            import new java.io.PrintWriter(##java.io.File) as newPrintWriter;
            import java.io.PrintWriter.println(String);
            import java.io.PrintWriter.close();
            import java.io.PrintWriter.checkError();

            let javaFile = newFile(f);
            let w = newPrintWriter(javaFile);

            println(w, "${data}");
            close(w);

            Ok(checkError(w))

        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

}
