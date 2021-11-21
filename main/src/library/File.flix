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
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.exists();
            Ok(exists(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    pub def isDirectory(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.isDirectory();
            Ok(isDirectory(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    pub def isRegularFile(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:isRegularFile(##java.nio.file.Path, Array[##java.nio.file.LinkOption]);
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let isRegular = isRegularFile(javaPath, []);
            Ok(isRegular)
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
            case _: ##java.io.IOException => Err("I/O Error")
        }

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:isSymbolicLink(##java.nio.file.Path);
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let isSymbolic = isSymbolicLink(javaPath);
            Ok(isSymbolic)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canWrite();
            Ok(canWrite(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: String): Result[Bool, String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.canExecute();
            Ok(canExecute(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }

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
    /// TODO: OBS I changed offset to Int32.
    ///
    pub def readWith(opts: {offset :: Int32, count :: Int32, charSet :: String}, f : String): Result[String, String] & Impure = {
        try {
            import new java.io.File(String) as newFile;
            import new java.io.FileInputStream(##java.io.File) as newInputStream;
            import java.io.FileInputStream.read(Array[Int8], Int32, Int32);
            import java.nio.charset.Charset:forName(String);
            import new java.lang.String(Array[Int8], ##java.nio.charset.Charset) as newString;

            let javaFile = newFile(f);
            let stream = newInputStream(javaFile);
            let bytes = [];
            read(stream, bytes, opts.offset, opts.count);
            let charSet = forName(opts.charSet);
            let returnString = newString(bytes, charSet);
            Ok(returnString)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
        }
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
        let line = readLine(r);
        if (Object.isNull(line))
            acc
        else
            readAllHelper(r, line :: acc)

    ///
    /// Returns a list of all the lines in the given file `f` with the options `opts`.
    /// TODO: specify the opts.
    ///
    pub def readLinesWith(opts: {charSet :: String}, f: String): Result[List[String], String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset);
            import java.nio.charset.Charset:forName(String);

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts.charSet));
            Ok(readAll(reader))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(ex))
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
    /// Returns an iterator of the given file `f` with the options `opts`.
    /// TODO: specify opts.
    ///
    pub def readLinesIterWith(opts: {charSet :: String}, f: String): Iterator[Result[String, String]] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import java.io.File.toPath();
            import java.nio.file.Files:newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset);
            import java.nio.charset.Charset:forName(String);
            import java.io.BufferedReader.readLine();

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts.charSet));
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
    /// Returns an array of all the bytes in the given file `f` and applying the options `opts`.
    /// TODO: add a note on what exactly the opts do.
    /// TODO: OBS I changed offset to Int32.
    ///
    pub def readBytesWith(opts: {offset :: Int32, count :: Int32}, f: String): Result[Array[Int8], String] & Impure =
        try {
            import new java.io.File(String) as newFile;
            import new java.io.FileInputStream(##java.io.File) as newInputStream;
            import java.io.FileInputStream.read(Array[Int8], Int32, Int32);

            let javaFile = newFile(f);
            let stream = newInputStream(javaFile);
            let bytes = [];
            read(stream, bytes, opts.offset, opts.count);
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

}
