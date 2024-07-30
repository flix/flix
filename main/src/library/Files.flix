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
mod Files {

    use IOError.{Generic}
    import java.io.{BufferedReader, File, FileInputStream, FileOutputStream, FileWriter, IOException, OutputStream, PrintWriter, Writer}
    import java.lang.{Class, String => JString}
    import java.nio.charset.Charset
    import java.nio.file.attribute.{FileTime, BasicFileAttributes}
    import java.nio.file.{CopyOption, DirectoryStream, FileVisitOption, LinkOption, Path, StandardCopyOption}
    import java.util.{Iterator => JIterator}
    import java.util.stream.Stream

    ///
    /// File types.
    ///
    pub enum FileType with Eq, Order, ToString {
        case File,
        case Directory,
        case SymLink,
        case Other
    }

    ///
    /// Returns the last access time of the given file `f` in milliseconds.
    ///
    pub def accessTime(f: String): Result[IOError, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.lastAccessTime(): FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> lastAccessTime |> toMillis
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Returns the creation time of the given file `f` in milliseconds.
    ///
    pub def creationTime(f: String): Result[IOError, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.creationTime(): FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> creationTime |> toMillis
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds
    ///
    pub def modificationTime(f: String): Result[IOError, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.lastModifiedTime(): FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> lastModifiedTime |> toMillis
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    pub def size(f: String): Result[IOError, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.size(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                size(fileAttributes)
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Returns the BasicFileAttributes of the file `f`.
    ///
    def getAttributes(f: String): Result[IOError, BasicFileAttributes] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.readAttributes(Path, Class, Array[LinkOption, rc]): BasicFileAttributes \ IO;
            import static java.lang.Class.forName(String): Class \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let classOfAttributes = forName("java.nio.file.attribute.BasicFileAttributes");

            readAttributes(javaPath, classOfAttributes, Array#{} @ rc)
        })
    }

    ///
    /// Returns the type of the given file `f`.
    ///
    pub def fileType(f: String): Result[IOError, FileType] \ IO =
        forM (
            isFile <- Files.isRegularFile(f);
            isDirec <- Files.isDirectory(f);
            isSymLink <- Files.isSymbolicLink(f)
        ) yield {
            if (isFile)         FileType.File
            else if (isDirec)   FileType.Directory
            else if (isSymLink) FileType.SymLink
            else                FileType.Other
        }

    ///
    /// Statistical information for a file.
    ///
    pub type alias StatInfo = {
        accessTime       = Int64,
        creationTime     = Int64,
        modificationTime = Int64,
        size             = Int64,
        fileType         = FileType
    }

    ///
    /// Returns the statistics of the given file `f`.
    ///
    pub def stat(f: String): Result[IOError, StatInfo] \ IO =
        forM (
            fileAccessTime <- accessTime(f);
            fileCreationTime <- creationTime(f);
            fileModificationTime <- modificationTime(f);
            fileSize <- size(f);
            typeOfFile <- fileType(f)
        ) yield
            {
                accessTime = fileAccessTime,
                creationTime = fileCreationTime,
                modificationTime = fileModificationTime,
                size = fileSize,
                fileType = typeOfFile
            }

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.exists(): Bool \ IO;
            exists(new File(f))
        })

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    pub def isDirectory(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.isDirectory(): Bool \ IO;
            isDirectory(new File(f))
        })

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    pub def isRegularFile(f: String): Result[IOError, Bool] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.isRegularFile(Path, Array[LinkOption, rc]): Bool \ IO;
            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            isRegularFile(javaPath, Array#{} @ rc)
        })
    }

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.canRead(): Bool \ IO;
            canRead(new File(f))
        })

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.isSymbolicLink(Path): Bool \ IO;
            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            isSymbolicLink(javaPath)
        })

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.canWrite(): Bool \ IO;
            canWrite(new File(f))
        })

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.canExecute(): Bool \ IO;
            canExecute(new File(f))
        })

    ///
    /// Returns a string containing the given file `f`.
    ///
    pub def read(f: String): Result[IOError, String] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.readString(Path): String \ IO;
            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let result = readString(javaPath);
            result
        })

    ///
    /// Returns a string containing the given file `f` with the options `opts`.
    /// The options `opts` to apply consists of
    /// `offset` - the start offset in the given file `f`.
    /// `count` - the number of bytes read.
    /// `charSet` - the specific charset to be used to decode the bytes.
    ///
    pub def readWith(opts: {offset = Int64, count = Int32, charSet = String}, f : String): Result[IOError, String] \ IO = region rc {
        if (0 < opts#count) {
            try {
                import java.io.FileInputStream.read(Array[Int8, rc], Int32, Int32): Int32 \ IO;
                import static java.nio.charset.Charset.forName(String): Charset \ IO;

                let javaFile = new File(f);
                let stream = new FileInputStream(javaFile);
                match skip((opts#offset / 10_000i64), opts#offset, stream, f) {
                    case Ok(_)    => {
                        let bytes = Array.empty(rc, opts#count);
                        discard read(stream, bytes, 0, opts#count);
                        let charSet = forName(opts#charSet);
                        let returnString = new JString(bytes, charSet);
                        Ok(returnString)
                    }
                    case Err(msg) => Err(msg)
                }
            } catch {
                case ex: IOException =>
                    import java.lang.Throwable.getMessage(): String \ IO;
                    Err(Generic(getMessage(checked_cast(ex))))
            }
        } else if (opts#count == 0) {
            Ok("")
        } else {
            Err(Generic("The argument `count` has to be a non negative number when calling `File.readWith`."))
        }
    }

    ///
    /// Helper function for `readWith` and `readBytesWith`.
    /// Tries to skip the requested number of bytes `bytes to skip` in `f` over `i + 1` iterations.
    ///
    def skip(i: Int64, bytesToSkip: Int64, stream: FileInputStream, f: String): Result[IOError, Unit] \ IO =
        try {
            if (0i64 <= i) {
                import java.io.FileInputStream.skip(Int64): Int64 \ IO as javaSkip;
                let skipped = javaSkip(stream, bytesToSkip);
                if (skipped < bytesToSkip)
                    skip(i - 1i64, bytesToSkip - skipped, stream, f)
                else
                    Ok()
            } else {
                Err(Generic("An error occurred when reading the file: '${f}'."))
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Returns a list of all the lines in the given file `f`.
    ///
    pub def readLines(f: String): Result[IOError, List[String]] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.newBufferedReader(Path): BufferedReader \ IO;
            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            readAll(reader)
        })

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: BufferedReader): List[String] \ IO =
        import java.io.BufferedReader.readLine(): String \ IO;
        def loop(k) = {
            let l = readLine(r);
            if (Object.isNull(l))
                k(Nil)
            else
                loop(ks -> k(l :: ks))
        };
        loop(identity)

    ///
    /// Returns a list of all the lines in the given file `f` with the options `opts`.
    /// The options `opts` to apply consists of
    /// `charSet` - the specific charset to be used to decode the bytes.
    ///
    pub def readLinesWith(opts: {charSet = String}, f: String): Result[IOError, List[String]] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.newBufferedReader(Path, Charset): BufferedReader \ IO;
            import static java.nio.charset.Charset.forName(String): Charset \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts#charSet));
            readAll(reader)
        })

    ///
    /// Returns an iterator of the given file `f`
    ///
    pub def readLinesIter(rc: Region[r], f: String): Iterator[Result[IOError, String], IO + r, r] \ { r, IO } =
        try {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.newBufferedReader(Path): BufferedReader \ IO;
            import java.io.BufferedReader.readLine(): String \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            let line = ref readLine(reader) @ rc;
            let next = () -> ({
                if (not Object.isNull(deref line)) {
                    try {
                        let l = deref line;
                        Ref.put(readLine(reader), line);
                        Some(Ok(l))
                    } catch {
                        case ex: IOException => {
                            Ref.put(checked_cast(null), line);
                            import java.lang.Throwable.getMessage(): String \ {};
                            Some(Err(Generic(getMessage(checked_cast(ex)))))
                        }
                    }
                } else {
                    None
                }
            });
            Iterator.iterate(rc, next)
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ {};
                Iterator.iterate(rc, constant(Some(Err(Generic(getMessage(checked_cast(ex)))))))
        }

    ///
    /// Returns an iterator of the given file `f` with the options `opts`.
    /// The options `opts` to apply consists of
    /// `charSet` - the specific charset to be used to decode the bytes.
    ///
    pub def readLinesIterWith(rc: Region[r], opts: {charSet = String}, f: String): Iterator[Result[IOError, String], IO + r, r] \ { r, IO } =
        try {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.newBufferedReader(Path, Charset): BufferedReader \ IO;
            import static java.nio.charset.Charset.forName(String): Charset \ IO;
            import java.io.BufferedReader.readLine(): String \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts#charSet));
            let line = ref readLine(reader) @ rc;
            let next = () -> ({
                if (not Object.isNull(deref line)) {
                    try {
                        let l = deref line;
                        Ref.put(readLine(reader), line);
                        Some(Ok(l))
                    } catch {
                        case ex: IOException => {
                            Ref.put(checked_cast(null), line);
                            import java.lang.Throwable.getMessage(): String \ {};
                            Some(Err(Generic(getMessage(checked_cast(ex)))))
                        }
                    }
                } else {
                    None
                }
            });
            Iterator.iterate(rc, next)
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ {};
                Iterator.iterate(rc, constant(Some(Err(Generic(getMessage(checked_cast(ex)))))))
        }

    ///
    /// Returns an array of all the bytes in the given file `f`.
    ///
    pub def readBytes(_rc: Region[r], f: String): Result[IOError, Array[Int8, r]] \ { r, IO } =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.readAllBytes(Path): Array[Int8, r] \ IO + r;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let bytes = readAllBytes(javaPath);

            bytes
        })

    ///
    /// Returns an array of all the bytes in the given file `f` and applying the options `opts`.
    /// The options `opts` to apply consists of
    /// `offset` - the start offset in the given file `f`.
    /// `count` - the number of bytes read.
    ///
    pub def readBytesWith(rc: Region[r], opts: {offset = Int64, count = Int32}, f: String): Result[IOError, Array[Int8, r]] \ { r, IO } =
        if (0 < opts#count) {
            try {
                import java.io.FileInputStream.read(Array[Int8, r], Int32, Int32): Int32 \ IO;

                let javaFile = new File(f);
                let stream = new FileInputStream(javaFile);
                match skip((opts#offset / 10_000i64), opts#offset, stream, f) {
                    case Ok(_)    => {
                        let bytes = Array.empty(rc, opts#count);
                        let numberRead = read(stream, bytes, 0, opts#count);
                        let res = readBytesWithHelper(rc, bytes, numberRead, opts#count);
                        Ok(res)
                    }
                    case Err(msg) => Err(msg)
                }
            } catch {
                case ex: IOException =>
                    import java.lang.Throwable.getMessage(): String \ IO;
                    Err(Generic(getMessage(checked_cast(ex))))
            }
        } else if (opts#count == 0) {
            Ok(Array#{} @ rc)
        } else {
            Err(Generic("The argument `count` has to be a non negative number when calling `File.readBytesWith`."))
        }

    ///
    /// Helper function for `readBytesWith`.
    /// Returns an array that corresponds to the bytes actually read from the stream.
    ///
    def readBytesWithHelper(rc: Region[r], bytes: Array[Int8, r], numberRead: Int32, count: Int32): Array[Int8, r] \ r =
        let nothingRead = (numberRead == -1);
        if (nothingRead) {
            Array#{} @ rc
        } else {
            let countBiggest = numberRead < count;
            if (countBiggest) {
                Array.slice(rc, start = 0, end = numberRead, bytes)
            } else {
                bytes
            }
        }

    ///
    /// Returns an iterator of the bytes in the given `file` in chunks of size `chunkSize`.
    ///
    pub def readChunks(rc: Region[r], chunkSize: Int32, f: String): Iterator[Result[IOError, Array[Int8, r]], IO + r, r] \ IO + r =
        if (0 < chunkSize) {
            try {
                import java.io.FileInputStream.read(Array[Int8, r]): Int32 \ IO + r;

                let javaFile = new File(f);
                let stream = new FileInputStream(javaFile);
                let bytes = ref Array.empty(rc, chunkSize) @ rc;
                let numberRead = ref read(stream, deref bytes) @ rc;
                let next = () -> ({
                    if (deref numberRead >= 0) {
                        try {
                            let chunk = deref bytes;
                            Ref.put(Array.empty(rc, chunkSize), bytes);
                            let accRead = deref numberRead;
                            Ref.put(read(stream, deref bytes), numberRead);
                            Some(Ok(readBytesWithHelper(rc, chunk, accRead, chunkSize)))
                        } catch {
                            case ex: IOException => {
                                Ref.put(-1, numberRead);
                                import java.lang.Throwable.getMessage(): String \ {};
                                Some(Err(Generic(getMessage(checked_cast(ex)))))
                            }
                        }
                    } else {
                        None
                    }
                });
                Iterator.iterate(rc, next)
            } catch {
                case ex: IOException =>
                    import java.lang.Throwable.getMessage(): String \ {};
                    Iterator.iterate(rc, constant(Some(Err(Generic(getMessage(checked_cast(ex)))))))
            }
        } else {
            Iterator.iterate(rc, constant(Some(Err(Generic("The argument `chunkSize` has to be a non negative number when calling `File.readChunks`.")))))
        }

    ///
    /// Writes `data` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file was created.
    ///
    pub def write(f: String, data: t): Result[IOError, Bool] \ IO with ToString[t] =
        try {
            import java.io.PrintWriter.print(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            let w = new PrintWriter(javaFile);

            print(w, "${data}");
            close(w);

            if (checkError(w)) {
                Err(Generic("An error occurred when writing to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Writes `data` to the given `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def writeLines(f: String, data: f[String]): Result[IOError, Bool] \ (IO + Foldable.Aef[f]) with Foldable[f] =
        try {
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            let w = new PrintWriter(javaFile);

            Foldable.forEach(line -> println(w, line), data);
            close(w);

            if (checkError(w)) {
                Err(Generic("An error occurred when writing lines to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def append(f: String, data: t): Result[IOError, Bool] \ IO with ToString[t] =
        try {
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            // 2nd parameter `true` is for append.
            let fileWriter = new FileWriter(javaFile, true);
            let printWriter = new PrintWriter(fileWriter);

            println(printWriter, "${data}");
            close(printWriter);

            if (checkError(printWriter)) {
                Err(Generic("An error occurred when appending to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def appendLines(f: String, data: m[String]): Result[IOError, Bool] \ (IO + Foldable.Aef[m]) with Foldable[m] =
        try {
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            // 2nd parameter `true` is for append.
            let fileWriter = new FileWriter(javaFile, true);
            let printWriter = new PrintWriter(fileWriter);

            Foldable.forEach(line -> println(printWriter, line), data);
            close(printWriter);

            if (checkError(printWriter)) {
                Err(Generic("An error occurred when appending lines to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Writes `data` to the given `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def writeBytes(f: String, data: m[Int8]): Result[IOError, Bool] \ (IO + Foldable.Aef[m]) with Foldable[m] = region rc {
        try {
            import java.io.FileOutputStream.write(Array[Int8, rc]): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            let fileStream = new FileOutputStream(javaFile);
            let printWriter = new PrintWriter(fileStream);

            let dataAsArray = Foldable.toArray(rc, data);
            write(fileStream, dataAsArray);
            close(printWriter);

            if (checkError(printWriter)) {
                Err(Generic("An error occurred when writing to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }
    }

    ///
    /// Appends `data` to the given `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def appendBytes(f: String, data: m[Int8]): Result[IOError, Bool] \ (IO + Foldable.Aef[m]) with Foldable[m] = region rc {
        try {
            import java.io.FileOutputStream.write(Array[Int8, rc]): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            // 2nd parameter `true` is for append.
            let fileStream = new FileOutputStream(javaFile, true);
            let printWriter = new PrintWriter(fileStream);

            let dataAsArray = Foldable.toArray(rc, data);
            write(fileStream, dataAsArray);

            close(printWriter);

            if (checkError(printWriter)) {
                Err(Generic("An error occurred when appending to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }
    }

    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def truncate(f: String): Result[IOError, Bool] \ IO =
        try {
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = new File(f);
            let printWriter = new PrintWriter(javaFile);

            close(printWriter);

            if (checkError(printWriter)) {
                Err(Generic("An error occurred when truncating to the file: '${f}'."))
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Creates the directory `d`.
    ///
    /// Returns `Ok(true)` if the directory `d` was created and did not already exist.
    /// Returns `Ok(false)` if the directory `d` already existed and is a directory.
    /// Returns `Err(msg)` if the directory could not be created.
    ///
    pub def mkdir(d: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.mkdir(): Bool \ IO;

            let javaFile = new File(d);
            mkdir(javaFile)
        })

    ///
    /// Creates the directory `d` along with all necessary parent directories.
    ///
    /// Returns `Ok(true)` if the directory `d` was created and did not already exist.
    /// Returns `Ok(false)` if the directory `d` already existed and is a directory.
    /// Returns `Err(msg)` if the directory could not be created.
    ///
    pub def mkdirs(d: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.mkdirs(): Bool \ IO;

            let javaFile = new File(d);
            mkdirs(javaFile)
        })

    ///
    /// Returns a list naming the files and directories in the directory `f`.
    /// The full paths of the files and directories are specified.
    ///
    /// Does not recursively traverse the directory.
    ///
    pub def list(f: String): Result[IOError, List[String]] \ IO = region rc {
        try {
            import java.io.File.listFiles(): Array[File, rc] \ IO;
            import java.io.File.toPath(): Path \ IO;
            import java.nio.file.Path.toString(): String \ IO as pathToString;

            let javaList = new File(f) |> listFiles;

            if (not Object.isNull(javaList)) {
                let paths = javaList |> Array.map(rc, toPath >> pathToString);
                Ok(Array.toList(paths))
            } else {
                Err(Generic("An error occurred when trying to list the file: '${f}'."))
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }
    }

    ///
    /// Returns an Iterator naming the files and directories in the directory `f`.
    /// The full paths of the files and directories are specified.
    ///
    /// Does not recursively traverse the directory.
    ///
    pub def listWithIter(rc: Region[r], f: String): Result[IOError, Iterator[String, r, r]] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.newDirectoryStream(Path): DirectoryStream \ IO;
            import java.nio.file.DirectoryStream.iterator(): JIterator \ IO;
            import java.nio.file.Path.toString(): String \ {} as pathToString;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let javaStream = newDirectoryStream(javaPath);
            let javaIter = iterator(javaStream);

            let iter = fromJavaIter(rc, javaIter);
            Iterator.map(pathToString, iter)
        })

    ///
    /// Returns an Iterator over the files and directories recursively under the path `f`, including `f` itself.
    /// The full paths of the files and directories are specified.
    ///
    /// Recursively traverses the directory.
    ///
    pub def walk(rc: Region[r], f: String): Result[IOError, Iterator[String, r, r]] \ IO + r =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.walk(Path, Array[FileVisitOption, r]): Stream \ IO;
            import java.util.stream.BaseStream.iterator(): JIterator \ IO;
            import java.nio.file.Path.toString(): String \ {} as pathToString;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);
            let javaStream = walk(javaPath, Array#{} @ rc);
            let javaIter = iterator(checked_cast(javaStream));

            let iter = fromJavaIter(rc, javaIter);
            Iterator.map(pathToString, iter)
        })

    ///
    /// Converts a Java iterator to a Flix Iterator.
    ///
    def fromJavaIter(rc: Region[r], iter: JIterator): Iterator[a, r, r] =
        import java.util.Iterator.hasNext(): Bool \ r;
        import java.util.Iterator.next(): a \ r as getNext;
        let next = () -> {
            if(hasNext(iter)) {
                getNext(iter) |> Some
            } else {
                None
            }
        };
        Iterator.iterate(rc, next)

    ///
    /// Moves a file or directory from path `src` to path `dst`.
    ///
    /// Returns `Ok(())` if `src` was moved, and `dst` did not already exist.
    /// Returns `Err(msg)` if `src` was not moved because:
    /// - `dst` already exists, or
    /// - `dst` is a subpath of `src`, or
    /// - an I/O error occurred.
    ///
    pub def move(src: {src = String} , dst: String): Result[IOError, Unit] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.move(Path, Path, Array[CopyOption, rc]): Path \ IO;

            let srcFile = new File(src#src);
            let srcPath = toPath(srcFile);
            let dstFile = new File(dst);
            let dstPath = toPath(dstFile);
            discard move(srcPath, dstPath, Array#{} @ rc)
        })
    }

    ///
    /// Moves a file or directory from path `src` to directory `dst`.
    ///
    /// Returns `Ok(())` if `src` was moved, and `dst` is a directory.
    /// Returns `Err(msg)` if:
    /// - `src` was not moved, or
    /// - `dst` is a subpath of `src`, or
    /// - `dst` is not a directory, or
    /// - an I/O error occurred.
    ///
    pub def moveInto(src: {src = String} , dst: String): Result[IOError, Unit] \ IO =
        try {
            match isDirectory(dst) {
                case Ok(true)  => {
                    import java.io.File.toPath(): Path \ IO;
                    import java.nio.file.Path.getFileName(): Path \ IO;
                    import java.nio.file.Path.toString(): String \ IO as pathToString;

                    let javaFile = new File(src#src);
                    let javaPath = toPath(javaFile);
                    let name = pathToString(getFileName(javaPath));
                    let newDst = dst / name;

                    move(src = src#src, newDst)
                }
                case Ok(false) => Err(Generic("dst is not a directory."))
                case Err(msg)  => Err(msg)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Moves a file or directory from path `src` to path `dst`. Overwrites if `dst` already exists.
    ///
    /// Returns `Ok(())` if `src` was moved.
    /// Returns `Err(msg)` if `src` was not moved, or an I/O error occurred.
    ///
    pub def moveOver(src: {src = String}, dst: String): Result[IOError, Unit] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.move(Path, Path, Array[CopyOption, rc]): Path \ IO;
            import static java_get_field java.nio.file.StandardCopyOption.REPLACE_EXISTING: StandardCopyOption \ IO as getOverwrite;

            let dstFile = new File(dst);
            let dstPath = toPath(dstFile);

            let srcFile = new File(src#src);
            let srcPath = toPath(srcFile);
            discard move(srcPath, dstPath, Array#{checked_cast(getOverwrite())} @ rc)
        })
    }

    ///
    /// Copies a file or directory from path `src` to path `dst`.
    ///
    /// Returns `Ok(())` if `src` was copied, and `dst` did not already exist.
    /// Returns `Err(msg)` if `src` was not copied because:
    /// - `dst` already exists, or
    /// - `dst` is a subpath of `src`, or
    /// - an I/O error occurred.
    ///
    pub def copy(src: {src = String} , dst: String): Result[IOError, Unit] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.copy(Path, Path, Array[CopyOption, rc]): Path \ IO;

            let srcFile = new File(src#src);
            let srcPath = toPath(srcFile);
            let dstFile = new File(dst);
            let dstPath = toPath(dstFile);
            discard copy(srcPath, dstPath, Array#{} @ rc)
       })
    }

    ///
    /// Copies a file or directory from path `src` to directory `dst`.
    ///
    /// Returns `Ok(())` if `src` was copied, and `dst` is a directory.
    /// Returns `Err(msg)` if:
    /// - `src` was not copied, or
    /// - `dst` is a subpath of `src`, or
    /// - `dst` is not a directory, or
    /// - an I/O error occurred.
    ///
    pub def copyInto(src: {src = String} , dst: String): Result[IOError, Unit] \ IO =
        try {
            match isDirectory(dst) {
                case Ok(true)  => {
                    import java.io.File.toPath(): Path \ IO;
                    import java.nio.file.Path.getFileName(): Path \ IO;
                    import java.nio.file.Path.toString(): String \ IO as pathToString;

                    let javaFile = new File(src#src);
                    let javaPath = toPath(javaFile);
                    let name = pathToString(getFileName(javaPath));
                    let newDst = dst / name;

                    copy(src = src#src, newDst)
                }
                case Ok(false) => Err(Generic("dst is not a directory."))
                case Err(msg)  => Err(msg)
            }
        } catch {
            case ex: IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(Generic(getMessage(checked_cast(ex))))
        }

    ///
    /// Copies a file or directory from path `src` to path `dst`. Overwrites if `dst` already exists.
    ///
    /// Returns `Ok(())` if `src` was copied.
    /// Returns `Err(msg)` if `src` was not copied, or an I/O error occurred.
    ///
    pub def copyOver(src: {src = String}, dst: String): Result[IOError, Unit] \ IO = region rc {
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.copy(Path, Path, Array[CopyOption, rc]): Path \ IO;
            import static java_get_field java.nio.file.StandardCopyOption.REPLACE_EXISTING: StandardCopyOption \ IO as getOverwrite;

            let dstFile = new File(dst);
            let dstPath = toPath(dstFile);

            let srcFile = new File(src#src);
            let srcPath = toPath(srcFile);
            discard copy(srcPath, dstPath, Array#{checked_cast(getOverwrite())} @ rc)
        })
    }

    ///
    /// Deletes the given file or directory `f`.
    ///
    /// If `f` is a directory it must be empty.
    ///
    pub def delete(f: String): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.delete(Path): Unit \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);

            delete(javaPath)
        })

    ///
    /// Returns `true` if the given file or directory `f` was deleted
    /// and `false` if `f` was not deleted because it did not exist.
    ///
    /// If `f` is a directory it must be empty.
    ///
    pub def deleteIfExists(f: String): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.File.toPath(): Path \ IO;
            import static java.nio.file.Files.deleteIfExists(Path): Bool \ IO;

            let javaFile = new File(f);
            let javaPath = toPath(javaFile);

            deleteIfExists(javaPath)
        })

}
