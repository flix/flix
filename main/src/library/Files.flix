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
    pub def accessTime(f: String): Result[String, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.lastAccessTime(): ##java.nio.file.attribute.FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> lastAccessTime |> toMillis
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns the creation time of the given file `f` in milliseconds.
    ///
    pub def creationTime(f: String): Result[String, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.creationTime(): ##java.nio.file.attribute.FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> creationTime |> toMillis
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds
    ///
    pub def modificationTime(f: String): Result[String, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.lastModifiedTime(): ##java.nio.file.attribute.FileTime \ IO;
            import java.nio.file.attribute.FileTime.toMillis(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                fileAttributes |> lastModifiedTime |> toMillis
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    pub def size(f: String): Result[String, Int64] \ IO =
        try {
            import java.nio.file.attribute.BasicFileAttributes.size(): Int64 \ IO;

            forM (
                fileAttributes <- getAttributes(f)
            ) yield
                size(fileAttributes)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns the BasicFileAttributes of the file `f`.
    ///
    def getAttributes(f: String): Result[String, ##java.nio.file.attribute.BasicFileAttributes] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.readAttributes(##java.nio.file.Path, ##java.lang.Class, Array[##java.nio.file.LinkOption, rc]): ##java.nio.file.attribute.BasicFileAttributes \ IO;
            import static java.lang.Class.forName(String): ##java.lang.Class \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let classOfAttributes = forName("java.nio.file.attribute.BasicFileAttributes");

            Ok(readAttributes(javaPath, classOfAttributes, Array#{} @ rc))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Returns the type of the given file `f`.
    ///
    pub def fileType(f: String): Result[String, FileType] \ IO =
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
    pub def stat(f: String): Result[String, StatInfo] \ IO =
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
    pub def exists(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.exists(): Bool \ IO;
            Ok(exists(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    pub def isDirectory(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.isDirectory(): Bool \ IO;
            Ok(isDirectory(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    pub def isRegularFile(f: String): Result[String, Bool] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.isRegularFile(##java.nio.file.Path, Array[##java.nio.file.LinkOption, rc]): Bool \ IO;
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let isRegular = isRegularFile(javaPath, Array#{} @ rc);
            Ok(isRegular)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    pub def isReadable(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.canRead(): Bool \ IO;
            Ok(canRead(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    pub def isSymbolicLink(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.isSymbolicLink(##java.nio.file.Path): Bool \ IO;
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let isSymbolic = isSymbolicLink(javaPath);
            Ok(isSymbolic)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    pub def isWritable(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.canWrite(): Bool \ IO;
            Ok(canWrite(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    pub def isExecutable(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.canExecute(): Bool \ IO;
            Ok(canExecute(newFile(f)))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns a string containing the given file `f`.
    ///
    pub def read(f: String): Result[String, String] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.readString(##java.nio.file.Path): String \ IO;
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let result = readString(javaPath);
            Ok(result)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns a string containing the given file `f` with the options `opts`.
    /// The options `opts` to apply consists of
    /// `offset` - the start offset in the given file `f`.
    /// `count` - the number of bytes read.
    /// `charSet` - the specific charset to be used to decode the bytes.
    ///
    pub def readWith(opts: {offset = Int64, count = Int32, charSet = String}, f : String): Result[String, String] \ IO = region rc {
        if (0 < opts.count) {
            try {
                import new java.io.File(String): ##java.io.File \ IO as newFile;
                import new java.io.FileInputStream(##java.io.File): ##java.io.FileInputStream \ IO as newInputStream;
                import java.io.FileInputStream.read(Array[Int8, rc], Int32, Int32): Int32 \ IO;
                import static java.nio.charset.Charset.forName(String): ##java.nio.charset.Charset \ IO;
                import new java.lang.String(Array[Int8, rc], ##java.nio.charset.Charset): String \ IO as newString;

                let javaFile = newFile(f);
                let stream = newInputStream(javaFile);
                match skip((opts.offset / 10_000i64), opts.offset, stream, f) {
                    case Ok(_)    => {
                        let bytes = Array.new(rc, opts.count);
                        discard read(stream, bytes, 0, opts.count);
                        let charSet = forName(opts.charSet);
                        let returnString = newString(bytes, charSet);
                        Ok(returnString)
                    }
                    case Err(msg) => Err(msg)
                }
            } catch {
                case ex: ##java.io.IOException =>
                    import java.lang.Throwable.getMessage(): String \ IO;
                    Err(getMessage(ex))
            }
        } else if (opts.count == 0) {
            Ok("")
        } else {
            Err("The argument `count` has to be a non negative number when calling `Files.readWith`.")
        }
    }

    ///
    /// Helper function for `readWith` and `readBytesWith`.
    /// Tries to skip the requested number of bytes `bytes to skip` in `f` over `i + 1` iterations.
    ///
    def skip(i: Int64, bytesToSkip: Int64, stream: ##java.io.FileInputStream, f: String): Result[String, Unit] \ IO =
        try {
            if (0i64 <= i) {
                import java.io.FileInputStream.skip(Int64): Int64 \ IO as javaSkip;
                let skipped = javaSkip(stream, bytesToSkip);
                if (skipped < bytesToSkip)
                    skip(i - 1i64, bytesToSkip - skipped, stream, f)
                else
                    Ok()
            } else {
                Err("An error occurred when reading the file: '${f}'.")
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns a list of all the lines in the given file `f`.
    ///
    pub def readLines(f: String): Result[String, List[String]] \ IO = try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path): ##java.io.BufferedReader \ IO;
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            Ok(readAll(reader))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: ##java.io.BufferedReader): List[String] \ IO =
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
    pub def readLinesWith(opts: {charSet = String}, f: String): Result[String, List[String]] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset): ##java.io.BufferedReader \ IO;
            import static java.nio.charset.Charset.forName(String): ##java.nio.charset.Charset \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts.charSet));
            Ok(readAll(reader))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns an iterator of the given file `f`
    ///
    pub def readLinesIter(rc: Region[r], f: String): Iterator[Result[String, String], IO, r] \ { r, IO } =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path): ##java.io.BufferedReader \ IO;
            import java.io.BufferedReader.readLine(): String \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath);
            let line = ref readLine(reader) @ rc;
            let next = () -> {
                if (not Object.isNull(deref line)) {
                    try {
                        let l = deref line;
                        line := readLine(reader);
                        Some(Ok(l))
                    } catch {
                        case ex: ##java.io.IOException => {
                            line := checked_cast(null);
                            import java.lang.Throwable.getMessage(): String \ {};
                            Some(Err(getMessage(ex)))
                        }
                    }
                } else {
                    None
                }
            };
            Iterator.iterate(rc, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ {};
                Iterator.iterate(rc, constant(Some(Err(getMessage(ex)))))
        }

    ///
    /// Returns an iterator of the given file `f` with the options `opts`.
    /// The options `opts` to apply consists of
    /// `charSet` - the specific charset to be used to decode the bytes.
    ///
    pub def readLinesIterWith(rc: Region[r], opts: {charSet = String}, f: String): Iterator[Result[String, String], IO, r] \ { r, IO } =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset): ##java.io.BufferedReader \ IO;
            import static java.nio.charset.Charset.forName(String): ##java.nio.charset.Charset \ IO;
            import java.io.BufferedReader.readLine(): String \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let reader = newBufferedReader(javaPath, forName(opts.charSet));
            let line = ref readLine(reader) @ rc;
            let next = () -> {
                if (not Object.isNull(deref line)) {
                    try {
                        let l = deref line;
                        line := readLine(reader);
                        Some(Ok(l))
                    } catch {
                        case ex: ##java.io.IOException => {
                            line := checked_cast(null);
                            import java.lang.Throwable.getMessage(): String \ {};
                            Some(Err(getMessage(ex)))
                        }
                    }
                } else {
                    None
                }
            };
            Iterator.iterate(rc, next)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ {};
                Iterator.iterate(rc, constant(Some(Err(getMessage(ex)))))
        }

    ///
    /// Returns an array of all the bytes in the given file `f`.
    ///
    pub def readBytes(_rc: Region[r], f: String): Result[String, Array[Int8, r]] \ { r, IO } =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.readAllBytes(##java.nio.file.Path): Array[Int8, r] \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let bytes = readAllBytes(javaPath);

            Ok(bytes)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns an array of all the bytes in the given file `f` and applying the options `opts`.
    /// The options `opts` to apply consists of
    /// `offset` - the start offset in the given file `f`.
    /// `count` - the number of bytes read.
    ///
    pub def readBytesWith(rc: Region[r], opts: {offset = Int64, count = Int32}, f: String): Result[String, Array[Int8, r]] \ { r, IO } =
        if (0 < opts.count) {
            try {
                import new java.io.File(String): ##java.io.File \ IO as newFile;
                import new java.io.FileInputStream(##java.io.File): ##java.io.FileInputStream \ IO as newInputStream;
                import java.io.FileInputStream.read(Array[Int8, r], Int32, Int32): Int32 \ IO;

                let javaFile = newFile(f);
                let stream = newInputStream(javaFile);
                match skip((opts.offset / 10_000i64), opts.offset, stream, f) {
                    case Ok(_)    => {
                        let bytes = Array.new(rc, opts.count);
                        let numberRead = read(stream, bytes, 0, opts.count);
                        let res = readBytesWithHelper(rc, bytes, numberRead, opts.count);
                        Ok(res)
                    }
                    case Err(msg) => Err(msg)
                }
            } catch {
                case ex: ##java.io.IOException =>
                    import java.lang.Throwable.getMessage(): String \ IO;
                    Err(getMessage(ex))
            }
        } else if (opts.count == 0) {
            Ok(Array#{} @ rc)
        } else {
            Err("The argument `count` has to be a non negative number when calling `Files.readBytesWith`.")
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
    pub def readChunks(rc: Region[r], chunkSize: Int32, f: String): Iterator[Result[String, Array[Int8, r]], IO, r] \ IO =
        if (0 < chunkSize) {
            try {
                import new java.io.File(String): ##java.io.File \ IO as newFile;
                import new java.io.FileInputStream(##java.io.File): ##java.io.FileInputStream \ IO as newInputStream;
                import java.io.FileInputStream.read(Array[Int8, r]): Int32 \ IO;

                let javaFile = newFile(f);
                let stream = newInputStream(javaFile);
                let bytes = ref Array.new(rc, chunkSize) @ rc;
                let numberRead = ref read(stream, deref bytes) @ rc;
                let next = () -> {
                    if (deref numberRead >= 0) {
                        try {
                            let chunk = deref bytes;
                            bytes := Array.new(rc, chunkSize);
                            let accRead = deref numberRead;
                            numberRead := read(stream, deref bytes);
                            Some(Ok(readBytesWithHelper(rc, chunk, accRead, chunkSize)))
                        } catch {
                            case ex: ##java.io.IOException => {
                                numberRead := -1;
                                import java.lang.Throwable.getMessage(): String \ {};
                                Some(Err(getMessage(ex)))
                            }
                        }
                    } else {
                        None
                    }
                };
                Iterator.iterate(rc, next)
            } catch {
                case ex: ##java.io.IOException =>
                    import java.lang.Throwable.getMessage(): String \ {};
                    Iterator.iterate(rc, constant(Some(Err(getMessage(ex)))))
            }
        } else {
            Iterator.iterate(rc, constant(Some(Err("The argument `chunkSize` has to be a non negative number when calling `Files.readChunks`."))))
        }

    ///
    /// Writes `data` to the given file `f`.
    ///
    /// Creates file `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file was created.
    ///
    pub def write(f: String, data: t): Result[String, Bool] \ IO with ToString[t] =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.PrintWriter(##java.io.File): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.PrintWriter.print(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            let w = newPrintWriter(javaFile);

            print(w, "${data}");
            close(w);

            if (checkError(w)) {
                Err("An error occurred when writing to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Writes `data` to the given `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def writeLines(f: String, data: f[String]): Result[String, Bool] \ IO with Foldable[f] =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.PrintWriter(##java.io.File): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            let w = newPrintWriter(javaFile);

            Foldable.forEach(line -> println(w, line), data);
            close(w);

            if (checkError(w)) {
                Err("An error occurred when writing lines to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def append(f: String, data: t): Result[String, Bool] \ IO with ToString[t] =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.FileWriter(##java.io.File, Bool): ##java.io.FileWriter \ IO as newFileWriter;
            import new java.io.PrintWriter(##java.io.Writer): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            // 2nd parameter `true` is for append.
            let fileWriter = newFileWriter(javaFile, true);
            let printWriter = newPrintWriter(checked_cast(fileWriter));

            println(printWriter, "${data}");
            close(printWriter);

            if (checkError(printWriter)) {
                Err("An error occurred when appending to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Appends `data` to the given file `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def appendLines(f: String, data: f[String]): Result[String, Bool] \ IO with Foldable[f] =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.FileWriter(##java.io.File, Bool): ##java.io.FileWriter \ IO as newFileWriter;
            import new java.io.PrintWriter(##java.io.Writer): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.PrintWriter.println(String): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            // 2nd parameter `true` is for append.
            let fileWriter = newFileWriter(javaFile, true);
            let printWriter = newPrintWriter(checked_cast(fileWriter));

            Foldable.forEach(line -> println(printWriter, line), data);
            close(printWriter);

            if (checkError(printWriter)) {
                Err("An error occurred when appending lines to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Writes `data` to the given `f`.
    ///
    /// Creates `f` if it does not exist. Overwrites it if it exists.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def writeBytes(f: String, data: f[Int8]): Result[String, Bool] \ IO with Foldable[f] = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.FileOutputStream(##java.io.File): ##java.io.FileOutputStream \ IO as newFileStream;
            import new java.io.PrintWriter(##java.io.OutputStream): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.FileOutputStream.write(Array[Int8, rc]): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            let fileStream = newFileStream(javaFile);
            let printWriter = newPrintWriter(checked_cast(fileStream));

            let dataAsArray = Foldable.toArray(rc, data);
            write(fileStream, dataAsArray);
            close(printWriter);

            if (checkError(printWriter)) {
                Err("An error occurred when writing to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Appends `data` to the given `f`.
    ///
    /// Returns `true` if the file `f` was created, and `false` if `data` was appended to an existing `f`.
    ///
    pub def appendBytes(f: String, data: f[Int8]): Result[String, Bool] \ IO with Foldable[f] = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.FileOutputStream(##java.io.File, Bool): ##java.io.FileOutputStream \ IO as newFileStream;
            import new java.io.PrintWriter(##java.io.OutputStream): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.FileOutputStream.write(Array[Int8, rc]): Unit \ IO;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            // 2nd parameter `true` is for append.
            let fileStream = newFileStream(javaFile, true);
            let printWriter = newPrintWriter(checked_cast(fileStream));

            let dataAsArray = Foldable.toArray(rc, data);
            write(fileStream, dataAsArray);

            close(printWriter);

            if (checkError(printWriter)) {
                Err("An error occurred when appending to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Returns `true` if the file `f` was created, and `false` if `f` was overwritten.
    ///
    pub def truncate(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import new java.io.PrintWriter(##java.io.File): ##java.io.PrintWriter \ IO as newPrintWriter;
            import java.io.PrintWriter.close(): Unit \ IO;
            import java.io.PrintWriter.checkError(): Bool \ IO;

            let alreadyExists = Files.exists(f);

            let javaFile = newFile(f);
            let printWriter = newPrintWriter(javaFile);

            close(printWriter);

            if (checkError(printWriter)) {
                Err("An error occurred when truncating to the file: '${f}'.")
            }
            else {
                Result.flatMap(exists -> Ok(not exists), alreadyExists)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Creates the directory `d`.
    ///
    /// Returns `Ok(true)` if the directory `d` was created and did not already exist.
    /// Returns `Ok(false)` if the directory `d` already existed and is a directory.
    /// Returns `Err(msg)` if the directory could not be created.
    ///
    pub def mkdir(d: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.mkdir(): Bool \ IO;

            let javaFile = newFile(d);
            Ok(mkdir(javaFile))
        } catch {
            case ex: ##java.io.IOException =>
            import java.lang.Throwable.getMessage(): String \ IO;
            Err(getMessage(ex))
        }

    ///
    /// Creates the directory `d` along with all necessary parent directories.
    ///
    /// Returns `Ok(true)` if the directory `d` was created and did not already exist.
    /// Returns `Ok(false)` if the directory `d` already existed and is a directory.
    /// Returns `Err(msg)` if the directory could not be created.
    ///
    pub def mkdirs(d: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.mkdirs(): Bool \ IO;

            let javaFile = newFile(d);
            Ok(mkdirs(javaFile))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns a list naming the files and directories in the directory `f`.
    /// The full paths of the files and directories are specified.
    ///
    /// Does not recursively traverse the directory.
    ///
    pub def list(f: String): Result[String, List[String]] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.listFiles(): Array[##java.io.File, rc] \ IO;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import java.nio.file.Path.toString(): String \ IO as pathToString;

            let javaList = f |> newFile |> listFiles;

            if (not Object.isNull(javaList)) {
                let paths = javaList |> Array.map(rc, toPath >> pathToString);
                Ok(Array.toList(paths))
            } else {
                Err("An error occurred when trying to list the file: '${f}'.")
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Returns an Iterator naming the files and directories in the directory `f`.
    /// The full paths of the files and directories are specified.
    ///
    /// Does not recursively traverse the directory.
    ///
    pub def listWithIter(rc: Region[r], f: String): Result[String, Iterator[String, r, r]] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.newDirectoryStream(##java.nio.file.Path): ##java.nio.file.DirectoryStream \ IO;
            import java.nio.file.DirectoryStream.iterator(): ##java.util.Iterator \ IO;
            import java.nio.file.Path.toString(): String \ {} as pathToString;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let javaStream = newDirectoryStream(javaPath);
            let javaIter = iterator(javaStream);

            let iter = fromJavaIter(rc, javaIter);
            Ok(Iterator.map(pathToString, iter))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns an Iterator over the files and directories recursively under the path `f`, including `f` itself.
    /// The full paths of the files and directories are specified.
    ///
    /// Recursively traverses the directory.
    ///
    pub def walk(rc: Region[r], f: String): Result[String, Iterator[String, r, r]] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.walk(##java.nio.file.Path, Array[##java.nio.file.FileVisitOption, r]): ##java.util.stream.Stream \ IO;
            import java.util.stream.BaseStream.iterator(): ##java.util.Iterator \ IO;
            import java.nio.file.Path.toString(): String \ {} as pathToString;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let javaStream = walk(javaPath, Array#{} @ rc);
            let javaIter = iterator(checked_cast(javaStream));

            let iter = fromJavaIter(rc, javaIter);
            Ok(Iterator.map(pathToString, iter))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Converts a Java iterator to a Flix Iterator.
    ///
    def fromJavaIter(rc: Region[r], iter: ##java.util.Iterator): Iterator[a, r, r] =
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
    pub def move(src: {src = String} , dst: String): Result[String, Unit] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.move(##java.nio.file.Path, ##java.nio.file.Path, Array[##java.nio.file.CopyOption, rc]): ##java.nio.file.Path \ IO;

            let srcFile = newFile(src.src);
            let srcPath = toPath(srcFile);
            let dstFile = newFile(dst);
            let dstPath = toPath(dstFile);
            discard move(srcPath, dstPath, Array#{} @ rc);
            Ok()
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
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
    pub def moveInto(src: {src = String} , dst: String): Result[String, Unit] \ IO =
        try {
            match Files.isDirectory(dst) {
                case Ok(true)  => {
                    import new java.io.File(String): ##java.io.File \ IO as newFile;
                    import java.io.File.toPath(): ##java.nio.file.Path \ IO;
                    import java.nio.file.Path.getFileName(): ##java.nio.file.Path \ IO;
                    import java.nio.file.Path.toString(): String \ IO as pathToString;

                    let javaFile = newFile(src.src);
                    let javaPath = toPath(javaFile);
                    let name = pathToString(getFileName(javaPath));
                    let newDst = dst / name;

                    move(src = src.src, newDst)
                }
                case Ok(false) => Err("dst is not a directory.")
                case Err(msg)  => Err(msg)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Moves a file or directory from path `src` to path `dst`. Overwrites if `dst` already exists.
    ///
    /// Returns `Ok(())` if `src` was moved.
    /// Returns `Err(msg)` if `src` was not moved, or an I/O error occurred.
    ///
    pub def moveOver(src: {src = String}, dst: String): Result[String, Unit] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.move(##java.nio.file.Path, ##java.nio.file.Path, Array[##java.nio.file.CopyOption, rc]): ##java.nio.file.Path \ IO;
            import static get java.nio.file.StandardCopyOption.REPLACE_EXISTING: ##java.nio.file.StandardCopyOption \ IO as getOverwrite;

            let dstFile = newFile(dst);
            let dstPath = toPath(dstFile);

            let srcFile = newFile(src.src);
            let srcPath = toPath(srcFile);
            discard move(srcPath, dstPath, Array#{checked_cast(getOverwrite())} @ rc);

            Ok()
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
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
    pub def copy(src: {src = String} , dst: String): Result[String, Unit] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.copy(##java.nio.file.Path, ##java.nio.file.Path, Array[##java.nio.file.CopyOption, rc]): ##java.nio.file.Path \ IO;

            let srcFile = newFile(src.src);
            let srcPath = toPath(srcFile);
            let dstFile = newFile(dst);
            let dstPath = toPath(dstFile);
            discard copy(srcPath, dstPath, Array#{} @ rc);
            Ok()
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
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
    pub def copyInto(src: {src = String} , dst: String): Result[String, Unit] \ IO =
        try {
            match Files.isDirectory(dst) {
                case Ok(true)  => {
                    import new java.io.File(String): ##java.io.File \ IO as newFile;
                    import java.io.File.toPath(): ##java.nio.file.Path \ IO;
                    import java.nio.file.Path.getFileName(): ##java.nio.file.Path \ IO;
                    import java.nio.file.Path.toString(): String \ IO as pathToString;

                    let javaFile = newFile(src.src);
                    let javaPath = toPath(javaFile);
                    let name = pathToString(getFileName(javaPath));
                    let newDst = dst / name;

                    copy(src = src.src, newDst)
                }
                case Ok(false) => Err("dst is not a directory.")
                case Err(msg)  => Err(msg)
            }
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Copies a file or directory from path `src` to path `dst`. Overwrites if `dst` already exists.
    ///
    /// Returns `Ok(())` if `src` was copied.
    /// Returns `Err(msg)` if `src` was not copied, or an I/O error occurred.
    ///
    pub def copyOver(src: {src = String}, dst: String): Result[String, Unit] \ IO = region rc {
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.copy(##java.nio.file.Path, ##java.nio.file.Path, Array[##java.nio.file.CopyOption, rc]): ##java.nio.file.Path \ IO;
            import static get java.nio.file.StandardCopyOption.REPLACE_EXISTING: ##java.nio.file.StandardCopyOption \ IO as getOverwrite;

            let dstFile = newFile(dst);
            let dstPath = toPath(dstFile);

            let srcFile = newFile(src.src);
            let srcPath = toPath(srcFile);
            discard copy(srcPath, dstPath, Array#{checked_cast(getOverwrite())} @ rc);

            Ok()
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }
    }

    ///
    /// Deletes the given file or directory `f`.
    ///
    /// If `f` is a directory it must be empty.
    ///
    pub def delete(f: String): Result[String, Unit] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.delete(##java.nio.file.Path): Unit \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);

            Ok(delete(javaPath))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

    ///
    /// Returns `true` if the given file or directory `f` was deleted
    /// and `false` if `f` was not deleted because it did not exist.
    ///
    /// If `f` is a directory it must be empty.
    ///
    pub def deleteIfExists(f: String): Result[String, Bool] \ IO =
        try {
            import new java.io.File(String): ##java.io.File \ IO as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path \ IO;
            import static java.nio.file.Files.deleteIfExists(##java.nio.file.Path): Bool \ IO;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);

            Ok(deleteIfExists(javaPath))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String \ IO;
                Err(getMessage(ex))
        }

}
