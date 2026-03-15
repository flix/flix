/*
 * Copyright 2026 Flix Authors
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

pub mod Fs.FileSystem {

    import java.io.File
    import java.io.IOException
    import java.lang.Class
    import java.lang.IllegalArgumentException
    import java.lang.UnsupportedOperationException
    import java.nio.charset.StandardCharsets
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.attribute.FileAttribute
    import java.nio.file.FileAlreadyExistsException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.NotDirectoryException
    import java.nio.file.OpenOption
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.StandardOpenOption

    use Fs.AccessTime
    use Fs.AppendBytes
    use Fs.AppendFile
    use Fs.AppendLines
    use Fs.CreationTime
    use Fs.DirList
    use Fs.FileExists
    use Fs.FilePermission
    use Fs.FileRead
    use Fs.FileSize
    use Fs.FileStat
    use Fs.FileSystem
    use Fs.FileTest
    use Fs.FileTime
    use Fs.FileWrite
    use Fs.FsClient.{logPre, logPost}
    use Fs.IsDirectory
    use Fs.IsExecutable
    use Fs.IsReadable
    use Fs.IsRegularFile
    use Fs.IsSymbolicLink
    use Fs.IsWritable
    use Fs.MkDir
    use Fs.MkDirs
    use Fs.MkTempDir
    use Fs.ModificationTime
    use Fs.ReadBytes
    use Fs.ReadFile
    use Fs.ReadLines
    use Fs.Truncate
    use Fs.WriteBytes
    use Fs.WriteFile
    use Fs.WriteLines
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileSystem) + IO = x ->
        run {
            f(x)
        } with handler FileSystem {
            def exists(filename, k) = {
                let res = try {
                    Ok(Files.exists(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isDirectory(filename, k) = {
                let res = try {
                    Ok(Files.isDirectory(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isRegularFile(filename, k) = {
                let res = try {
                    Ok(Files.isRegularFile(Paths.get(filename), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isSymbolicLink(filename, k) = {
                let res = try {
                    Ok(Files.isSymbolicLink(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isReadable(filename, k) = {
                let res = try {
                    Ok(Files.isReadable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isWritable(filename, k) = {
                let res = try {
                    Ok(Files.isWritable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isExecutable(filename, k) = {
                let res = try {
                    Ok(Files.isExecutable(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def accessTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).lastAccessTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def creationTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def modificationTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).lastModifiedTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def size(filename, k) = {
                let res = try {
                    Ok(Files.size(Paths.get(filename)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def read(filename, k) = {
                let res = try {
                    Ok(String.fromBytes(Array.toVector(Files.readAllBytes(Paths.get(filename)))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readLines(filename, k) = {
                let res = try {
                    Ok(ToFlix.toFlix(Files.readAllLines(Paths.get(filename))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readBytes(filename, k) = {
                let res = try {
                    Ok(Array.toVector(Files.readAllBytes(Paths.get(filename))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def list(filename, k) = {
                let res = try {
                    let file = new File(filename);
                    Ok(Array.toList(file.list()))
                } catch {
                    case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: NotDirectoryException => Err(IoError(ErrorKind.NotDirectory, ex.getMessage()))
                    case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def write(data, file, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    Files.write(Paths.get(file), bytes, (...{}: Vector[OpenOption]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def writeLines(data, file, k) = {
                let res = try {
                    let bf = Files.newBufferedWriter(Paths.get(file), (...{}: Vector[OpenOption]));
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def writeBytes(data, file, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        Files.write(Paths.get(file), bytes, (...{}: Vector[OpenOption]));
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def append(data, file, k) = {
                let res = try {
                    let bytes = data#str.getBytes(StandardCharsets.UTF_8);
                    let opts: Vector[OpenOption] = ...{
                        checked_cast(StandardOpenOption.APPEND),
                        checked_cast(StandardOpenOption.CREATE)
                    };
                    Files.write(Paths.get(file), bytes, opts);
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def appendLines(data, file, k) = {
                let res = try {
                    let opts: Vector[OpenOption] = ...{
                        checked_cast(StandardOpenOption.APPEND),
                        checked_cast(StandardOpenOption.CREATE)
                    };
                    let bf = Files.newBufferedWriter(Paths.get(file), opts);
                    List.forEach(line -> {
                        bf.write(line, 0, String.length(line));
                        bf.newLine()
                    }, data#lines);
                    bf.close();
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def appendBytes(data, file, k) = {
                let res = try {
                    region rc {
                        let bytes = Vector.toArray(rc, data);
                        let opts: Vector[OpenOption] = ...{
                            checked_cast(StandardOpenOption.APPEND),
                            checked_cast(StandardOpenOption.CREATE)
                        };
                        Files.write(Paths.get(file), bytes, opts);
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def truncate(file, k) = {
                let res = try {
                    region rc {
                        let opt: OpenOption = checked_cast(StandardOpenOption.TRUNCATE_EXISTING);
                        let arr: Array[Int8, rc] = Array#{} @ rc;
                        Files.write(Paths.get(file), arr, ...{opt});
                        Ok(())
                    }
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def mkDir(d, k) = {
                let res = try {
                    Files.createDirectory(Paths.get(d), (...{}: Vector[FileAttribute]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def mkDirs(d, k) = {
                let res = try {
                    Files.createDirectories(Paths.get(d), (...{}: Vector[FileAttribute]));
                    Ok(())
                } catch {
                    case ex: InvalidPathException          => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: FileAlreadyExistsException    => Err(IoError(ErrorKind.AlreadyExists, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def mkTempDir(prefix, k) = {
                let res = try {
                    let path = Files.createTempDirectory(prefix, (...{}: Vector[FileAttribute]));
                    Ok(path.toString())
                } catch {
                    case ex: IllegalArgumentException      => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileSystem) + IO = handle(f)()

    ///
    /// Middleware that intercepts the `FileSystem` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    /// The function `f` must not use leaf or intermediate effects directly —
    /// those are excluded from the effect row to ensure all operations are logged.
    ///
    pub def withLogging(f: Unit -> a \ ef - {FileExists, IsDirectory, IsRegularFile, IsSymbolicLink, IsReadable, IsWritable, IsExecutable, AccessTime, CreationTime, ModificationTime, FileSize, ReadFile, ReadLines, ReadBytes, DirList, WriteFile, WriteLines, WriteBytes, AppendFile, AppendLines, AppendBytes, Truncate, MkDir, MkDirs, MkTempDir, FileTest, FilePermission, FileTime, FileStat, FileRead, FileWrite}): a \ (ef - FileSystem) + {FileSystem, Logger} =
        run { f() } with handler FileSystem {
            def exists(filename, k) = {
                logPre("exists", filename);
                let result = FileSystem.exists(filename);
                logPost("exists", filename, b -> "${b}", result);
                k(result)
            }
            def isDirectory(filename, k) = {
                logPre("isDirectory", filename);
                let result = FileSystem.isDirectory(filename);
                logPost("isDirectory", filename, b -> "${b}", result);
                k(result)
            }
            def isRegularFile(filename, k) = {
                logPre("isRegularFile", filename);
                let result = FileSystem.isRegularFile(filename);
                logPost("isRegularFile", filename, b -> "${b}", result);
                k(result)
            }
            def isSymbolicLink(filename, k) = {
                logPre("isSymbolicLink", filename);
                let result = FileSystem.isSymbolicLink(filename);
                logPost("isSymbolicLink", filename, b -> "${b}", result);
                k(result)
            }
            def isReadable(filename, k) = {
                logPre("isReadable", filename);
                let result = FileSystem.isReadable(filename);
                logPost("isReadable", filename, b -> "${b}", result);
                k(result)
            }
            def isWritable(filename, k) = {
                logPre("isWritable", filename);
                let result = FileSystem.isWritable(filename);
                logPost("isWritable", filename, b -> "${b}", result);
                k(result)
            }
            def isExecutable(filename, k) = {
                logPre("isExecutable", filename);
                let result = FileSystem.isExecutable(filename);
                logPost("isExecutable", filename, b -> "${b}", result);
                k(result)
            }
            def accessTime(filename, k) = {
                logPre("accessTime", filename);
                let result = FileSystem.accessTime(filename);
                logPost("accessTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def creationTime(filename, k) = {
                logPre("creationTime", filename);
                let result = FileSystem.creationTime(filename);
                logPost("creationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def modificationTime(filename, k) = {
                logPre("modificationTime", filename);
                let result = FileSystem.modificationTime(filename);
                logPost("modificationTime", filename, t -> "${t}ms", result);
                k(result)
            }
            def size(filename, k) = {
                logPre("size", filename);
                let result = FileSystem.size(filename);
                logPost("size", filename, s -> "${s} bytes", result);
                k(result)
            }
            def read(filename, k) = {
                logPre("read", filename);
                let result = FileSystem.read(filename);
                logPost("read", filename, s -> "${String.length(s)} chars", result);
                k(result)
            }
            def readLines(filename, k) = {
                logPre("readLines", filename);
                let result = FileSystem.readLines(filename);
                logPost("readLines", filename, ls -> "${List.length(ls)} lines", result);
                k(result)
            }
            def readBytes(filename, k) = {
                logPre("readBytes", filename);
                let result = FileSystem.readBytes(filename);
                logPost("readBytes", filename, bs -> "${Vector.length(bs)} bytes", result);
                k(result)
            }
            def list(filename, k) = {
                logPre("list", filename);
                let result = FileSystem.list(filename);
                logPost("list", filename, ls -> "${List.length(ls)} entries", result);
                k(result)
            }
            def write(data, file, k) = {
                logPre("write", file);
                let result = FileSystem.write(data, file);
                logPost("write", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def writeLines(data, file, k) = {
                logPre("writeLines", file);
                let result = FileSystem.writeLines(data, file);
                logPost("writeLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def writeBytes(data, file, k) = {
                logPre("writeBytes", file);
                let result = FileSystem.writeBytes(data, file);
                logPost("writeBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def append(data, file, k) = {
                logPre("append", file);
                let result = FileSystem.append(data, file);
                logPost("append", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def appendLines(data, file, k) = {
                logPre("appendLines", file);
                let result = FileSystem.appendLines(data, file);
                logPost("appendLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def appendBytes(data, file, k) = {
                logPre("appendBytes", file);
                let result = FileSystem.appendBytes(data, file);
                logPost("appendBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def truncate(file, k) = {
                logPre("truncate", file);
                let result = FileSystem.truncate(file);
                logPost("truncate", file, _ -> "done", result);
                k(result)
            }
            def mkDir(d, k) = {
                logPre("mkDir", d);
                let result = FileSystem.mkDir(d);
                logPost("mkDir", d, _ -> "done", result);
                k(result)
            }
            def mkDirs(d, k) = {
                logPre("mkDirs", d);
                let result = FileSystem.mkDirs(d);
                logPost("mkDirs", d, _ -> "done", result);
                k(result)
            }
            def mkTempDir(prefix, k) = {
                logPre("mkTempDir", prefix);
                let result = FileSystem.mkTempDir(prefix);
                logPost("mkTempDir", prefix, path -> path, result);
                k(result)
            }
        }

    ///
    /// Returns the attributes of the given file `f`.
    /// May throw `IOException` and `SecurityException`.
    ///
    def fileAttributes(path: Path): BasicFileAttributes \ IO =
        Files.readAttributes(
            path,
            Class.forName("java.nio.file.attribute.BasicFileAttributes"),
            (...{}: Vector[LinkOption])
        )

}
