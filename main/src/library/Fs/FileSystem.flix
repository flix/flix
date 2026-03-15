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

    use Fs.FileSystem
    use Fs.FsClient.{logPre, logPost}
    use IoError.IoError

    ///
    /// Handles the `FileSystem` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileSystem` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileSystem) + IO = x ->
        run {
            f(x)
        } with handler FileSystem {
            def exists(filename, k)           = k(Fs.FsClient.exists(filename))
            def isDirectory(filename, k)      = k(Fs.FsClient.isDirectory(filename))
            def isRegularFile(filename, k)    = k(Fs.FsClient.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(Fs.FsClient.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(Fs.FsClient.isReadable(filename))
            def isWritable(filename, k)       = k(Fs.FsClient.isWritable(filename))
            def isExecutable(filename, k)     = k(Fs.FsClient.isExecutable(filename))
            def accessTime(filename, k)       = k(Fs.FsClient.accessTime(filename))
            def creationTime(filename, k)     = k(Fs.FsClient.creationTime(filename))
            def modificationTime(filename, k) = k(Fs.FsClient.modificationTime(filename))
            def size(filename, k)             = k(Fs.FsClient.size(filename))
            def read(filename, k)             = k(Fs.FsClient.read(filename))
            def readLines(filename, k)        = k(Fs.FsClient.readLines(filename))
            def readBytes(filename, k)        = k(Fs.FsClient.readBytes(filename))
            def list(filename, k)             = k(Fs.FsClient.list(filename))
            def write(data, file, k)          = k(Fs.FsClient.write(data, file))
            def writeLines(data, file, k)     = k(Fs.FsClient.writeLines(data, file))
            def writeBytes(data, file, k)     = k(Fs.FsClient.writeBytes(data, file))
            def append(data, file, k)         = k(Fs.FsClient.append(data, file))
            def appendLines(data, file, k)    = k(Fs.FsClient.appendLines(data, file))
            def appendBytes(data, file, k)    = k(Fs.FsClient.appendBytes(data, file))
            def truncate(file, k)             = k(Fs.FsClient.truncate(file))
            def mkDir(d, k)                   = k(Fs.FsClient.mkDir(d))
            def mkDirs(d, k)                  = k(Fs.FsClient.mkDirs(d))
            def mkTempDir(prefix, k)          = k(Fs.FsClient.mkTempDir(prefix))
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
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - FileSystem) + {FileSystem, Logger} =
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

}
