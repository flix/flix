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

pub mod Fs.FileWrite {

    use Fs.AppendBytes
    use Fs.AppendFile
    use Fs.AppendLines
    use Fs.FileSystem
    use Fs.FileWrite
    use Fs.FsClient.{logPre, logPost}
    use Fs.MkDir
    use Fs.MkDirs
    use Fs.MkTempDir
    use Fs.Truncate
    use Fs.WriteBytes
    use Fs.WriteFile
    use Fs.WriteLines

    ///
    /// Handles the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileWrite) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                handleWithFileSystem(f)
            )(x)
        )

    ///
    /// Runs the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileWrite) + IO = handle(f)()

    ///
    /// Handles the `FileWrite` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - FileWrite) + FileSystem = x ->
        run { f(x) } with handler FileWrite {
            def write(data, file, k)       = k(Fs.FileSystem.write(data, file))
            def writeLines(data, file, k)  = k(Fs.FileSystem.writeLines(data, file))
            def writeBytes(data, file, k)  = k(Fs.FileSystem.writeBytes(data, file))
            def append(data, file, k)      = k(Fs.FileSystem.append(data, file))
            def appendLines(data, file, k) = k(Fs.FileSystem.appendLines(data, file))
            def appendBytes(data, file, k) = k(Fs.FileSystem.appendBytes(data, file))
            def truncate(file, k)          = k(Fs.FileSystem.truncate(file))
            def mkDir(d, k)                = k(Fs.FileSystem.mkDir(d))
            def mkDirs(d, k)               = k(Fs.FileSystem.mkDirs(d))
            def mkTempDir(prefix, k)       = k(Fs.FileSystem.mkTempDir(prefix))
        }

    ///
    /// Middleware that intercepts the `FileWrite` effect, logging each operation
    /// and its result via the `Logger` effect.
    ///
    /// Successful operations are logged at `Debug` level; errors at `Warn`.
    ///
    /// The function `f` must not use the leaf effects directly — those are
    /// excluded from the effect row to ensure all write operations are logged.
    ///
    pub def withLogging(f: Unit -> a \ ef - {WriteFile, WriteLines, WriteBytes, AppendFile, AppendLines, AppendBytes, Truncate, MkDir, MkDirs, MkTempDir}): a \ (ef - FileWrite) + {FileWrite, Logger} =
        run { f() } with handler FileWrite {
            def write(data, file, k) = {
                logPre("write", file);
                let result = FileWrite.write(data, file);
                logPost("write", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def writeLines(data, file, k) = {
                logPre("writeLines", file);
                let result = FileWrite.writeLines(data, file);
                logPost("writeLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def writeBytes(data, file, k) = {
                logPre("writeBytes", file);
                let result = FileWrite.writeBytes(data, file);
                logPost("writeBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def append(data, file, k) = {
                logPre("append", file);
                let result = FileWrite.append(data, file);
                logPost("append", file, _ -> "${String.length(data#str)} chars", result);
                k(result)
            }
            def appendLines(data, file, k) = {
                logPre("appendLines", file);
                let result = FileWrite.appendLines(data, file);
                logPost("appendLines", file, _ -> "${List.length(data#lines)} lines", result);
                k(result)
            }
            def appendBytes(data, file, k) = {
                logPre("appendBytes", file);
                let result = FileWrite.appendBytes(data, file);
                logPost("appendBytes", file, _ -> "${Vector.length(data)} bytes", result);
                k(result)
            }
            def truncate(file, k) = {
                logPre("truncate", file);
                let result = FileWrite.truncate(file);
                logPost("truncate", file, _ -> "done", result);
                k(result)
            }
            def mkDir(d, k) = {
                logPre("mkDir", d);
                let result = FileWrite.mkDir(d);
                logPost("mkDir", d, _ -> "done", result);
                k(result)
            }
            def mkDirs(d, k) = {
                logPre("mkDirs", d);
                let result = FileWrite.mkDirs(d);
                logPost("mkDirs", d, _ -> "done", result);
                k(result)
            }
            def mkTempDir(prefix, k) = {
                logPre("mkTempDir", prefix);
                let result = FileWrite.mkTempDir(prefix);
                logPost("mkTempDir", prefix, path -> path, result);
                k(result)
            }
        }

}
