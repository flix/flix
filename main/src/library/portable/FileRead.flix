/*
 * Copyright 2026 Magnus Madsen
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

///
/// Portable-only: handler implementation for `FileRead`.
///
/// Uses the `%%FILE_*%%` primops which are implemented by both the JVM and LLVM backends.
///

pub mod FileRead {

    use IoError.IoError

    ///
    /// Handles the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - FileRead) + IO = x ->
        run {
            Ok(f(x))
        } with handler FileRead {

            def exists(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_EXISTS%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isDirectory(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_DIRECTORY%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isRegularFile(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_REGULAR_FILE%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isReadable(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_READABLE%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isSymbolicLink(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_SYMBOLIC_LINK%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isWritable(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_WRITABLE%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def isExecutable(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_IS_EXECUTABLE%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def accessTime(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_ACCESS_TIME%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def creationTime(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_CREATION_TIME%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def modificationTime(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_MODIFICATION_TIME%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def size(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_SIZE%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def read(filename, k) = {
                let (ok, v, kind, msg) = %%FILE_READ%%(filename);
                if (ok) k(v) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def readLines(filename, k) = {
                let res = region rc {
                    let (ok, v, kind, msg) = %%FILE_READ_LINES%%((rc, filename));
                    if (ok) Ok(Array.toList(v)) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(lines) => k(lines)
                    case Err(err) => Err(err)
                }
            }

            def readBytes(filename, k) = {
                let res = region rc {
                    let (ok, v, kind, msg) = %%FILE_READ_BYTES%%((rc, filename));
                    if (ok) Ok(Array.toVector(v)) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(bytes) => k(bytes)
                    case Err(err) => Err(err)
                }
            }

            def list(filename, k) = {
                let res = region rc {
                    let (ok, v, kind, msg) = %%FILE_LIST%%((rc, filename));
                    if (ok) Ok(Array.toList(v)) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(xs) => k(xs)
                    case Err(err) => Err(err)
                }
            }

        }

    ///
    /// Runs the `FileRead` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileRead` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ (ef - FileRead) + IO = handle(f)()

}
