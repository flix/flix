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
/// Portable-only: handler implementation for `FileWrite`.
///
/// Uses the `%%FILE_*%%` primops which are implemented by both the JVM and LLVM backends.
///

pub mod FileWrite {

    use IoError.IoError

    ///
    /// Handles the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - FileWrite) + IO = x ->
        run {
            Ok(f(x))
        } with handler FileWrite {

            def write(data, file, k) = {
                let (ok, _, kind, msg) = %%FILE_WRITE%%((data#str, file));
                if (ok) k(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def writeLines(data, file, k) = {
                let res = region rc {
                    let sb = StringBuilder.empty(rc);
                    List.forEach(line -> StringBuilder.appendLine(line, sb), data#lines);
                    let s = StringBuilder.toString(sb);
                    let (ok, _, kind, msg) = %%FILE_WRITE%%((s, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(()) => k(())
                    case Err(err) => Err(err)
                }
            }

            def writeBytes(data, file, k) = {
                let res = region rc {
                    let bytes = Vector.toArray(rc, data);
                    let (ok, _, kind, msg) = %%FILE_WRITE_BYTES%%((bytes, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(()) => k(())
                    case Err(err) => Err(err)
                }
            }

            def append(data, file, k) = {
                let (ok, _, kind, msg) = %%FILE_APPEND%%((data#str, file));
                if (ok) k(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def appendLines(data, file, k) = {
                let res = region rc {
                    let sb = StringBuilder.empty(rc);
                    List.forEach(line -> StringBuilder.appendLine(line, sb), data#lines);
                    let s = StringBuilder.toString(sb);
                    let (ok, _, kind, msg) = %%FILE_APPEND%%((s, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(()) => k(())
                    case Err(err) => Err(err)
                }
            }

            def appendBytes(data, file, k) = {
                let res = region rc {
                    let bytes = Vector.toArray(rc, data);
                    let (ok, _, kind, msg) = %%FILE_APPEND_BYTES%%((bytes, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                match res {
                    case Ok(()) => k(())
                    case Err(err) => Err(err)
                }
            }

            def truncate(file, k) = {
                let (ok, _, kind, msg) = %%FILE_TRUNCATE%%(file);
                if (ok) k(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def mkDir(d, k) = {
                let (ok, _, kind, msg) = %%FILE_MKDIR%%(d);
                if (ok) k(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def mkDirs(d, k) = {
                let (ok, _, kind, msg) = %%FILE_MKDIRS%%(d);
                if (ok) k(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

            def mkTempDir(prefix, k) = {
                let (ok, path, kind, msg) = %%FILE_MK_TEMP_DIR%%(prefix);
                if (ok) k(path) else Err(IoError(IoError.errorKindFromCode(kind), msg))
            }

        }

    ///
    /// Runs the `FileWrite` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWrite` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ (ef - FileWrite) + IO = handle(f)()

}
