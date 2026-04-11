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
/// Portable-only: default handler implementation for `FileWriteWithResult`.
///
/// Uses the `%%FILE_*%%` primops which are implemented by both the JVM and LLVM backends.
///

pub mod FileWriteWithResult {

    use IoError.IoError

    ///
    /// Handles the `FileWriteWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWriteWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileWriteWithResult) + IO = x ->
        run {
            f(x)
        } with handler FileWriteWithResult {

            def write(data, file, k) = {
                let (ok, _, kind, msg) = %%FILE_WRITE%%((data#str, file));
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def writeLines(data, file, k) = {
                let res = region rc {
                    let sb = StringBuilder.empty(rc);
                    List.forEach(line -> StringBuilder.appendLine(line, sb), data#lines);
                    let s = StringBuilder.toString(sb);
                    let (ok, _, kind, msg) = %%FILE_WRITE%%((s, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                k(res)
            }

            def writeBytes(data, file, k) = {
                let res = region rc {
                    let bytes = Vector.toArray(rc, data);
                    let (ok, _, kind, msg) = %%FILE_WRITE_BYTES%%((bytes, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                k(res)
            }

            def append(data, file, k) = {
                let (ok, _, kind, msg) = %%FILE_APPEND%%((data#str, file));
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def appendLines(data, file, k) = {
                let res = region rc {
                    let sb = StringBuilder.empty(rc);
                    List.forEach(line -> StringBuilder.appendLine(line, sb), data#lines);
                    let s = StringBuilder.toString(sb);
                    let (ok, _, kind, msg) = %%FILE_APPEND%%((s, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                k(res)
            }

            def appendBytes(data, file, k) = {
                let res = region rc {
                    let bytes = Vector.toArray(rc, data);
                    let (ok, _, kind, msg) = %%FILE_APPEND_BYTES%%((bytes, file));
                    if (ok) Ok(()) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                k(res)
            }

            def truncate(file, k) = {
                let (ok, _, kind, msg) = %%FILE_TRUNCATE%%(file);
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def mkDir(d, k) = {
                let (ok, _, kind, msg) = %%FILE_MKDIR%%(d);
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def mkDirs(d, k) = {
                let (ok, _, kind, msg) = %%FILE_MKDIRS%%(d);
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def mkTempDir(prefix, k) = {
                let (ok, path, kind, msg) = %%FILE_MK_TEMP_DIR%%(prefix);
                if (ok) k(Ok(path)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

        }

    ///
    /// Runs the `FileWriteWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileWriteWithResult` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileWriteWithResult) + IO = handle(f)()

}
