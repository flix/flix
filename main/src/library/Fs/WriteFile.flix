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

pub mod Fs.WriteFile {

    import java.io.IOException
    import java.lang.UnsupportedOperationException
    import java.nio.charset.StandardCharsets
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.OpenOption
    import java.nio.file.Paths

    use Fs.FileWrite
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `WriteFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `WriteFile` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - WriteFile) + IO = x ->
        run {
            f(x)
        } with handler WriteFile {
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
        }

    ///
    /// Runs the `WriteFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `WriteFile` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - WriteFile) + IO = handle(f)()

    ///
    /// Handles the `WriteFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def handleWithFileWrite(f: a -> b \ ef): a -> b \ (ef - WriteFile) + FileWrite = x ->
        run { f(x) } with handler WriteFile {
            def write(data, file, k) = k(Fs.FileWrite.write(data, file))
        }

}
