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

pub mod Fs.WriteLines {

    import java.io.IOException
    import java.lang.UnsupportedOperationException
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.OpenOption
    import java.nio.file.Paths

    use Fs.FileWrite
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `WriteLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `WriteLines` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - WriteLines) + IO = x ->
        run {
            f(x)
        } with handler WriteLines {
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
        }

    ///
    /// Runs the `WriteLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `WriteLines` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - WriteLines) + IO = handle(f)()

    ///
    /// Handles the `WriteLines` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def handleWithFileWrite(f: a -> b \ ef): a -> b \ (ef - WriteLines) + FileWrite = x ->
        run { f(x) } with handler WriteLines {
            def writeLines(data, file, k) = k(Fs.FileWrite.writeLines(data, file))
        }

}
