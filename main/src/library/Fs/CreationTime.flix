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

pub mod Fs.CreationTime {

    import java.io.IOException
    import java.lang.Class
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.attribute.BasicFileAttributes

    use Fs.FileTime
    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `CreationTime` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `CreationTime` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - CreationTime) + IO = x ->
        run {
            f(x)
        } with handler CreationTime {
            def creationTime(filename, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(filename)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `CreationTime` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `CreationTime` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - CreationTime) + IO = handle(f)()

    ///
    /// Handles the `CreationTime` effect of the given function `f` using the `FileTime` effect.
    ///
    pub def handleWithFileTime(f: a -> b \ ef): a -> b \ (ef - CreationTime) + FileTime = x ->
        run { f(x) } with handler CreationTime {
            def creationTime(filename, k) = k(Fs.FileTime.creationTime(filename))
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
