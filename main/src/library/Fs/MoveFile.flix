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

pub mod Fs.MoveFile {

    use Fs.FileWrite
    use Fs.MoveFile

    ///
    /// Convenience function: moves `src` to `dst` with no options.
    ///
    pub def move(src: {src = String}, dst: String): Result[IoError, Unit] \ MoveFile =
        Fs.MoveFile.moveWith(src, dst, Set.empty())

    ///
    /// Handles the `MoveFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `MoveFile` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - MoveFile) + IO = x ->
        run { f(x) } with handler MoveFile {
            def moveWith(src, dst, opts, k) = k(Fs.FsLayer.moveWith(src, dst, opts))
        }

    ///
    /// Runs the `MoveFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `MoveFile` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - MoveFile) + IO = handle(f)()

    ///
    /// Handles the `MoveFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def handleWithFileWrite(f: a -> b \ ef): a -> b \ (ef - MoveFile) + FileWrite = x ->
        run { f(x) } with handler MoveFile {
            def moveWith(src, dst, opts, k) = k(Fs.FileWrite.moveWith(src, dst, opts))
        }

}
