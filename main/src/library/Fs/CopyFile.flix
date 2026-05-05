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

pub mod Fs.CopyFile {

    use Fs.FileWrite
    use Fs.CopyFile
    use Fs.CopyOption

    ///
    /// An effect used to copy a file.
    ///
    /// Use the `CopyFile.runWithFileWrite` handler.
    ///
    pub eff CopyFile {

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

    }

    ///
    /// Convenience function: copies `src` to `dst` with no options.
    ///
    pub def copy(src: {src = String}, dst: String): Result[IoError, Unit] \ CopyFile =
        Fs.CopyFile.copyWith(src, dst, Set.empty())

    ///
    /// Handles the `CopyFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - CopyFile) + FileWrite =
        run { f() } with handler CopyFile {
            def copyWith(src, dst, opts, k) = k(Fs.FileWrite.copyWith(src, dst, opts))
        }

}
