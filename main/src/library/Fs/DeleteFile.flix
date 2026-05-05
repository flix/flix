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

pub mod Fs.DeleteFile {

    use Fs.FileWrite

    ///
    /// An effect used to delete a file.
    ///
    /// Use the `DeleteFile.runWithFileWrite` handler.
    ///
    pub eff DeleteFile {

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `DeleteFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - DeleteFile) + FileWrite =
        run { f() } with handler DeleteFile {
            def delete(file, k) = k(Fs.FileWrite.delete(file))
        }

}
