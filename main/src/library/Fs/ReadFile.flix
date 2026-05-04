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

pub mod Fs.ReadFile {

    use Fs.FileRead

    ///
    /// An effect used to read the contents of a file as a string.
    ///
    /// Use the `ReadFile.runWithFileRead` handler.
    ///
    pub eff ReadFile {

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

    }

    ///
    /// Handles the `ReadFile` effect of the given function `f` using the `FileRead` effect.
    ///
    pub def runWithFileRead(f: Unit -> a \ ef): a \ (ef - ReadFile) + FileRead =
        run { f() } with handler ReadFile {
            def read(filename, k) = k(Fs.FileRead.read(filename))
        }

}
