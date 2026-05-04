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

pub mod Fs.IsRegularFile {

    use Fs.FileTest

    ///
    /// An effect used to check if a path is a regular file.
    ///
    /// Use the `IsRegularFile.runWithFileTest` handler.
    ///
    pub eff IsRegularFile {

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsRegularFile` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - IsRegularFile) + FileTest =
        run { f() } with handler IsRegularFile {
            def isRegularFile(filename, k) = k(Fs.FileTest.isRegularFile(filename))
        }

}
