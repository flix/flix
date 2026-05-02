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

pub mod Fs.FileExists {

    use Fs.FileTest

    ///
    /// An effect used to check if a file exists.
    ///
    /// Use the `FileExists.runWithFileTest` handler.
    ///
    pub eff FileExists {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `FileExists` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - FileExists) + FileTest =
        run { f() } with handler FileExists {
            def exists(filename, k) = k(Fs.FileTest.exists(filename))
        }

}
