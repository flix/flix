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

pub mod Fs.IsSymbolicLink {

    use Fs.FileTest

    ///
    /// An effect used to check if a path is a symbolic link.
    ///
    /// Use the `IsSymbolicLink.runWithFileTest` handler.
    ///
    pub eff IsSymbolicLink {

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsSymbolicLink` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - IsSymbolicLink) + FileTest =
        run { f() } with handler IsSymbolicLink {
            def isSymbolicLink(filename, k) = k(Fs.FileTest.isSymbolicLink(filename))
        }

}
