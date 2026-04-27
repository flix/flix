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

pub mod Fs.IsExecutable {

    use Fs.FilePermission

    ///
    /// An effect used to check if a file is executable.
    ///
    /// Use the `IsExecutable.runWithFilePermission` handler.
    ///
    pub eff IsExecutable {

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsExecutable` effect of the given function `f` using the `FilePermission` effect.
    ///
    pub def runWithFilePermission(f: Unit -> a \ ef): a \ (ef - IsExecutable) + FilePermission =
        run { f() } with handler IsExecutable {
            def isExecutable(filename, k) = k(Fs.FilePermission.isExecutable(filename))
        }

}
