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

pub mod Fs.MkTempDir {

    use Fs.FileWrite

    ///
    /// An effect used to create a temporary directory.
    ///
    /// Use the `MkTempDir.runWithFileWrite` handler.
    ///
    pub eff MkTempDir {

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

    ///
    /// Handles the `MkTempDir` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MkTempDir) + FileWrite =
        run { f() } with handler MkTempDir {
            def mkTempDir(prefix, k) = k(Fs.FileWrite.mkTempDir(prefix))
        }

}
