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

pub mod Fs.MkDir {

    use Fs.FileWrite

    ///
    /// An effect used to create a directory.
    ///
    /// Use the `MkDir.runWithFileWrite` handler.
    ///
    pub eff MkDir {

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `MkDir` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MkDir) + FileWrite =
        run { f() } with handler MkDir {
            def mkDir(d, k) = k(Fs.FileWrite.mkDir(d))
        }

}
