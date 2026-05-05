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

pub mod Fs.FileSize {

    use Fs.FileStat
    use Fs.Size

    ///
    /// An effect used to read the size of a file.
    ///
    /// Use the `FileSize.runWithFileStat` handler.
    ///
    pub eff FileSize {

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

    }

    ///
    /// Handles the `FileSize` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def runWithFileStat(f: Unit -> a \ ef): a \ (ef - FileSize) + FileStat =
        run { f() } with handler FileSize {
            def size(filename, k) = k(Fs.FileStat.size(filename))
        }

}
