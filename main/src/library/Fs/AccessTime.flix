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

pub mod Fs.AccessTime {

    use Fs.FileTime

    ///
    /// An effect used to read the last access time of a file.
    ///
    /// Use the `AccessTime.runWithFileTime` handler.
    ///
    pub eff AccessTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

    }

    ///
    /// Handles the `AccessTime` effect of the given function `f` using the `FileTime` effect.
    ///
    pub def runWithFileTime(f: Unit -> a \ ef): a \ (ef - AccessTime) + FileTime =
        run { f() } with handler AccessTime {
            def accessTime(filename, k) = k(Fs.FileTime.accessTime(filename))
        }

}
