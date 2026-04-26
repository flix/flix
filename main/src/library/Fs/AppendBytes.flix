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

pub mod Fs.AppendBytes {

    use Fs.FileWrite

    ///
    /// An effect used to append bytes to a file.
    ///
    /// Use the `AppendBytes.runWithFileWrite` handler.
    ///
    pub eff AppendBytes {

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `AppendBytes` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - AppendBytes) + FileWrite =
        run { f() } with handler AppendBytes {
            def appendBytes(data, file, k) = k(Fs.FileWrite.appendBytes(data, file))
        }

}
