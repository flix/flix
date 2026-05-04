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

pub mod Fs.WriteBytes {

    use Fs.FileWrite

    ///
    /// An effect used to write bytes to a file.
    ///
    /// Use the `WriteBytes.runWithFileWrite` handler.
    ///
    pub eff WriteBytes {

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `WriteBytes` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - WriteBytes) + FileWrite =
        run { f() } with handler WriteBytes {
            def writeBytes(data, file, k) = k(Fs.FileWrite.writeBytes(data, file))
        }

}
