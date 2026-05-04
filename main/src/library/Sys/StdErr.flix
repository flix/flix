/*
 *  Copyright 2026 Flix Authors
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

pub mod Sys.StdErr {

    import java.io.InputStream

    ///
    /// A wrapper for the stderr stream of a process.
    ///
    pub enum StdErr(InputStream)

    instance Readable[StdErr] {
        type Elm = Int8
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdErr): Result[IoError, Int32] \ r + IO =
            let StdErr.StdErr(pr) = r;
            Readable.read(b, pr)
    }

}
