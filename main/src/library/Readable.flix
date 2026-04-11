/*
 * Copyright 2023 Magnus Madsen
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
use IoError.IoError

///
/// A trait for types which are resources that can be read from.
///
trait Readable[t] {

    ///
    /// Type of each element that can be read from the resource.
    ///
    type Elm: Type

    ///
    /// Effect associated with reading from the resource.
    ///
    type Aef: Eff

    ///
    /// Reads `k` elements from the resource and writes them into `b`.
    ///
    /// Returns `Ok(k)` to signify that `k` bytes were successfully read and written to `b`.
    ///
    /// Guarantees that `0 <= k <= length(b)`.
    ///
    /// Returns `Err(e)` if some underlying I/O error occurs.
    ///
    pub def read(buffer: Array[Readable.Elm[t], r], reader: t): Result[IoError, Int32] \ r + Readable.Aef[t]

}
