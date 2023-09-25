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

///
/// A type class for types which are resources that can be accessed as bytes.
///
class Reader[t] {
    ///
    /// Reads `k` bytes from the underlying resource and writes them into `b`.
    ///
    /// Returns `Ok(k)` to signify that `k` bytes was successfully read and written to `b`.
    ///
    /// Guarantees that `0 <= k < length(b)`.
    ///
    /// Returns `Err(e)` if some underlying I/O error occurs.
    ///
    pub def read(b: Array[Int8, r], r: t): Result[IOError, Int32] \ {r, IO}
}
