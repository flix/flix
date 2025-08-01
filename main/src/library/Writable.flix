/*
 *  Copyright 2025 Ry Wiese
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
use IoError.ErrorKind
use IoError.IoError

import java.io.IOException
import java.io.OutputStream

///
/// A trait for types which are resources that can be written to.
///
trait Writable[t] {

    ///
    /// Type of each element that can be written to the resource.
    ///
    type Elm: Type

    ///
    /// Effect associated with writing to the underlying resource.
    ///
    type Aef: Eff

    ///
    /// Reads `k` items from `b` and writes them into the underlying resource.
    ///
    /// Returns `Ok(k)` to signify that `k` items were successfully read and written to `r`.
    ///
    /// Guarantees that `0 <= k <= length(b)`.
    ///
    /// Returns `Err(e)` if some underlying I/O error occurs.
    ///
    pub def write(buffer: Array[Writable.Elm[t], r], writer: t): Result[IoError, Int32] \ r + Writable.Aef[t]

}

instance Writable[OutputStream] {

    type Elm = Int8

    type Aef = IO

    pub def write(b: Array[Int8, r], r: OutputStream): Result[IoError, Int32] \ r + IO =
        try {
            r.write(b);
            checked_ecast(Ok(Array.length(b)))
        } catch {
            case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

}
