/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

mod Readable {

    use IoError.ErrorKind
    use IoError.IoError

    import java.io.InputStream
    import java.io.IOException

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

    instance Readable[InputStream] {

        type Elm = Int8

        type Aef = IO

        pub def read(b: Array[Int8, r], r: InputStream): Result[IoError, Int32] \ r + IO =
            try {
                let result = r.read(b);
                let numRead = if (result == -1) 0 else result;
                checked_ecast(Ok(numRead))
            } catch {
                case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
            }

    }

}
