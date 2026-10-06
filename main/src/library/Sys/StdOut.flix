/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of process stdout streams, and its operations.
///
pub mod Sys.StdOut {

    import java.io.InputStream

    ///
    /// Type of process stdout streams.
    ///
    pub enum StdOut(InputStream)

    instance Readable[StdOut] {
        type Elm = Int8
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdOut): Result[IoError, Int32] \ r + IO =
            let StdOut.StdOut(pr) = r;
            Readable.read(b, pr)
    }

}
