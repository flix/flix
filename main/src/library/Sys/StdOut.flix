/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sys.StdOut {

    import java.io.InputStream

    ///
    /// A wrapper for the stdout stream of a process.
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
