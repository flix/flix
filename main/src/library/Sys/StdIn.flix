/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sys.StdIn {

    import java.io.OutputStream

    ///
    /// A wrapper for the stdin stream of a process.
    ///
    pub enum StdIn(OutputStream)

    instance Writable[StdIn] {
        type Elm = Int8
        type Aef = IO
        pub def write(b: Array[Int8, r], r: StdIn): Result[IoError, Int32] \ r + IO =
            let StdIn.StdIn(pr) = r;
            Writable.write(b, pr)
    }

}
