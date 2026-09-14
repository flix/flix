/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
