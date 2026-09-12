/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.Body {

    ///
    /// An enum representing the body of an HTTP request or response.
    ///
    pub enum Body with Eq, ToString {
        case Text(String)
        case Bytes(Vector[Int8])
    }

}
