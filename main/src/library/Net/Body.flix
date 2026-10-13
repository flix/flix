/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of HTTP request and response bodies, and its functions.
///
pub mod Net.Body {

    ///
    /// Type of HTTP request and response bodies.
    ///
    pub enum Body with Eq, ToString {
        case Text(String)
        case Bytes(Vector[Int8])
    }

}
