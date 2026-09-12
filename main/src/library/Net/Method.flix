/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.Method {

    ///
    /// An enum representing an HTTP method.
    ///
    pub enum Method with Eq, ToString {
        case Get
        case Head
        case Options
        case Post
        case Put
        case Patch
        case Delete
    }

}
