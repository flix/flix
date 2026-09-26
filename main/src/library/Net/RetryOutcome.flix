/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.RetryOutcome {

    use Net.HttpResponse

    ///
    /// Describes why a retry is being considered.
    ///
    pub enum RetryOutcome {
        /// A transport-level failure (connection refused, timeout, etc.).
        case TransportError(IoError)
        /// A response was received but its status code warrants a retry (e.g. 429, 503).
        case HttpError(HttpResponse)
    }

}
