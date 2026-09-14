/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.RetryDecision {

    use Time.Duration

    ///
    /// The decision returned by a retry handler.
    ///
    pub enum RetryDecision with Eq, ToString {
        /// Retry after the given duration.
        case Retry(Duration)
        /// Do not retry; propagate the last result.
        case Stop
    }

}
