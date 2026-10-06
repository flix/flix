/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of decisions returned by a retry strategy, and its operations.
///
pub mod Net.RetryDecision {

    use Time.Duration

    ///
    /// Type of decisions returned by a retry strategy.
    ///
    pub enum RetryDecision with Eq, ToString {
        /// Retry after the given duration.
        case Retry(Duration)
        /// Do not retry; propagate the last result.
        case Stop
    }

}
