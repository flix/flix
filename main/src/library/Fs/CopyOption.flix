/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.CopyOption {

    ///
    /// Options for the `copyWith` operation.
    ///
    pub enum CopyOption with Eq, Order, ToString {
        case CopyAttributes
        case ReplaceExisting
    }

}
